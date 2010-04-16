/* Peephole optimizations for wordcode compiler. */

#include "Python.h"

#include "Python-ast.h"
#include "node.h"
#include "pyarena.h"
#include "ast.h"
#include "code.h"
#include "compile.h"
#include "symtable.h"
#include "opcode.h"
#include "wpython.h"

#define MAXCODELEN (32768 - 64)

#if MAXCODELEN <= 32767
#define BLOCKS_ADDRMAP_TYPE short
#else
#define BLOCKS_ADDRMAP_TYPE int
#endif

#ifdef WORDS_BIGENDIAN
#define GETWORD(arr, value) (value = *(arr); \
                             value = (value & 0xff) << 8 | (value >> 8))
#define NEXTARG16(arr, oparg) oparg = *(arr)++; \
                              oparg = oparg >> 8 | (oparg & 0xff) << 8
#define PACKOPCODE(op, arg) (((op) << 8) + (arg))
#else
#define GETWORD(arr, value) (value = *(arr))
#define NEXTARG16(arr, oparg) oparg = *(arr)++
#define PACKOPCODE(op, arg) (((arg) << 8) + (op))
#endif

#define NEXT_RAW_WORD(arr, value) (value = *(arr)++)
#define ABSOLUTE_JUMP8(op) (op == JUMP_ABSOLUTE || \
						   op == CONTINUE_LOOP || \
						   op == LIST_APPEND_LOOP)
#define UNCONDITIONAL_JUMP8(op)	(op == JUMP_ABSOLUTE || \
								 op == JUMP_FORWARD)
#define GETJUMPTARGET8(opcode, oparg, start) \
		(oparg + (ABSOLUTE_JUMP8(opcode) ? 0 : (start)))
#define ABSOLUTE_JUMP16(op) (op == EXT16(JUMP_ABSOLUTE) || \
						   op == EXT16(CONTINUE_LOOP) || \
						   op == EXT16(LIST_APPEND_LOOP))
#define UNCONDITIONAL_JUMP16(op) (op == EXT16(JUMP_ABSOLUTE) || \
								 op == EXT16(JUMP_FORWARD))
#define GETJUMPTARGET16(opcode, oparg, start) \
		(oparg + (ABSOLUTE_JUMP16(opcode) ? 0 : (start)))
#define CODESIZE(op)  (op >= EXTENDED_ARG32 ? 3 : \
					   (op >= EXTENDED_ARG16 ? 2 : 1))
#define ISBASICBLOCK(start, words) (blocks[start] == blocks[start + words - 1])


static int
markblocks(unsigned short *code, Py_ssize_t len, BLOCKS_ADDRMAP_TYPE *blocks)
{
	int i, rawopcode, opcode, oparg;
	unsigned short *code_start = code, *code_end = code + len;
    BLOCKS_ADDRMAP_TYPE blockcnt;

	memset(blocks, 0, len * sizeof(BLOCKS_ADDRMAP_TYPE));

	/* Mark labels in the first pass */
	while (code < code_end) {
		NEXT_RAW_WORD(code, rawopcode);
		switch (rawopcode) {
			case EXT16(JUMP_IF_FALSE_ELSE_POP):
			case EXT16(JUMP_IF_TRUE_ELSE_POP):
			case EXT16(JUMP_IF_FALSE):
			case EXT16(JUMP_IF_TRUE):
			case EXT16(JUMP_FORWARD):
			case EXT16(JUMP_ABSOLUTE):
			case EXT16(SETUP_LOOP):
			case EXT16(SETUP_EXCEPT):
			case EXT16(SETUP_FINALLY):
			case EXT16(CONTINUE_LOOP):
			case EXT16(FOR_ITER):
			case EXT16(LIST_APPEND_LOOP):
				NEXTARG16(code, oparg);
				i = code - code_start;
				oparg = GETJUMPTARGET16(rawopcode, oparg, i);
				blocks[oparg] = 1;
				break;
			default:
				opcode = EXTRACTOP(rawopcode);
				switch (opcode) {
					case JUMP_IF_FALSE_ELSE_POP:
					case JUMP_IF_TRUE_ELSE_POP:
					case JUMP_IF_FALSE:
					case JUMP_IF_TRUE:
					case JUMP_FORWARD:
					case JUMP_ABSOLUTE:
					case SETUP_LOOP:
					case SETUP_EXCEPT:
					case SETUP_FINALLY:
					case CONTINUE_LOOP:
					case FOR_ITER:
					case LIST_APPEND_LOOP:
						i = code - code_start;
						oparg = EXTRACTARG(rawopcode);
						oparg = GETJUMPTARGET8(opcode, oparg, i);
						blocks[oparg] = 1;
						break;
					default:
						if (opcode == EXTENDED_ARG32)
							return -1;
						if (opcode > EXTENDED_ARG32)
							code += 2;
						else if (opcode >= EXTENDED_ARG16)
							code++;
						break;
				}
		}
	}
	/* Build block numbers in the second pass */
	for (blockcnt = i = 0 ; i<len ; i++) {
		blockcnt += blocks[i];	/* increment blockcnt over labels */
		blocks[i] = blockcnt;
	}
	return 0;
}

#ifdef WPY_UNARY_NOT_JUMP_IF
/* Replace UNARY_NOT JUMP_IF_FALSE
   with	   NOP JUMP_IF_TRUE, and
   UNARY_NOT JUMP_IF_FALSE_ELSE_POP
   with	   NOP JUMP_IF_TRUE_ELSE_POP */
static void
handle_unary_not(unsigned short *codestr, Py_ssize_t codelen,
	BLOCKS_ADDRMAP_TYPE *blocks, Py_ssize_t i)
{
	int rawopcode = codestr[i + 1];
	int opcode = EXTRACTOP(rawopcode);
	int oparg = EXTRACTARG(rawopcode);
	if ((rawopcode == EXT16(JUMP_IF_FALSE) ||
		rawopcode == EXT16(JUMP_IF_TRUE)) &&
		ISBASICBLOCK(i, 3)) {
		opcode = oparg ^ 1;
		rawopcode = EXT16(opcode);
	}
	else if ((opcode == JUMP_IF_FALSE ||
		(opcode == JUMP_IF_TRUE)) &&
		ISBASICBLOCK(i, 2)) {
		opcode ^= 1;
		rawopcode = PACKOPCODE(opcode, oparg);
	}
	else
		return;
	codestr[i] = CONVERT(NOP);
	codestr[i + 1] = rawopcode;
}
#endif

#ifdef WPY_REMOVE_UNREACHABLE_CODE
/* Remove unreachable code after unconditional flow change instructions.
   Return the pointer to the next instruction that has not been removed */
static unsigned short *
remove_unreachable_code(unsigned short *codestr, Py_ssize_t codelen,
	BLOCKS_ADDRMAP_TYPE *blocks, Py_ssize_t i)
{
	int opcode;
	/* i + 1 is the instruction following RETURN */
	Py_ssize_t j = i + 1;
	/* Starts with 1 word for RETURN, and 1 for the next instruction */
	Py_ssize_t n = 2;
	Py_ssize_t size;

	while (j < codelen && ISBASICBLOCK(i, n)) {
		opcode = EXTRACTOP(codestr[j]);
		size = CODESIZE(opcode);
		j += size;
		n += size;
	}
	n -= 2;
	codestr += i + 1;
	while (n--)
		*codestr++ = CONVERT(NOP);
	return codestr;
}
#endif

/* Skip over LOAD_CONST trueconst JUMP_IF_FALSE xx
   Skip over LOAD_CONST trueconst JUMP_IF_FALSE_ELSE_POP xx */
static void
handle_load_const(unsigned short *codestr, Py_ssize_t codelen,
	BLOCKS_ADDRMAP_TYPE *blocks, PyObject* consts,
	Py_ssize_t i, int oparg, int long_const)
{
	int opcode;
	int rawopcode = codestr[i + long_const + 1];
	if (PyObject_IsTrue(PyTuple_GET_ITEM(consts, oparg))) {
		opcode = EXTRACTOP(rawopcode);
		if (rawopcode == EXT16(JUMP_IF_FALSE) &&
			ISBASICBLOCK(i, 3 + long_const))
			oparg = 3 + long_const;
		else if (rawopcode == EXT16(JUMP_IF_FALSE_ELSE_POP) &&
			ISBASICBLOCK(i, 3 + long_const))
			oparg = 3 + long_const;
		else if (opcode == JUMP_IF_FALSE &&
			ISBASICBLOCK(i, 2 + long_const))
			oparg = 2 + long_const;
		else if (opcode == JUMP_IF_FALSE_ELSE_POP &&
			ISBASICBLOCK(i, 2 + long_const))
			oparg = 2 + long_const;
		else
			return;
		codestr += i;
		while (oparg--)
			*codestr++ = CONVERT(NOP);
	}
}

/* Checks for superinstructions which starts with the LOAD_FAST opcode. */

static void
load_fast_superinstructions(unsigned short *codestr,
    BLOCKS_ADDRMAP_TYPE *blocks, Py_ssize_t i)
{
	int opcode;
    int oparg = EXTRACTARG(codestr[i]);
	int op2 = EXTRACTOP(codestr[i + 1]);

	if (op2 == STORE_FAST && ISBASICBLOCK(i, 2)) {
#ifdef WPY_MOVE_FAST_FAST
		codestr[i] = PACKOPCODE(MOVE_FAST_FAST, oparg);
		opcode = EXTRACTARG(codestr[i + 1]);
		codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
	}
	else if (op2 == STORE_ATTR && ISBASICBLOCK(i, 2)) {
#ifdef WPY_STORE_FAST_ATTR
		codestr[i] = PACKOPCODE(STORE_FAST_ATTR, oparg);
		opcode = EXTRACTARG(codestr[i + 1]);
		codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
	}
    else if (op2 == LOAD_ATTR && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_FAST && ISBASICBLOCK(i, 3)) {
#ifdef WPY_MOVE_FAST_ATTR_FAST
			codestr[i] = PACKOPCODE(MOVE_FAST_ATTR_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else if (EXTRACTOP(codestr[i + 2]) == LOAD_FAST &&
			oparg == EXTRACTARG(codestr[i + 2]) &&
			EXTRACTOP(codestr[i + 3]) == STORE_ATTR && ISBASICBLOCK(i, 4)) {
#ifdef WPY_MOVE_FAST_ATTR_FAST_ATTR
			codestr[i] = PACKOPCODE(MOVE_FAST_ATTR_FAST_ATTR, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 3]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
			codestr[i + 3] = CONVERT(NOP);
#endif
		}
		else if (EXTRACTOP(codestr[i + 2]) == QUICK_CALL_FUNCTION &&
			EXTRACTARG(codestr[i + 2]) == 0 && ISBASICBLOCK(i, 3)) {
			if (codestr[i + 3] == CONVERT(POP_TOP) && ISBASICBLOCK(i, 4)) {
#ifdef WPY_FAST_ATTR_CALL_PROC
				codestr[i] = PACKOPCODE(FAST_ATTR_CALL_PROC, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				codestr[i + 1] = PACKOPCODE(opcode, 0);
				codestr[i + 2] = CONVERT(NOP);
				codestr[i + 3] = CONVERT(NOP);
#endif
			}
			else {
#ifdef WPY_FAST_ATTR_CALL_FUNC
				codestr[i] = PACKOPCODE(FAST_ATTR_CALL_FUNC, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				codestr[i + 1] = PACKOPCODE(opcode, 0);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
		else {
#ifdef WPY_LOAD_FAST_ATTR
			codestr[i] = PACKOPCODE(LOAD_FAST_ATTR, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
		}
    }
	else if (op2 == LOAD_FAST && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_ATTR && ISBASICBLOCK(i, 3)) {
#ifdef WPY_MOVE_FAST_FAST_ATTR
			codestr[i] = PACKOPCODE(MOVE_FAST_FAST_ATTR, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else if (codestr[i + 2] == CONVERT(BINARY_ADD) && ISBASICBLOCK(i, 3)) {
			if (EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
				ISBASICBLOCK(i, 4)) {
#ifdef WPY_FAST_ADD_FAST_TO_FAST
				codestr[i] = PACKOPCODE(FAST_ADD_FAST_TO_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 3]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
				codestr[i + 3] = CONVERT(NOP);
#endif
			}
			else {
#ifdef WPY_FAST_ADD_FAST
				codestr[i] = PACKOPCODE(FAST_ADD_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				codestr[i + 1] = PACKOPCODE(opcode, 0);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
		else if (codestr[i + 2] == CONVERT(INPLACE_ADD) &&
			EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
			oparg == EXTRACTARG(codestr[i + 3]) && ISBASICBLOCK(i, 4)) {
#ifdef WPY_FAST_INPLACE_ADD_FAST
			codestr[i] = PACKOPCODE(FAST_INPLACE_ADD_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
			codestr[i + 2] = CONVERT(NOP);
			codestr[i + 3] = CONVERT(NOP);
#endif
		}
		else if (EXTRACTOP(codestr[i + 2]) == BINARY_OPS &&
            ISBASICBLOCK(i, 3)) {
			if (EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
				ISBASICBLOCK(i, 4)) {
				if (oparg == EXTRACTARG(codestr[i + 3])) {
#ifdef WPY_FAST_INPLACE_BINOP_FAST
					codestr[i] = PACKOPCODE(FAST_INPLACE_BINOP_FAST, oparg);
					opcode = EXTRACTARG(codestr[i + 1]);
					oparg = EXTRACTARG(codestr[i + 2]);
					codestr[i + 1] = PACKOPCODE(opcode, oparg);
					codestr[i + 2] = CONVERT(NOP);
					codestr[i + 3] = CONVERT(NOP);
#endif
				}
				else if (EXTRACTARG(codestr[i + 2]) <= (BINARY_OR >> 8)) {
#ifdef WPY_FAST_QUICKOP_FAST_TO_FAST
					opcode = FAST_POW_FAST_TO_FAST +
							 EXTRACTARG(codestr[i + 2]);
					codestr[i] = PACKOPCODE(opcode, oparg);
					opcode = EXTRACTARG(codestr[i + 1]);
					oparg = EXTRACTARG(codestr[i + 3]);
					codestr[i + 1] = PACKOPCODE(opcode, oparg);
					codestr[i + 2] = CONVERT(NOP);
					codestr[i + 3] = CONVERT(NOP);
#endif
				}
			}
			else {
#ifdef WPY_FAST_BINOP_FAST
				codestr[i] = PACKOPCODE(FAST_BINOP_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 2]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
	}
	else if (op2 == LOAD_CONST && ISBASICBLOCK(i, 2)) {
		if (codestr[i + 2] == CONVERT(BINARY_ADD) && ISBASICBLOCK(i, 3)) {
			if (EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
				ISBASICBLOCK(i, 4)) {
#ifdef WPY_FAST_ADD_CONST_TO_FAST
				codestr[i] = PACKOPCODE(FAST_ADD_CONST_TO_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 3]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
				codestr[i + 3] = CONVERT(NOP);
#endif
			}
			else {
#ifdef WPY_FAST_ADD_CONST
				codestr[i] = PACKOPCODE(FAST_ADD_CONST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				codestr[i + 1] = PACKOPCODE(opcode, 0);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
		else if (codestr[i + 2] == CONVERT(INPLACE_ADD) &&
			EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
			oparg == EXTRACTARG(codestr[i + 3]) && ISBASICBLOCK(i, 4)) {
#ifdef WPY_FAST_INPLACE_ADD_CONST
			codestr[i] = PACKOPCODE(FAST_INPLACE_ADD_CONST, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
			codestr[i + 2] = CONVERT(NOP);
			codestr[i + 3] = CONVERT(NOP);
#endif
		}
		else if (EXTRACTOP(codestr[i + 2]) == BINARY_OPS &&
            ISBASICBLOCK(i, 3)) {
			if (EXTRACTARG(codestr[i + 2]) <= (BINARY_OR >> 8) &&
				EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
				ISBASICBLOCK(i, 4)) {
#ifdef WPY_FAST_QUICKOP_CONST_TO_FAST
				opcode = FAST_POW_CONST_TO_FAST + EXTRACTARG(codestr[i + 2]);
				codestr[i] = PACKOPCODE(opcode, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 3]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
				codestr[i + 3] = CONVERT(NOP);
#endif
			}
			else {
#ifdef WPY_FAST_BINOP_CONST
				codestr[i] = PACKOPCODE(FAST_BINOP_CONST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 2]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
	}
	else if (codestr[i + 1] == CONVERT(BINARY_ADD) && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_FAST && ISBASICBLOCK(i, 3)) {
#ifdef WPY_FAST_ADD_TO_FAST
			codestr[i] = PACKOPCODE(FAST_ADD_TO_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else {
#ifdef WPY_FAST_ADD
			codestr[i] = PACKOPCODE(FAST_ADD, oparg);
			codestr[i + 1] = CONVERT(NOP);
#endif
		}
    }
	else if (op2 == UNARY_OPS && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_FAST && ISBASICBLOCK(i, 3)) {
#ifdef WPY_FAST_UNOP_TO_FAST
			codestr[i] = PACKOPCODE(FAST_UNOP_TO_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else {
#ifdef WPY_FAST_UNOP
			codestr[i] = PACKOPCODE(FAST_UNOP, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
		}
    }
    else if (op2 == BINARY_OPS && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_FAST && ISBASICBLOCK(i, 3)) {
#ifdef WPY_FAST_BINOP_TO_FAST
			codestr[i] = PACKOPCODE(FAST_BINOP_TO_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else {
#ifdef WPY_FAST_BINOP
			codestr[i] = PACKOPCODE(FAST_BINOP, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
		}
    }
}

/* Checks for superinstructions which starts with the LOAD_CONST opcode. */

static void
load_const_superinstructions(unsigned short *codestr,
    BLOCKS_ADDRMAP_TYPE *blocks, Py_ssize_t i)
{
	int opcode;
    int oparg = EXTRACTARG(codestr[i]);
    int op2 = EXTRACTOP(codestr[i + 1]);

	if (op2 == STORE_FAST && ISBASICBLOCK(i, 2)) {
#ifdef WPY_MOVE_CONST_FAST
		codestr[i] = PACKOPCODE(MOVE_CONST_FAST, EXTRACTARG(codestr[i]));
		opcode = EXTRACTARG(codestr[i + 1]);
		codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
	}
	else if (op2 == LOAD_FAST && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_ATTR && ISBASICBLOCK(i, 3)) {
#ifdef WPY_MOVE_CONST_FAST_ATTR
			codestr[i] = PACKOPCODE(MOVE_CONST_FAST_ATTR, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
        else if (codestr[i + 2] == CONVERT(BINARY_ADD) && ISBASICBLOCK(i, 3)) {
			if (EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
				ISBASICBLOCK(i, 4)) {
#ifdef WPY_CONST_ADD_FAST_TO_FAST
				codestr[i] = PACKOPCODE(CONST_ADD_FAST_TO_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 3]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
				codestr[i + 3] = CONVERT(NOP);
#endif
			}
			else {
#ifdef WPY_CONST_ADD_FAST
				codestr[i] = PACKOPCODE(CONST_ADD_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				codestr[i + 1] = PACKOPCODE(opcode, 0);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
        else if (EXTRACTOP(codestr[i + 2]) == BINARY_OPS &&
            ISBASICBLOCK(i, 3)) {
			if (EXTRACTARG(codestr[i + 2]) <= (BINARY_OR >> 8) &&
				EXTRACTOP(codestr[i + 3]) == STORE_FAST &&
				ISBASICBLOCK(i, 4)) {
#ifdef WPY_CONST_QUICKOP_FAST_TO_FAST
				opcode = CONST_POW_FAST_TO_FAST +
						 EXTRACTARG(codestr[i + 2]);
				codestr[i] = PACKOPCODE(opcode, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 3]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
				codestr[i + 3] = CONVERT(NOP);
#endif
			}
			else {
#ifdef WPY_CONST_BINOP_FAST
				codestr[i] = PACKOPCODE(CONST_BINOP_FAST, oparg);
				opcode = EXTRACTARG(codestr[i + 1]);
				oparg = EXTRACTARG(codestr[i + 2]);
				codestr[i + 1] = PACKOPCODE(opcode, oparg);
				codestr[i + 2] = CONVERT(NOP);
#endif
			}
        }
	}
    else if (codestr[i + 1] == CONVERT(BINARY_ADD) && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_FAST && ISBASICBLOCK(i, 3)) {
#ifdef WPY_CONST_ADD_TO_FAST
			codestr[i] = PACKOPCODE(CONST_ADD_TO_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else {
#ifdef WPY_CONST_ADD
            codestr[i] = PACKOPCODE(CONST_ADD, oparg);
			codestr[i + 1] = CONVERT(NOP);
#endif
		}
    }
    else if (op2 == BINARY_OPS && ISBASICBLOCK(i, 2)) {
		if (EXTRACTOP(codestr[i + 2]) == STORE_FAST && ISBASICBLOCK(i, 3)) {
#ifdef WPY_CONST_BINOP_TO_FAST
			codestr[i] = PACKOPCODE(CONST_BINOP_TO_FAST, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			oparg = EXTRACTARG(codestr[i + 2]);
			codestr[i + 1] = PACKOPCODE(opcode, oparg);
			codestr[i + 2] = CONVERT(NOP);
#endif
		}
		else {
#ifdef WPY_CONST_BINOP
			codestr[i] = PACKOPCODE(CONST_BINOP, oparg);
			opcode = EXTRACTARG(codestr[i + 1]);
			codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
		}
    }
}

/* Checks for superinstructions which starts with the LOAD_GLOBAL opcode. */

static void
load_global_superinstructions(unsigned short *codestr,
    BLOCKS_ADDRMAP_TYPE *blocks, Py_ssize_t i)
{
	int opcode;
    int oparg = EXTRACTARG(codestr[i]);

	if (EXTRACTOP(codestr[i + 1]) == STORE_FAST && ISBASICBLOCK(i, 2)) {
#ifdef WPY_MOVE_GLOBAL_FAST
		codestr[i] = PACKOPCODE(MOVE_GLOBAL_FAST, oparg);
		opcode = EXTRACTARG(codestr[i + 1]);
		codestr[i + 1] = PACKOPCODE(opcode, 0);
#endif
	}
#ifdef WPY_LOAD_GLOBAL_ATTR
	else if (EXTRACTOP(codestr[i + 1]) == LOAD_ATTR && ISBASICBLOCK(i, 2)) {
		codestr[i] = PACKOPCODE(LOAD_GLOBAL_ATTR, oparg);
		opcode = EXTRACTARG(codestr[i + 1]);
		codestr[i + 1] = PACKOPCODE(opcode, 0);
	}
#endif
#ifdef WPY_LOAD_GLOB_FAST_CALL_FUNC
	else if (EXTRACTOP(codestr[i + 1]) == LOAD_FAST &&
		EXTRACTOP(codestr[i + 2]) == QUICK_CALL_FUNCTION && ISBASICBLOCK(i, 3)) {
		codestr[i] = PACKOPCODE(LOAD_GLOB_FAST_CALL_FUNC, oparg);
		opcode = EXTRACTARG(codestr[i + 1]);
		oparg = EXTRACTARG(codestr[i + 2]);
		codestr[i + 1] = PACKOPCODE(opcode, oparg);
		codestr[i + 2] = CONVERT(NOP);
	}
#endif
}

/* Perform basic peephole optimizations to components of a code object.
   The consts object should still be in tuple form, not list, since
   constant folding now is performed on the AST layer, so now there's
   no need do append new constants.

   To keep the optimizer simple, it bails out (does nothing) for code
   containing extended arguments or that has a length over 32,700.  That 
   allows us to avoid overflow and sign issues.	 Likewise, it bails when
   the lineno table has complex encoding for gaps >= 255.

   Optimizations are restricted to simple transformations occuring within a
   single basic block.	All transformations keep the code size the same or 
   smaller.  For those that reduce size, the gaps are initially filled with 
   NOPs.  Later those NOPs are removed and the jump addresses retargeted in 
   a single pass.  Line numbering is adjusted accordingly. */

PyObject *
PyCode_Optimize(PyObject *code, PyObject* consts, PyObject *names,
                PyObject *lineno_obj)
{
#ifdef WPY_STATIC_BUFFER_ALLOCATION
    unsigned short codestr[MAXCODELEN], *source, *target, *code_end;
	/* Declare a scratchpad array that will be used both for addrmap and blocks.
	   That's because addrmap and blocks use of this area is
	   mutually exclusive. */
	BLOCKS_ADDRMAP_TYPE scratchpad[MAXCODELEN];
#else
	unsigned short *codestr = NULL, *source, *target, *code_end;
	BLOCKS_ADDRMAP_TYPE *scratchpad = NULL;
#endif
	/* Mapping to new jump targets after NOPs are removed. */
	BLOCKS_ADDRMAP_TYPE *addrmap, *blocks;
	Py_ssize_t i, codelen;
	int tgt, tgttgt;
	int rawopcode, opcode, oparg;
	unsigned char *lineno;
    int tabsiz;
	BLOCKS_ADDRMAP_TYPE nops, new_line, cum_orig_line, last_line;

	assert(PyTuple_Check(consts));

	/* Bail out if an exception is set */
	if (PyErr_Occurred())
		goto exitUnchanged;

	/* Bypass optimization when the lineno table is too complex */
	assert(PyString_Check(lineno_obj));
	lineno = (unsigned char*)PyString_AS_STRING(lineno_obj);
	tabsiz = PyString_GET_SIZE(lineno_obj);
	if (memchr(lineno, 255, tabsiz) != NULL)
		goto exitUnchanged;

	assert(PyString_Check(code));
	/* Avoid situations where jump retargeting could overflow */
	codelen = PyString_GET_SIZE(code) >> 1;
	if (codelen > MAXCODELEN)
		goto exitUnchanged;

	/* Verify that RETURN_VALUE terminates the codestring.	This allows
	   the various transformation patterns to look ahead several
	   instructions without additional checks to make sure they are not
	   looking beyond the end of the code string.
	*/
	GETWORD((unsigned short *) PyString_AS_STRING(code) + codelen - 1,
        rawopcode);
	if (rawopcode != RETURN_VALUE && EXTRACTOP(rawopcode) != RETURN_CONST &&
        rawopcode != RETURN_LOCALS)
		goto exitUnchanged;

#ifndef WPY_STATIC_BUFFER_ALLOCATION
	scratchpad = (BLOCKS_ADDRMAP_TYPE *)
                 PyMem_Malloc(codelen * sizeof(BLOCKS_ADDRMAP_TYPE));
	if (scratchpad == NULL)
		goto exitUnchanged;
#endif

	/* Mapping to new jump targets after NOPs are removed */
	addrmap = blocks = scratchpad;

	/* Mark jump blocks and verify that no EXTENDED_ARG32 was found */
	if (markblocks((unsigned short *) PyString_AS_STRING(code), codelen, blocks) < 0)
		goto exitUnchanged;

#ifndef WPY_STATIC_BUFFER_ALLOCATION
	codestr = (unsigned short *) PyMem_Malloc(codelen << 1);
	if (codestr == NULL)
		goto exitUnchanged;
#endif
	/* Make a modifiable copy of the code string */
	memcpy(codestr, PyString_AS_STRING(code), codelen << 1);

	for (i = 0; i < codelen; i += CODESIZE(opcode)) {
		rawopcode = codestr[i];

		switch (rawopcode) {

#ifdef WPY_UNARY_NOT_JUMP_IF
			case CONVERT(UNARY_NOT):
				handle_unary_not(codestr, codelen, blocks, i);
				break;
#endif

#ifdef WPY_PACK_MULTIPLE_POPS
                /* Replace a sequence of POP_TOPs
				   with POP_TWO, POP_THREE, ... */
			case CONVERT(POP_TOP): {
				/* Starts with 1 word for POP_TOP,
				   and 1 for the next instruction */
				Py_ssize_t n = 2;
				unsigned short *p = codestr + i;
				break;
				while (n <= 4 && p[n - 1] == CONVERT(POP_TOP) &&
					ISBASICBLOCK(i, n))
					n++;
				n -= 2;
				if (n) {
					rawopcode = POP_TOP + (n << 8);
					*p++ = CONVERT(rawopcode);
					while (n--)
						*p++ = CONVERT(NOP);
				}
			}
			break;
#endif

#ifdef WPY_REMOVE_UNREACHABLE_CODE
			case CONVERT(RAISE_0):
			case CONVERT(RAISE_1):
			case CONVERT(RAISE_2):
			case CONVERT(RAISE_3):
			case CONVERT(BREAK_LOOP):
			case CONVERT(RETURN_VALUE):
            case CONVERT(RETURN_LOCALS):
			case EXT16(RETURN_CONST):
				/* Add 1 to i if instruction was 16 bits RETURN_CONST */
				remove_unreachable_code(codestr, codelen, blocks,
					i + (rawopcode == EXT16(RETURN_CONST))
                );
				break;
#endif

#ifdef WPY_ADD_TO_FAST
			case CONVERT(BINARY_ADD):
				if (EXTRACTOP(codestr[i + 1]) == STORE_FAST &&
					ISBASICBLOCK(i, 2)) {
					oparg = EXTRACTARG(codestr[i + 1]);
					codestr[i] = PACKOPCODE(ADD_TO_FAST, oparg);
					codestr[i + 1] = CONVERT(NOP);
				}
				break;
#endif

			case EXT16(LOAD_CONST):
				GETWORD(codestr + i + 1, oparg);
				handle_load_const(codestr, codelen, blocks, consts,
								  i, oparg, 1);
				break;

#ifdef WPY_BUILD_UNPACK_TO_ROT
				/* Skip over BUILD_SEQN 1 UNPACK_SEQN 1.
				   Replace BUILD_SEQN 2 UNPACK_SEQN 2 with ROT2.
				   Replace BUILD_SEQN 3 UNPACK_SEQN 3 with ROT3 ROT2. */
			case PACKOPCODE(BUILD_TUPLE, 1):
			case PACKOPCODE(BUILD_TUPLE, 2):
			case PACKOPCODE(BUILD_TUPLE, 3):
			case PACKOPCODE(BUILD_LIST, 1):
			case PACKOPCODE(BUILD_LIST, 2):
			case PACKOPCODE(BUILD_LIST, 3):
				oparg = EXTRACTARG(rawopcode);
				rawopcode = codestr[i + 1];
				if ((rawopcode == PACKOPCODE(UNPACK_SEQUENCE, 1) ||
					rawopcode == PACKOPCODE(UNPACK_SEQUENCE, 2) ||
					rawopcode == PACKOPCODE(UNPACK_SEQUENCE, 3)) &&
					EXTRACTARG(rawopcode) == oparg &&
					ISBASICBLOCK(i, 2))
					if (oparg == 1) {
						codestr[i] = CONVERT(NOP);
						codestr[i + 1] = CONVERT(NOP);
					} else if (oparg == 2) {
						codestr[i] = CONVERT(NOP);
						codestr[i + 1] = CONVERT(ROT_TWO);
					} else if (oparg == 3) {
						codestr[i] = CONVERT(ROT_THREE);
						codestr[i + 1] = CONVERT(ROT_TWO);
				    }
				break;
#endif

				/* Simplify conditional jump to conditional jump where the
				   result of the first test implies the success of a similar
				   test or the failure of the opposite test.
				   Arises in code like:
				   "if a and b:"
				   "if a or b:"
				   "a and b or c"
				   "(a and b) and c"
				   x:JUMP_IF_FALSE y   y:JUMP_IF_FALSE z  -->  x:JUMP_IF_FALSE z
				   x:JUMP_IF_FALSE y   y:JUMP_IF_TRUE z	 -->  x:JUMP_IF_FALSE y+2
				   where y+2 is the instruction following the second test
				   (y+1 if the second jump is in 8 bit version).
				*/
			case EXT16(JUMP_IF_FALSE_ELSE_POP):
			case EXT16(JUMP_IF_TRUE_ELSE_POP):
			case EXT16(JUMP_IF_FALSE):
			case EXT16(JUMP_IF_TRUE):
#ifdef WPY_MULTIPLE_PASSES_ON_JUMP_IF
                do
#endif
            {
				int tgtrawopcode, tgtopcode;

				opcode = EXTRACTARG(rawopcode);
				GETWORD(codestr + i + 1, oparg);
				tgt = GETJUMPTARGET16(rawopcode, oparg, i + 2);
				tgtrawopcode = codestr[tgt];
				tgtopcode = EXTRACTOP(tgtrawopcode);
				tgttgt = -1; /* Signals that no condition was found */
				if (tgtrawopcode == EXT16(JUMP_IF_FALSE_ELSE_POP) ||
					tgtrawopcode == EXT16(JUMP_IF_TRUE_ELSE_POP) ||
					tgtrawopcode == EXT16(JUMP_IF_FALSE) ||
					tgtrawopcode == EXT16(JUMP_IF_TRUE))
					/* Opcodes must match in type: both FALSE or TRUE */
					if (!((tgtopcode ^ opcode) & 1)) {
						GETWORD(codestr + tgt + 1, oparg);
						tgttgt = GETJUMPTARGET16(tgtrawopcode, oparg,
												 tgt + 2) - i - 2;
						/* The new opcode type must be the target one.
						   So x:JUMP_IF_FALSE_ELSE_POP y   y:JUMP_IF_FALSE z
						   becomes x:JUMP_IF_FALSE z.
						   Notice: the following pattern can't be generated:
						   x:JUMP_IF_FALSE y   y:JUMP_IF_FALSE_ELSE_POP z
						   so we are safe doint this opcode type change */
						opcode = tgtopcode;
					}
					else {
						/* tgttgt = (tgt - i - 2) + 2; */
						tgttgt = tgt - i;
						/* The new opcode type must not be an *_ELSE_POP jump.
						   So x:JUMP_IF_FALSE_ELSE_POP y
                              y:JUMP_IF_TRUE_ELSE_POP z
						   becomes x:JUMP_IF_FALSE y + 2.
						   Setting bit 1 makes the jump not *_ELSE_POP */
						opcode |= 2;
					}
				else if (tgtopcode == JUMP_IF_FALSE_ELSE_POP ||
					tgtopcode == JUMP_IF_TRUE_ELSE_POP ||
					tgtopcode == JUMP_IF_FALSE ||
					tgtopcode == JUMP_IF_TRUE)
					/* Opcodes must match in type: both FALSE or TRUE */
					if (!((tgtopcode ^ opcode) & 1)) {
						oparg = EXTRACTARG(tgtrawopcode);
						tgttgt = GETJUMPTARGET8(tgtopcode, oparg,
												tgt + 1) - i - 2;
						opcode = tgtopcode;
					}
					else {
						/* tgttgt = (tgt - i - 2) + 1; */
						tgttgt = tgt - i - 1;
						opcode |= 2;
					}
				if (tgttgt >= 0) {
					rawopcode = EXT16(opcode);
					codestr[i] = rawopcode;
					codestr[i + 1] = CONVERT(tgttgt);
				}
			}
#ifdef WPY_MULTIPLE_PASSES_ON_JUMP_IF
            while (tgttgt >= 0);
#endif
				/* Intentional fallthrough */  

				/* Replace jumps to unconditional jumps */
			case EXT16(JUMP_FORWARD):
			case EXT16(JUMP_ABSOLUTE):
			case EXT16(SETUP_LOOP):
			case EXT16(SETUP_EXCEPT):
			case EXT16(SETUP_FINALLY):
			case EXT16(CONTINUE_LOOP):
			case EXT16(FOR_ITER): {
				int tgtrawopcode, tgtopcode;

				GETWORD(codestr + i + 1, oparg);
				tgt = GETJUMPTARGET16(rawopcode, oparg, i + 2);
#ifdef WPY_REMOVE_UNREACHABLE_CODE
				if (UNCONDITIONAL_JUMP16(rawopcode) ||
					rawopcode == EXT16(CONTINUE_LOOP)) {
					if (remove_unreachable_code(codestr, codelen, blocks,
												i + 1) - codestr == tgt &&
						rawopcode == EXT16(JUMP_FORWARD)) {
						/* All code inside the jump has been removed,
						   so the jump can be removed too */
						codestr[i] = CONVERT(NOP);
						codestr[i + 1] = CONVERT(NOP);
						break;
					}
					
				}
#endif
				/* Replace JUMP_* to a RETURN into just a RETURN */
				tgtrawopcode = codestr[tgt];
				if (UNCONDITIONAL_JUMP16(rawopcode) &&
                   (tgtrawopcode == CONVERT(RETURN_VALUE) ||
                    EXTRACTOP(tgtrawopcode) == RETURN_CONST) ||
                    tgtrawopcode == CONVERT(RETURN_LOCALS)) {
				    codestr[i] = tgtrawopcode;
				    codestr[i + 1] = CONVERT(NOP);
				    break;
                }
				tgtopcode = EXTRACTOP(tgtrawopcode);
				if (UNCONDITIONAL_JUMP16(tgtrawopcode)) {
					GETWORD(codestr + tgt + 1, oparg);
					tgttgt = GETJUMPTARGET16(tgtrawopcode, oparg, tgt + 2);
				}
				else if (UNCONDITIONAL_JUMP8(tgtopcode)) {
					oparg = EXTRACTARG(tgtrawopcode);
					tgttgt = GETJUMPTARGET8(tgtopcode, oparg, tgt + 1);
				}
				else
					break;
				if (!ABSOLUTE_JUMP16(rawopcode))
					tgttgt -= i + 2;	/* Calc relative jump addr */
				if ((tgttgt < 0) && (rawopcode == EXT16(JUMP_FORWARD))) {
					/* JMP_ABS can go backwards */
					rawopcode = EXT16(JUMP_ABSOLUTE);
					tgttgt += i + 2;	/* Restore absolute jump addr */
				}
				if (tgttgt >= 0) {
					if (ABSOLUTE_JUMP16(rawopcode))
						if (tgttgt <= 255) {
							opcode = EXTRACTARG(rawopcode);
							codestr[i] = PACKOPCODE(opcode, tgttgt);
							codestr[i + 1] = CONVERT(NOP);
						}
						else {
							codestr[i] = rawopcode;
							codestr[i + 1] = CONVERT(tgttgt);
						}
					else
						if (tgttgt <= 254) {
							 /* tgttgt was calculated for 16 bit jumps.
								Shrinking to 8 bits needs to take into account
								one more word, because offset calculation is
								i + 1 now (i + 2 for 16 bits). */
							tgttgt++;
							opcode = EXTRACTARG(rawopcode);
							codestr[i] = PACKOPCODE(opcode, tgttgt);
							codestr[i + 1] = CONVERT(NOP);
						}
						else {
							codestr[i] = rawopcode;
							codestr[i + 1] = CONVERT(tgttgt);
						}
				}
				break;
			}
			default: /* Check for 8 bit arguments versions */
				opcode = EXTRACTOP(rawopcode);
				oparg = EXTRACTARG(rawopcode);
				switch (opcode) {
#ifdef WPY_UNOP_TO_FAST
					case UNARY_OPS:
						if (EXTRACTOP(codestr[i + 1]) == STORE_FAST &&
							ISBASICBLOCK(i, 2)) {
							codestr[i] = PACKOPCODE(UNOP_TO_FAST, oparg);
							opcode = EXTRACTARG(codestr[i + 1]);
							codestr[i + 1] = PACKOPCODE(opcode, 0);
						}
						break;
#endif

#ifdef WPY_BINOP_TO_FAST
					case BINARY_OPS:
						//break;
						if (EXTRACTOP(codestr[i + 1]) == STORE_FAST &&
							ISBASICBLOCK(i, 2)) {
							codestr[i] = PACKOPCODE(BINOP_TO_FAST, oparg);
							opcode = EXTRACTARG(codestr[i + 1]);
							codestr[i + 1] = PACKOPCODE(opcode, 0);
						}
						break;
#endif

					case LOAD_FAST:
                        load_fast_superinstructions(codestr, blocks, i);
                        break;

					case LOAD_CONST:
						handle_load_const(codestr, codelen, blocks, consts,
										  i, oparg, 0);
                    	if (EXTRACTOP(codestr[i]) == LOAD_CONST)
                            load_const_superinstructions(codestr, blocks, i);
						break;

					case LOAD_GLOBAL:
                        load_global_superinstructions(codestr, blocks, i);
						break;

					case QUICK_CALL_FUNCTION:
						if (codestr[i + 1] == CONVERT(POP_TOP) &&
							ISBASICBLOCK(i, 2))
							if (EXTRACTOP(codestr[i + 2]) == LOAD_CONST &&
								codestr[i + 3] == CONVERT(RETURN_VALUE) &&
								ISBASICBLOCK(i, 4)) {
#ifdef WPY_CALL_PROC_RETURN_CONST
								codestr[i] = PACKOPCODE(CALL_PROC_RETURN_CONST, oparg);
								opcode = EXTRACTARG(codestr[i + 2]);
								codestr[i + 1] = PACKOPCODE(opcode, 0);
								codestr[i + 2] = CONVERT(NOP);
								codestr[i + 3] = CONVERT(NOP);
#endif
							}
#ifdef WPY_CALL_PROCEDURE
							else {
								codestr[i] = PACKOPCODE(QUICK_CALL_PROCEDURE, oparg);
								codestr[i + 1] = CONVERT(NOP);
							}
#endif
						break;

#ifdef WPY_CALL_PROCEDURE
                    case CALL_SUB:
						if (codestr[i + 2] == CONVERT(POP_TOP) &&
                            ISBASICBLOCK(i, 3)) {
							oparg |= 4;
							codestr[i] = PACKOPCODE(opcode, oparg);
							codestr[i + 2] = CONVERT(NOP);
                        }
						break;
#endif

#ifdef WPY_REMOVE_UNREACHABLE_CODE
					case RETURN_CONST:
						remove_unreachable_code(codestr, codelen, blocks, i);
						break;
#endif

					case JUMP_IF_FALSE_ELSE_POP:
					case JUMP_IF_TRUE_ELSE_POP:
					case JUMP_IF_FALSE:
					case JUMP_IF_TRUE:
#ifdef WPY_MULTIPLE_PASSES_ON_JUMP_IF
                        do
#endif
                    {
						int tgtrawopcode, tgtopcode;

						oparg = EXTRACTARG(rawopcode);
						tgt = GETJUMPTARGET8(opcode, oparg, i + 1);
						tgtrawopcode = codestr[tgt];
						tgtopcode = EXTRACTOP(tgtrawopcode);
						tgttgt = -1;
						if (tgtrawopcode == EXT16(JUMP_IF_FALSE_ELSE_POP) ||
							tgtrawopcode == EXT16(JUMP_IF_TRUE_ELSE_POP) ||
							tgtrawopcode == EXT16(JUMP_IF_FALSE) ||
							tgtrawopcode == EXT16(JUMP_IF_TRUE))
							if (!((tgtopcode ^ opcode) & 1)) {
								GETWORD(codestr + tgt + 1, oparg);
								tgttgt = GETJUMPTARGET16(tgtrawopcode, oparg,
														 tgt + 2) - i - 1;
								opcode = tgtopcode;
							}
							else {
								/* tgttgt = (tgt - i - 1) + 2; */
								tgttgt = tgt - i + 1;
								opcode |= 2;
							}
						else if (tgtopcode == JUMP_IF_FALSE_ELSE_POP ||
							tgtopcode == JUMP_IF_TRUE_ELSE_POP ||
							tgtopcode == JUMP_IF_FALSE ||
							tgtopcode == JUMP_IF_TRUE)
							if (!((tgtopcode ^ opcode) & 1)) {
								oparg = EXTRACTARG(tgtrawopcode);
								tgttgt = GETJUMPTARGET8(tgtopcode, oparg,
														tgt + 1) - i - 1;
								opcode = tgtopcode;
							}
							else {
								/* tgttgt = (tgt - i - 1) + 1; */
								tgttgt = tgt - i;
								opcode |= 2;
							}
						if (tgttgt >= 0)
							if (tgttgt <= 255) {
								rawopcode = PACKOPCODE(opcode, tgttgt);
								codestr[i] = rawopcode;
							}
							else {
								opcode = EXTRACTOP(rawopcode);
								tgttgt = -1;
							}
					}
#ifdef WPY_MULTIPLE_PASSES_ON_JUMP_IF
                    while (tgttgt >= 0);
#endif
						/* Intentional fallthrough */  

					case JUMP_FORWARD:
					case JUMP_ABSOLUTE:
					case SETUP_LOOP:
					case SETUP_EXCEPT:
					case SETUP_FINALLY:
					case CONTINUE_LOOP:
					case FOR_ITER: {
						int tgtrawopcode, tgtopcode;

						oparg = EXTRACTARG(rawopcode);
						tgt = GETJUMPTARGET8(opcode, oparg, i + 1);
#ifdef WPY_REMOVE_UNREACHABLE_CODE
						if (UNCONDITIONAL_JUMP8(opcode) ||
							opcode == CONTINUE_LOOP)
							if (remove_unreachable_code(codestr, codelen,
														blocks, i) -
								codestr == tgt && opcode == JUMP_FORWARD) {
								codestr[i] = CONVERT(NOP);
								break;
							}
#endif

						tgtrawopcode = codestr[tgt];
						if (UNCONDITIONAL_JUMP8(opcode) &&
							(tgtrawopcode == CONVERT(RETURN_VALUE) ||
                             EXTRACTOP(tgtrawopcode) == RETURN_CONST) ||
                             tgtrawopcode == CONVERT(RETURN_LOCALS)) {
							codestr[i] = tgtrawopcode;
							break;
						}
						tgtopcode = EXTRACTOP(tgtrawopcode);
						if (UNCONDITIONAL_JUMP16(tgtrawopcode)) {
							GETWORD(codestr + tgt + 1, oparg);
							tgttgt = GETJUMPTARGET16(tgtrawopcode, oparg,
													 tgt + 2);
						}
						else if (UNCONDITIONAL_JUMP8(tgtopcode)) {
							oparg = EXTRACTARG(tgtrawopcode);
							tgttgt = GETJUMPTARGET8(tgtopcode, oparg,
													tgt + 1);
						}
						else
							break;
						if (!ABSOLUTE_JUMP8(opcode))
							tgttgt -= i + 1;
						if ((tgttgt < 0) && (opcode == JUMP_FORWARD)) {
							opcode = JUMP_ABSOLUTE;
							tgttgt += i + 1;
						}
						if (tgttgt >= 0 && tgttgt <= 255)
							if (ABSOLUTE_JUMP8(opcode))
								codestr[i] = PACKOPCODE(opcode, tgttgt);
							else
								codestr[i] = PACKOPCODE(opcode, tgttgt);
						break;
					}
				}
		}
		opcode = EXTRACTOP(codestr[i]); /* Needed to calculate opcode size */
	}

	/* Fixup linenotab */
	i = nops = 0;
	while (i < codelen) {
		int n;
		GETWORD(codestr + i, rawopcode);
		n = CODESIZE(EXTRACTOP(rawopcode));
		while (n--) {
			addrmap[i] = (BLOCKS_ADDRMAP_TYPE) i - nops;
			i++;
		}
		nops += rawopcode == CONVERT(NOP);
	}
	cum_orig_line = 0;
	last_line = 0;
	for (i=0 ; i < tabsiz ; i+=2) {
		cum_orig_line += lineno[i];
		new_line = addrmap[cum_orig_line];
		assert (new_line - last_line < 255);
		lineno[i] =((unsigned char)(new_line - last_line));
		last_line = new_line;
	}

	/* Remove NOPs and fixup jump targets */
	source = target = codestr;
	code_end = codestr + codelen;
	while (source < code_end) {
		NEXT_RAW_WORD(source, rawopcode);
		switch (rawopcode) {
			case CONVERT(NOP):
				continue;

			case EXT16(JUMP_ABSOLUTE):
			case EXT16(CONTINUE_LOOP):
			case EXT16(LIST_APPEND_LOOP):
				*target++ = rawopcode;
				NEXTARG16(source, oparg);
				*target++ = addrmap[oparg];
				break;

			case EXT16(JUMP_IF_FALSE_ELSE_POP):
			case EXT16(JUMP_IF_TRUE_ELSE_POP):
			case EXT16(JUMP_IF_FALSE):
			case EXT16(JUMP_IF_TRUE):
			case EXT16(JUMP_FORWARD):
			case EXT16(SETUP_LOOP):
			case EXT16(SETUP_EXCEPT):
			case EXT16(SETUP_FINALLY):
			case EXT16(FOR_ITER):
				i = source - codestr - 1;
				*target++ = rawopcode;
				NEXTARG16(source, oparg);
				tgt = addrmap[oparg + i + 2] - addrmap[i] - 2;
				*target++ = tgt;
				break;
			default:
				opcode = EXTRACTOP(rawopcode);
				switch (opcode) {
					case JUMP_ABSOLUTE:
					case CONTINUE_LOOP:
					case LIST_APPEND_LOOP:
						oparg = EXTRACTARG(rawopcode);
						opcode = PACKOPCODE(opcode, addrmap[oparg]);
						*target++ = opcode;
						break;

					case JUMP_IF_FALSE_ELSE_POP:
					case JUMP_IF_TRUE_ELSE_POP:
					case JUMP_IF_FALSE:
					case JUMP_IF_TRUE:
					case JUMP_FORWARD:
					case SETUP_LOOP:
					case SETUP_EXCEPT:
					case SETUP_FINALLY:
					case FOR_ITER:
						i = source - codestr - 1;
						oparg = EXTRACTARG(rawopcode);
						tgt = addrmap[oparg + i + 1] - addrmap[i] - 1;
						opcode = PACKOPCODE(opcode, tgt);
						*target++ = opcode;
						break;
					default:
						*target++ = rawopcode;
						if (opcode >= EXTENDED_ARG32)
							*target++ = *source++;
						if (opcode >= EXTENDED_ARG16)
							*target++ = *source++;
						break;
				}
		}
	}
	i = target - codestr;
	assert(i + nops == codelen);

	code = PyString_FromStringAndSize((char *)codestr, i << 1);
#ifndef WPY_STATIC_BUFFER_ALLOCATION
	PyMem_Free(scratchpad);
	PyMem_Free(codestr);
#endif
	return code;

 exitUnchanged:
#ifndef WPY_STATIC_BUFFER_ALLOCATION
	if (scratchpad != NULL)
		PyMem_Free(scratchpad);
	if (codestr != NULL)
		PyMem_Free(codestr);
#endif
	Py_INCREF(code);
	return code;
}
