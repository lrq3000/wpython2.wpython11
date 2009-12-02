#ifndef Py_OPCODE_H
#define Py_OPCODE_H
#ifdef __cplusplus
extern "C" {
#endif


/* Instruction opcodes for compiled code */

/* Opcodes from here haven't arguments */
#define UNARY_OPS 0
#define BINARY_OPS	1
#define TERNARY_OPS	2
#define STACK_OPS 3
#define STACK_ERR_OPS 4
#define MISC_OPS 5

/* Opcodes from here have an argument */
#define HAVE_ARGUMENT	6

#define LOAD_CONST	6	/* Index in const list */
#define LOAD_FAST   7   /* Local variable number */
#define STORE_FAST	8	/* "" */
#define DELETE_FAST	9	/* "" */
#define LOAD_ATTR   10	/* Index in name list */
#define STORE_ATTR	11	/* "" */
#define DELETE_ATTR	12	/* "" */
#define LOAD_GLOBAL	13  /* Index in name list */
#define STORE_GLOBAL	14	/* "" */
#define DELETE_GLOBAL	15	/* "" */

/* CALL_XXX opcodes defined below depend on this definition.
   Argument is defined as #args + (#kwargs<<4),
   or #args + (#kwargs<<8) if EXTENDED_ARG16.
   Opcodes must be chosen so that CALL_FUNCTION & 7 is 0,
   CALL_FUNCTION_VAR & 7 is 1, etc.
   So they must start at multiple of 8.
   Bit 2 will be 0 for FUNCTIONs and 1 for PROCEDUREs. */
#define CALL_FUNCTION 16
#define CALL_FUNCTION_VAR 17
#define CALL_FUNCTION_KW  18
#define CALL_FUNCTION_VAR_KW  19
#define CALL_PROCEDURE	20
#define CALL_PROCEDURE_VAR 21
#define CALL_PROCEDURE_KW  22
#define CALL_PROCEDURE_VAR_KW  23

#define LOAD_NAME   24	/* Index in name list */
#define STORE_NAME	25	/* "" */
#define DELETE_NAME	26	/* "" */
#define MAKE_FUNCTION	27	/* #defaults */
#define LOAD_CONSTS	28	/* Index in const list */
#define RETURN_CONST  29  /* Index in const list */
#define JUMP_FORWARD	30	/* "" */
#define JUMP_ABSOLUTE	31	/* Target byte offset from beginning of code */
/* The following opcodes MUST start at multiple of 4 values */
#define JUMP_IF_FALSE_ELSE_POP	32	/* Number of words to skip */
#define JUMP_IF_TRUE_ELSE_POP	33	/* "" */
#define JUMP_IF_FALSE	34	/* Number of words to skip */
#define JUMP_IF_TRUE	35	/* "" */

#define BUILD_TUPLE	36	/* Number of tuple items */
#define BUILD_LIST	37	/* Number of list items */
#define BUILD_MAP   38	/* Always zero for now */
#define IMPORT_NAME	39	/* Index in name list */
#define IMPORT_FROM	40	/* "" */
#define SETUP_LOOP	41	  /* Target address (relative) */
#define SETUP_EXCEPT	42	/* "" */
#define SETUP_FINALLY	43	/* "" */
#define CONTINUE_LOOP	44	/* Start of loop (absolute) */
#define FOR_ITER	45      /* Target address (relative) */
#define LIST_APPEND_LOOP	46	/* Start of loop (absolute) */
#define LOAD_DEREF  47  /* Load and dereference from closure cell */ 
#define STORE_DEREF 48  /* Store into cell */ 
#define UNPACK_SEQUENCE	49  /* Number of sequence items */
#define LOAD_CLOSURE  50  /* Load free variable from closure */
#define MAKE_CLOSURE  51  /* #free vars */
#define FAST_ADD 52 /* TOP = TOP + FAST */
#define CONST_ADD 53 /* TOP = TOP + CONST */

/* FAST = TOP + SECOND. Disabled because test_zipfile fails! */
/*#define ADD_TO_FAST 54*/

/* Opcodes from here have arguments, and one extra word */
#define EXTENDED_ARG16	54
#define MOVE_FAST_FAST	55 /* FAST = FAST */
#define MOVE_CONST_FAST	56 /* FAST = CONST */
#define MOVE_GLOBAL_FAST	57	/* FAST = GLOBAL */
#define MOVE_FAST_ATTR_FAST	58	/* FAST = FAST.ATTR */
#define MOVE_FAST_FAST_ATTR	59	/* FAST.ATTR = FAST */

#define MOVE_CONST_FAST_ATTR	60	/* FAST.ATTR = CONST */
#define MOVE_FAST_ATTR_FAST_ATTR 61 /* FAST.ATTR = FAST.ATTR */
#define LOAD_FAST_ATTR	62	/* FAST.ATTR */
#define STORE_FAST_ATTR	63	/* FAST.ATTR = TOP */
#define FAST_ADD_FAST_TO_FAST 64 /* FAST = FAST + FAST */
#define FAST_INPLACE_ADD_FAST 65 /* FAST += FAST */
#define FAST_UNOP_TO_FAST 66 /* FAST = UNARY_OP FAST */
#define FAST_INPLACE_BINOP_FAST 67 /* FAST_X = FAST_X BINOP FAST_Y */
#define FAST_POW_FAST_TO_FAST 68 /* FAST = FAST ** FAST */
#define FAST_MUL_FAST_TO_FAST 69 /* FAST = FAST * FAST */

#define FAST_DIV_FAST_TO_FAST 70 /* FAST = FAST / FAST */
#define FAST_T_DIV_FAST_TO_FAST 71 /* FAST = FAST // FAST */
#define FAST_F_DIV_FAST_TO_FAST 72 /* FAST = FAST FLOOR_DIV FAST */
#define FAST_MOD_FAST_TO_FAST 73 /* FAST = FAST % FAST */
#define FAST_SUB_FAST_TO_FAST 74 /* FAST = FAST - FAST */
#define FAST_SUBSCR_FAST_TO_FAST 75 /* FAST = FAST[FAST] */
#define FAST_SHL_FAST_TO_FAST 76 /* FAST = FAST << FAST */
#define FAST_SHR_FAST_TO_FAST 77 /* FAST = FAST >> FAST */
#define FAST_AND_FAST_TO_FAST 78 /* FAST = FAST & FAST */
#define FAST_XOR_FAST_TO_FAST 79 /* FAST = FAST ^ FAST */

#define FAST_OR_FAST_TO_FAST 80 /* FAST = FAST | FAST */
#define CONST_ADD_FAST_TO_FAST 81 /* FAST = CONST + FAST */
#define FAST_ADD_CONST_TO_FAST 82 /* FAST = FAST + CONST */
#define FAST_INPLACE_ADD_CONST 83 /* FAST += CONST */
#define CONST_POW_FAST_TO_FAST 84 /* FAST = CONST ** FAST */
#define CONST_MUL_FAST_TO_FAST 85 /* FAST = CONST * FAST */
#define CONST_DIV_FAST_TO_FAST 86 /* FAST = CONST / FAST */
#define CONST_T_DIV_FAST_TO_FAST 87 /* FAST = CONST // FAST */
#define CONST_F_DIV_FAST_TO_FAST 88 /* FAST = CONST FLOOR_DIV FAST */
#define CONST_MOD_FAST_TO_FAST 89 /* FAST = CONST % FAST */

#define CONST_SUB_FAST_TO_FAST 90 /* FAST = CONST - FAST */
#define CONST_SUBSCR_FAST_TO_FAST 91 /* FAST = CONST[FAST] */
#define CONST_SHL_FAST_TO_FAST 92 /* FAST = CONST << FAST */
#define CONST_SHR_FAST_TO_FAST 93 /* FAST = CONST >> FAST */
#define CONST_AND_FAST_TO_FAST 94 /* FAST = CONST & FAST */
#define CONST_XOR_FAST_TO_FAST 95 /* FAST = CONST ^ FAST */
#define CONST_OR_FAST_TO_FAST 96 /* FAST = CONST | FAST */
#define FAST_POW_CONST_TO_FAST 97 /* FAST = FAST ** CONST */
#define FAST_MUL_CONST_TO_FAST 98 /* FAST = FAST * CONST */
#define FAST_DIV_CONST_TO_FAST 99 /* FAST = FAST / CONST */

#define FAST_T_DIV_CONST_TO_FAST 100 /* FAST = FAST // CONST */
#define FAST_F_DIV_CONST_TO_FAST 101 /* FAST = FAST FLOOR_DIV CONST */
#define FAST_MOD_CONST_TO_FAST 102 /* FAST = FAST % CONST */
#define FAST_SUB_CONST_TO_FAST 103 /* FAST = FAST - CONST */
#define FAST_SUBSCR_CONST_TO_FAST 104 /* FAST = FAST[CONST] */
#define FAST_SHL_CONST_TO_FAST 105 /* FAST = FAST << CONST */
#define FAST_SHR_CONST_TO_FAST 106 /* FAST = FAST >> CONST */
#define FAST_AND_CONST_TO_FAST 107 /* FAST = FAST & CONST */
#define FAST_XOR_CONST_TO_FAST 108 /* FAST = FAST ^ CONST */
#define FAST_OR_CONST_TO_FAST 109 /* FAST = FAST | CONST */

#define FAST_ADD_FAST 110 /* TOP = FAST ADD FAST */
#define FAST_BINOP_FAST 111 /* TOP = FAST BINOP FAST */
#define CONST_ADD_FAST 112 /* TOP = CONST ADD FAST */
#define CONST_BINOP_FAST 113 /* TOP = CONST BINOP FAST */
#define FAST_ADD_CONST 114 /* TOP = FAST ADD CONST */
#define FAST_BINOP_CONST 115 /* TOP = FAST BINOP CONST */
#define FAST_ADD_TO_FAST 116 /* FAST = TOP ADD FAST */
#define FAST_BINOP_TO_FAST 117 /* FAST = TOP BINOP FAST */
#define CONST_ADD_TO_FAST 118 /* FAST = TOP ADD CONST */
#define CONST_BINOP_TO_FAST 119 /* FAST = TOP BINOP CONST */

#define UNOP_TO_FAST 120 /* FAST = UNOP TOP */
#define BINOP_TO_FAST 121 /* FAST = SECOND BINOP TOP */
#define FAST_UNOP 122 /* TOP = FAST UNOP */
#define FAST_BINOP 123 /* TOP = TOP BINOP FAST */
#define CONST_BINOP 124 /* TOP = TOP BINOP CONST */
#define LOAD_GLOBAL_ATTR 125 /* GLOBAL.ATTR */
#define CALL_PROC_RETURN_CONST 126 /* CALL_PROCEDURE; RETURN_CONST */
#define LOAD_GLOB_FAST_CALL_FUNC 127 /* GLOBAL; FAST; CALL_FUNCTION */
#define FAST_ATTR_CALL_FUNC 128 /* FAST.ATTR() -> TOP */
#define FAST_ATTR_CALL_PROC 129 /* FAST.ATTR() */

/* Opcodes from here have arguments, and two extra words */
#define EXTENDED_ARG32	130

#define TOTAL_OPCODES  131  /* Total number of opcodes. */


/* UNARY_OPS */
#define DECL_UNARY(opcode) ((opcode) << 8 | UNARY_OPS)

#define UNARY_POSITIVE	DECL_UNARY(0)
#define UNARY_NEGATIVE	DECL_UNARY(1)
#define UNARY_NOT   DECL_UNARY(2)
#define UNARY_CONVERT   DECL_UNARY(3)
#define UNARY_INVERT	DECL_UNARY(4)
#define SLICE_0   DECL_UNARY(5)
#define GET_ITER   DECL_UNARY(6)
#define TUPLE_DEEP_COPY  DECL_UNARY(7)
#define LIST_DEEP_COPY  DECL_UNARY(8)
#define DICT_DEEP_COPY  DECL_UNARY(9)


/* BINARY_OPS */

#define DECL_BINARY(opcode) ((opcode) << 8 | BINARY_OPS)

#define BINARY_POWER	DECL_BINARY(0)
#define BINARY_MULTIPLY   DECL_BINARY(1)
#define BINARY_DIVIDE	  DECL_BINARY(2)
#define BINARY_TRUE_DIVIDE  DECL_BINARY(3)
#define BINARY_FLOOR_DIVIDE   DECL_BINARY(4)
#define BINARY_MODULO	  DECL_BINARY(5)
#define BINARY_SUBTRACT	  DECL_BINARY(6)
#define BINARY_SUBSCR	  DECL_BINARY(7)
#define BINARY_LSHIFT	  DECL_BINARY(8)
#define BINARY_RSHIFT   DECL_BINARY(9)
#define BINARY_AND	DECL_BINARY(10)
#define BINARY_XOR	DECL_BINARY(11)
#define BINARY_OR	  DECL_BINARY(12)
#define INPLACE_POWER	  DECL_BINARY(13)
#define INPLACE_MULTIPLY	DECL_BINARY(14)
#define INPLACE_DIVIDE	DECL_BINARY(15)
#define INPLACE_TRUE_DIVIDE   DECL_BINARY(16)
#define INPLACE_FLOOR_DIVIDE  DECL_BINARY(17)
#define INPLACE_MODULO	DECL_BINARY(18)
#define INPLACE_SUBTRACT	DECL_BINARY(19)
#define INPLACE_LSHIFT	DECL_BINARY(20)
#define INPLACE_RSHIFT	DECL_BINARY(21)
#define INPLACE_AND	  DECL_BINARY(22)
#define INPLACE_XOR	  DECL_BINARY(23)
#define INPLACE_OR	DECL_BINARY(24)
#define SLICE_1   DECL_BINARY(25)
#define SLICE_2   DECL_BINARY(26)
#define BUILD_SLICE_2   DECL_BINARY(27)
#define CMP_BAD  DECL_BINARY(28)
#define CMP_EXC_MATCH  DECL_BINARY(29)
/* The following opcodes MUST start at even values */
#define CMP_IS  DECL_BINARY(30)
#define CMP_IS_NOT  DECL_BINARY(31)
#define CMP_IN  DECL_BINARY(32)
#define CMP_NOT_IN  DECL_BINARY(33)

#define CMP_LT  DECL_BINARY(34)
#define CMP_LE  DECL_BINARY(35)
#define CMP_EQ  DECL_BINARY(36)
#define CMP_NE  DECL_BINARY(37)
#define CMP_GT  DECL_BINARY(38)
#define CMP_GE  DECL_BINARY(39)


/* TERNARY_OPS */
#define DECL_TERNARY(opcode) ((opcode) << 8 | TERNARY_OPS)

#define SLICE_3   DECL_TERNARY(0)
#define BUILD_SLICE_3   DECL_TERNARY(1)
#define BUILD_CLASS   DECL_TERNARY(2)


/* STACK_OPS */

#define DECL_STACK(opcode) ((opcode) << 8 | STACK_OPS)

/* The following opcodes MUST be consecutive */
#define POP_TOP   DECL_STACK(0)
#define ROT_TWO	  DECL_STACK(1)
#define ROT_THREE	  DECL_STACK(2)
#define ROT_FOUR  DECL_STACK(3)
#define DUP_TOP   DECL_STACK(4)
#define DUP_TOP_TWO	  DECL_STACK(5)
#define DUP_TOP_THREE	  DECL_STACK(6)


/* STACK_ERR_OPS */

#define DECL_STACK_ERR(opcode) ((opcode) << 8 | STACK_ERR_OPS)

/* The following opcodes MUST be consecutive */
#define STORE_SLICE_0	  DECL_STACK_ERR(0)
#define STORE_SLICE_1	  DECL_STACK_ERR(1)
#define STORE_SLICE_2	  DECL_STACK_ERR(2)
#define STORE_SLICE_3	  DECL_STACK_ERR(3)

/* The following opcodes MUST be consecutive */
#define DELETE_SLICE_0	DECL_STACK_ERR(4)
#define DELETE_SLICE_1	DECL_STACK_ERR(5)
#define DELETE_SLICE_2	DECL_STACK_ERR(6)
#define DELETE_SLICE_3	DECL_STACK_ERR(7)

#define STORE_SUBSCR    DECL_STACK_ERR(8)
#define DELETE_SUBSCR   DECL_STACK_ERR(9)
#define STORE_MAP   DECL_STACK_ERR(10)
#define PRINT_EXPR   DECL_STACK_ERR(11)
#define PRINT_ITEM_TO   DECL_STACK_ERR(12)
#define PRINT_ITEM  DECL_STACK_ERR(13)
#define PRINT_NEWLINE_TO  DECL_STACK_ERR(14)
#define PRINT_NEWLINE   DECL_STACK_ERR(15)


/* MISC_OPS */

#define DECL_MISC_OPS(opcode) ((opcode) << 8 | MISC_OPS)

#define NOP	  DECL_MISC_OPS(0)
#define STOP_CODE	  DECL_MISC_OPS(1)
#define BINARY_ADD	  DECL_MISC_OPS(2)
#define INPLACE_ADD	  DECL_MISC_OPS(3)
#define LOAD_LOCALS   DECL_MISC_OPS(4)
#define EXEC_STMT   DECL_MISC_OPS(5)
#define IMPORT_STAR	  DECL_MISC_OPS(6)
#define POP_BLOCK   DECL_MISC_OPS(7)
#define POP_FOR_BLOCK   DECL_MISC_OPS(8)
#define END_FINALLY	  DECL_MISC_OPS(9)
#define WITH_CLEANUP  DECL_MISC_OPS(10)
/* The following opcodes MUST be consecutive */
#define RAISE_0   DECL_MISC_OPS(11)
#define RAISE_1   DECL_MISC_OPS(12)
#define RAISE_2   DECL_MISC_OPS(13)
#define RAISE_3   DECL_MISC_OPS(14)

#define BREAK_LOOP  DECL_MISC_OPS(15)
#define RETURN_VALUE  DECL_MISC_OPS(16)
#define YIELD_VALUE  DECL_MISC_OPS(17)


enum cmp_op {PyCmp_LT=Py_LT, PyCmp_LE=Py_LE, PyCmp_EQ=Py_EQ, PyCmp_NE=Py_NE, PyCmp_GT=Py_GT, PyCmp_GE=Py_GE,
	     PyCmp_IN, PyCmp_NOT_IN, PyCmp_IS, PyCmp_IS_NOT, PyCmp_EXC_MATCH, PyCmp_BAD};

#define HAS_ARG(op) ((op) >= HAVE_ARGUMENT)

/* Defines macros to check normal, EXTENDED_16 and EXTENDED_32 opcodes */

#ifdef WORDS_BIGENDIAN
#define EXT16(opcode) EXTENDED_ARG16 << 8 | (opcode)
#define EXT32(opcode) EXTENDED_ARG32 << 8 | (opcode)
#define EXTRACTOP(value) ((value) >> 8)
#define EXTRACTARG(value) ((value) & 0xff)
#define EXTRACTOP_ARG(opcode, op, arg) op = (opcode) >> 8; \
									   arg = (opcode) & 0xff
#define CONVERT(value) ((value) & 0xff) << 8 | (value) >> 8
#else
#define EXT16(opcode) (((opcode) << 8) + EXTENDED_ARG16)
#define EXT32(opcode) (((opcode) << 8) + EXTENDED_ARG32)
#define EXTRACTOP(opcode) ((opcode) & 0xff)
#define EXTRACTARG(opcode) ((opcode) >> 8)
#define EXTRACTOP_ARG(opcode, op, arg) op = (opcode) & 0xff; \
									   arg = (opcode) >> 8
#define CONVERT(value) value
#endif

#define MATCHOP(value, op) (((value == CONVERT(EXT16(op))) || \
				(value == CONVERT(EXT32(op))) || \
				(EXTRACTOP(value) == op)))


#ifdef __cplusplus
}
#endif
#endif /* !Py_OPCODE_H */
