#ifndef Py_WPYTHON_H
#define Py_WPYTHON_H
/* Since this is a "meta-include" file, no #ifdef __cplusplus / extern "C" { */


/*  Keep in mind that disabling some optimizations do not remove all related
    code: some checks are always present!
    
*/


/* ************** AST RELATED OPTIMIZATIONS **************

It makes little sense to disable all the AST optimizations because in CPython
they are always active, since the peephole optimizer can't be disabled (neither
in a selective way).

The only ones that can be disabled to make a comparison with CPython are
WPY_DICT_TO_CONST and WPY_IN_CONST_TO_PURE_CONST.
That's because they're missing in the old peephole optimizer.

However, the AST optmizations are much better, because can handle more cases.
We can have tuples or lists which holds lists or dictionaries, whenever the
peephole optimizer can handle only tuples or lists with basic types (ints,
strings, et. all).
Unfortunately, it's quite hard to disable such behaviours to make a perfect
comparison with the old peephole optimizer.

*/

/* Converts "NONE" identifier in the Py_None object  */
#define WPY_NONE_NAME_TO_CONST

/* Converts a string in a PyString or PyUnicode object  */
#define WPY_STRING_TO_CONST

/* Converts a number in a PyInt, PyLong or PyFloat object  */
#define WPY_NUMBER_TO_CONST

/* Converts () to a PyTuple_New(0) object  */
#define WPY_EMPTY_TUPLE_TO_CONST

/* Converts [] to a PyList_New(0) object  */
#define WPY_EMPTY_LIST_TO_CONST

/* Converts a sequence of constants (such as [1, 'a', None]) to a
   PyList object */
#define WPY_LIST_TO_CONST

/* Converts a dictionary of constants (such as {'a' : 1, '' : None}) to a
   PyDict object  */
#define WPY_DICT_TO_CONST

/* Converts a backquoted constant expression (such as `[]`) to a
   PyString object */
#define WPY_BACKQUOTE_TO_CONST

/* Enables binary constant folding (such as a = 1 + 2 * 3 -> a = 7) */
#define WPY_BINARY_CONSTANT_FOLDING

/* Enables subscription of constant folding (such as (1, 2, 3, 4)[2] -> 3 */
#define WPY_CONSTANT_SUBSCRIPTION_FOLDING

/* Converts a sequence of constants (such as (1, 2, 3, 4)) to a
   PyTuple object */
#define WPY_TUPLE_TO_CONST

/* Enables unary constant folding (such as a = -(1 + 2) -> a = -3) */
#define WPY_UNARY_CONSTANT_FOLDING

/* not (a is b) -->  a is not b
   not (a in b) -->  a not in b
   not (a is not b) -->  a is b
   not (a not in b) -->  a in b */
#define WPY_NOT_WITH_IN_OR_IS

/* Handles "x in y" where y is constant.
   If y == [1, 2, 3], converts it to (1, 2, 3).
   Anyway, it marks y as "pure constant", so that in case y is a dictionary,
   it will not require a DICT_DEEP_COPY. */
#define WPY_IN_CONST_TO_PURE_CONST


/* ************** COMPILER RELATED OPTIMIZATIONS ************** */

/* If defined, a strict control will be made against the opcode limits
   in stack effect calculations.
   If undefined, only standard opcodes must ben used
   (which is the common case: no bad opcodes are expected into the regular
    opcode stream). */

#define WPY_SAFER_OPCODE_STACK_EFFECT

/* Collects all the constants in a function definition. Example:

def f():
  def g(a, b, x = 1, y = 2, z = 3): pass

With CPython disassembles as:

  2           0 LOAD_CONST               1 (1)
              3 LOAD_CONST               2 (2)
              6 LOAD_CONST               3 (3)
              9 LOAD_CONST               4 (<code object g at 01D0F890,
                                            file "<stdin>", line 2>)
             12 MAKE_FUNCTION            3
             15 STORE_FAST               0 (g)
             18 LOAD_CONST               0 (None)
             21 RETURN_VALUE

Whereas in WPython disassembles as:

  2           0 LOAD_CONSTS                   1 ((1, 2, 3, <code object g at
                                                            01E8E088, file
                                                            "<stdin>", line 2>)
                                                            )
              1 MAKE_FUNCTION                 3
              2 STORE_FAST                    0 (g)
              3 RETURN_CONST                  0 (None)

All constants are collected and loaded using only one instruction, LOAD_CONSTS.
*/

#define WPY_FUNCTION_DEF_CONSTANTS_COLLECTION

/* Same as WPY_FUNCTION_DEF_CONSTANTS_COLLECTION, but for lambda definition. */

#define WPY_LAMBDA_DEF_CONSTANTS_COLLECTION

/* Don't generate DEEP_COPY_LIST or DEEP_COPY_DICT if a FOR_ITER statement
   if we have a list or dictionary that holds pure constant elements. Example:

def f():
  for x in['a', 'b', 'c']:
    print x

Without the define disassembles as:

  2           0 LOAD_CONST                    1 (['a', 'b', 'c'])
              1 LIST_DEEP_COPY
              2 GET_ITER
        >>    3 FOR_ITER                      5 (to 9)
              4 STORE_FAST                    0 (x)

  3           5 LOAD_FAST                     0 (x)
              6 PRINT_ITEM
              7 PRINT_NEWLINE
              8 JUMP_ABSOLUTE                 3
        >>    9 RETURN_CONST                  0 (None)

Whereas with the define disassembles as:

  2           0 LOAD_CONST                    1 (['a', 'b', 'c'])
              1 GET_ITER
        >>    2 FOR_ITER                      5 (to 8)
              3 STORE_FAST                    0 (x)

  3           4 LOAD_FAST                     0 (x)
              5 PRINT_ITEM
              6 PRINT_NEWLINE
              7 JUMP_ABSOLUTE                 2
        >>    8 RETURN_CONST                  0 (None)

*/

#define WPY_NO_DEEP_COPY_FOR_CONTENT_CONST_IN_FOR

/* Skips SETUP_LOOP and POP_BLOCK if no break or continue instructions are
   found in a for loop. */

#define WPY_DROP_SETUP_IN_FOR

/* Sames as WPY_DROP_SETUP_IN_FOR, but for while construct. */

#define WPY_DROP_SETUP_IN_WHILE

/* Uses LOAD_CONSTS on import statements, instead of multiple LOAD_CONSTs. */

#define WPY_LOAD_CONSTS_ON_IMPORT

/* Converts LOAD_GLOBAL "None" or "LOAD_NAME" None in LOAD_CONST Py_None. */

#define WPY_LOAD_NONE_TO_LOAD_CONST

/* Seeks for two or more constants are loaded in a list building
   expression, using LOAD_CONSTS to replace the multiple LOAD_CONST
   instructions. Example:

def f(x, y, z):
  return [1, 2, x, 3, 4, 5, y, 6, z]

With CPython disassembles as:

  2           0 LOAD_CONST               1 (1)
              3 LOAD_CONST               2 (2)
              6 LOAD_FAST                0 (x)
              9 LOAD_CONST               3 (3)
             12 LOAD_CONST               4 (4)
             15 LOAD_CONST               5 (5)
             18 LOAD_FAST                1 (y)
             21 LOAD_CONST               6 (6)
             24 LOAD_FAST                2 (z)
             27 BUILD_LIST               9
             30 RETURN_VALUE

Whereas in WPython disassembles as:

  2           0 LOAD_CONSTS                   1 ((1, 2))
              1 LOAD_FAST                     0 (x)
              2 LOAD_CONSTS                   2 ((3, 4, 5))
              3 LOAD_FAST                     1 (y)
              4 LOAD_CONST                    3 (6)
              5 LOAD_FAST                     2 (z)
              6 BUILD_LIST                    9
              7 RETURN_VALUE
*/

#define WPY_PARTIAL_CONSTANTS_IN_LIST

/* Sames as WPY_PARTIAL_CONSTANTS_IN_LIST, but for tuples. */

#define WPY_PARTIAL_CONSTANTS_IN_TUPLE

/* Similar to WPY_PARTIAL_CONSTANTS_IN_LIST and WPY_PARTIAL_CONSTANTS_IN_TUPLE,
   but used on function calls. Example:

def f(x): g(1, 2, x, y = 3, z = 4)

With CPython disassembles as:

  1           0 LOAD_GLOBAL              0 (g)
              3 LOAD_CONST               1 (1)
              6 LOAD_CONST               2 (2)
              9 LOAD_FAST                0 (x)
             12 LOAD_CONST               3 ('y')
             15 LOAD_CONST               4 (3)
             18 LOAD_CONST               5 ('z')
             21 LOAD_CONST               6 (4)
             24 CALL_FUNCTION          515
             27 POP_TOP
             28 LOAD_CONST               0 (None)
             31 RETURN_VALUE

Whereas in WPython disassembles as:

  1           0 LOAD_GLOBAL                   0 (g)
              1 LOAD_CONSTS                   1 ((1, 2))
              2 LOAD_FAST                     0 (x)
              3 LOAD_CONSTS                   2 (('y', 3, 'z', 4))
              4 CALL_PROC_RETURN_CONST          35; RETURN None
*/

#define WPY_PARTIAL_CONSTANTS_IN_FUNCTION_CALL

/* Uses LOAD_CONSTS if a key and value are both "pure" constants. Example:

def f(x): {'a' : 1, 'b' : x, 'c' : 3}

With CPython disassembles as:

  1           0 BUILD_MAP                3
              3 LOAD_CONST               1 (1)
              6 LOAD_CONST               2 ('a')
              9 STORE_MAP
             10 LOAD_FAST                0 (x)
             13 LOAD_CONST               3 ('b')
             16 STORE_MAP
             17 LOAD_CONST               4 (3)
             20 LOAD_CONST               5 ('c')
             23 STORE_MAP
             24 RETURN_VALUE

Whereas in WPython disassembles as:

  1           0 BUILD_MAP                     3
              1 LOAD_CONSTS                   1 ((1, 'a'))
              2 STORE_MAP
              3 LOAD_FAST                     0 (x)
              4 LOAD_CONST                    2 ('b')
              5 STORE_MAP
              6 LOAD_CONSTS                   3 ((3, 'c'))
              7 STORE_MAP
              8 RETURN_VALUE
*/

#define WPY_LOAD_CONSTS_ON_DICT_ITEM


/* ************** PEEPHOLE OPTIMIZER RELATED OPTIMIZATIONS **************

It makes little sense to disable all the peephole optimizations, because in
CPython they are always active, since the peephole optimizer can't be disabled
(neither in a selective way).

The only ones that can be disabled to make a comparison with CPython are
WPY_STATIC_BUFFER_ALLOCATION and WPY_MULTIPLE_PASSES_ON_JUMP_IF.
That's because they're missing in the old peephole optimizer.

*/

/* Enable the bytecode peepholer optimizer. */

/* #define WPY_BYTECODE_PEEPHOLER */

/* Replace UNARY_NOT JUMP_IF_FALSE
   with	   NOP JUMP_IF_TRUE, and
   UNARY_NOT JUMP_IF_FALSE_ELSE_POP
   with	   NOP JUMP_IF_TRUE_ELSE_POP */

#define WPY_UNARY_NOT_JUMP_IF

/* Remove unreachable code after unconditional flow change instructions
   RAISE_0, RAISE_1, RAISE_2, RAISE_3, BREAK_LOOP, RETURN_VALUE, RETURN_CONST,
   CONTINUE_LOOP, LIST_APPEND_LOOP, JUMP_FORWARD. */

#define WPY_REMOVE_UNREACHABLE_CODE

/* Use statically allocated memory blocks for code and blocks. */

#define WPY_STATIC_BUFFER_ALLOCATION

/* Replace a sequence of POP_TOPs with POP_TWO, POP_THREE, ...
   Not implemented by the moment: some strange things happen. */

/* #define WPY_PACK_MULTIPLE_POPS */

/* Skip over BUILD_SEQN 1 UNPACK_SEQN 1.
				   Replace BUILD_SEQN 2 UNPACK_SEQN 2 with ROT2.
				   Replace BUILD_SEQN 3 UNPACK_SEQN 3 with ROT3 ROT2. */

#define WPY_BUILD_UNPACK_TO_ROT

/* When simplifying conditional jumps such that:

     x:JUMP_IF_FALSE y   y:JUMP_IF_FALSE z  -->  x:JUMP_IF_FALSE z
     x:JUMP_IF_FALSE y   y:JUMP_IF_TRUE z	 -->  x:JUMP_IF_FALSE y+2

   more loops and checks on these conditions will be made, until no more will be
   found, or no optimizations can be applied (e.g. because an 8 bit jump needs
   a 16 bit value for the target). */

#define WPY_MULTIPLE_PASSES_ON_JUMP_IF


/* ************** CEVAL (AND PEEPHOLE) RELATED OPTIMIZATIONS ***************/

/* Replace CALL_FUNCTION POP_TOP with CALL_PROCEDURE. */

#define WPY_CALL_PROCEDURE

/* Replace LOAD_FAST BINARY_ADD with FAST_ADD. */

#define WPY_FAST_ADD

/* Replace LOAD_CONST BINARY_ADD with CONST_ADD. */

#define WPY_CONST_ADD

/* Replace BINARY_ADD STORE_FAST with ADD_TO_FAST NOP.
   Disabled because test_zipfile.py fails!*/

/*#define WPY_ADD_TO_FAST*/

/* Replace LOAD_FAST STORE_FAST with MOVE_FAST_FAST. */

#define WPY_MOVE_FAST_FAST

/* Replace LOAD_CONST STORE_FAST with MOVE_CONST_FAST. */

#define WPY_MOVE_CONST_FAST

/* Replace LOAD_GLOBAL STORE_FAST with MOVE_GLOBAL_FAST. */

#define WPY_MOVE_GLOBAL_FAST

/* Replace LOAD_FAST LOAD_ATTR STORE_FAST with MOVE_FAST_ATTR_FAST. */

#define WPY_MOVE_FAST_ATTR_FAST

/* Replace LOAD_FAST LOAD_FAST STORE_ATTR with MOVE_FAST_FAST_ATTR. */

#define WPY_MOVE_FAST_FAST_ATTR

/* Replace LOAD_CONST LOAD_FAST STORE_ATTR with MOVE_CONST_FAST_ATTR. */

#define WPY_MOVE_CONST_FAST_ATTR

/* Replace LOAD_FAST LOAD_ATTR LOAD_FAST STORE_ATTR with
   MOVE_FAST_ATTR_FAST_ATTR. */

#define WPY_MOVE_FAST_ATTR_FAST_ATTR

/* Replace LOAD_FAST LOAD_ATTR with LOAD_FAST_ATTR. */

#define WPY_LOAD_FAST_ATTR

/* Replace LOAD_FAST STORE_ATTR with STORE_FAST_ATTR. */

#define WPY_STORE_FAST_ATTR

/* Replace LOAD_FAST LOAD_FAST BINARY_ADD STORE_FAST with
   FAST_ADD_FAST_TO_FAST. */

#define WPY_FAST_ADD_FAST_TO_FAST

/* Replace LOAD_FAST LOAD_FAST INPLACE_ADD STORE_FAST with
   FAST_INPLACE_ADD_FAST. */

#define WPY_FAST_INPLACE_ADD_FAST

/* Replace LOAD_FAST UNARY_OP STORE_FAST with FAST_UNOP_TO_FAST. */

#define WPY_FAST_UNOP_TO_FAST

/* Replace LOAD_FAST[X] LOAD_FAST BINARY_OP STORE_FAST[X] with
   FAST_INPLACE_BINOP_FAST. */

#define WPY_FAST_INPLACE_BINOP_FAST

/* Replace LOAD_FAST LOAD_FAST BINARY_OP[*] STORE_FAST with
   WPY_FAST_QUICKOP_FAST_TO_FAST.
   BINARY_OP can only be: BINARY_POWER, BINARY_MULTIPLY, BINARY_DIVIDE,
   BINARY_TRUE_DIVIDE, BINARY_FLOOR_DIVIDE, BINARY_MODULO, BINARY_SUBTRACT,
   BINARY_SUBSCR, BINARY_LSHIFT, BINARY_RSHIFT, BINARY_AND, BINARY_XOR,
   BINARY_XOR. */

#define WPY_FAST_QUICKOP_FAST_TO_FAST

#define WPY_FAST_INPLACE_ADD_FAST

/* Replace LOAD_CONST LOAD_FAST BINARY_ADD STORE_FAST with
   CONST_ADD_FAST_TO_FAST. */

#define WPY_CONST_ADD_FAST_TO_FAST

/* Replace LOAD_FAST LOAD_CONST BINARY_ADD STORE_FAST with
   FAST_ADD_CONST_TO_FAST. */

#define WPY_FAST_ADD_CONST_TO_FAST

/* Replace LOAD_FAST[X] LOAD_CONST INPLACE_ADD STORE_FAST[X] with
   FAST_INPLACE_ADD_CONST. */

#define WPY_FAST_INPLACE_ADD_CONST

/* Replace LOAD_CONST LOAD_FAST BINARY_OP[*] STORE_FAST with
   WPY_FAST_QUICKOP_FAST_TO_FAST.
   BINARY_OP can only be: BINARY_POWER, BINARY_MULTIPLY, BINARY_DIVIDE,
   BINARY_TRUE_DIVIDE, BINARY_FLOOR_DIVIDE, BINARY_MODULO, BINARY_SUBTRACT,
   BINARY_SUBSCR, BINARY_LSHIFT, BINARY_RSHIFT, BINARY_AND, BINARY_XOR,
   BINARY_XOR. */

#define WPY_CONST_QUICKOP_FAST_TO_FAST

/* Replace LOAD_FAST LOAD_CONST BINARY_OP[*] STORE_FAST with
   WPY_FAST_QUICKOP_FAST_TO_FAST.
   BINARY_OP can only be: BINARY_POWER, BINARY_MULTIPLY, BINARY_DIVIDE,
   BINARY_TRUE_DIVIDE, BINARY_FLOOR_DIVIDE, BINARY_MODULO, BINARY_SUBTRACT,
   BINARY_SUBSCR, BINARY_LSHIFT, BINARY_RSHIFT, BINARY_AND, BINARY_XOR,
   BINARY_XOR. */

#define WPY_FAST_QUICKOP_CONST_TO_FAST

/* Replace LOAD_FAST LOAD_FAST BINARY_ADD with FAST_ADD_FAST. */

#define WPY_FAST_ADD_FAST

/* Replace LOAD_FAST LOAD_FAST BINARY_OPS with FAST_BINOP_FAST. */

#define WPY_FAST_BINOP_FAST

/* Replace LOAD_CONST LOAD_FAST BINARY_ADD with CONST_ADD_FAST. */

#define WPY_CONST_ADD_FAST

/* Replace LOAD_CONST LOAD_FAST BINARY_OPS with CONST_BINOP_FAST. */

#define WPY_CONST_BINOP_FAST

/* Replace LOAD_FAST LOAD_CONST BINARY_ADD with FAST_ADD_CONST. */

#define WPY_FAST_ADD_CONST

/* Replace LOAD_FAST LOAD_CONST BINARY_OPS with FAST_BINOP_CONST. */

#define WPY_FAST_BINOP_CONST

/* Replace LOAD_FAST BINARY_ADD STORE_FAST with FAST_ADD_TO_FAST. */

#define WPY_FAST_ADD_TO_FAST

/* Replace LOAD_FAST BINARY_OPS STORE_FAST with FAST_BINOP_TO_FAST. */

#define WPY_FAST_BINOP_TO_FAST

/* Replace LOAD_CONST BINARY_ADD STORE_FAST with CONST_ADD_TO_FAST. */

#define WPY_CONST_ADD_TO_FAST

/* Replace LOAD_CONST BINARY_OPS STORE_FAST with CONST_BINOP_TO_FAST. */

#define WPY_CONST_BINOP_TO_FAST

/* Replace UNARY_OPS STORE_FAST with UNOP_TO_FAST. */

#define WPY_UNOP_TO_FAST

/* Replace BINARY_OPS STORE_FAST with BINOP_TO_FAST. */

#define WPY_BINOP_TO_FAST

/* Replace LOAD_FAST UNARY_OPS with FAST_UNOP. */

#define WPY_FAST_UNOP

/* Replace LOAD_FAST BINARY_OPS with FAST_BINOP. */

#define WPY_FAST_BINOP

/* Replace LOAD_CONST BINARY_OPS with CONST_BINOP. */

#define WPY_CONST_BINOP

/* Replace LOAD_GLOBAL LOAD_ATTR with LOAD_GLOBAL_ATTR. */

#define WPY_LOAD_GLOBAL_ATTR

/* Replace CALL_FUNCTION POP_TOP LOAD_CONST RETURN_VALUE with
   CALL_PROC_RETURN_CONST. */

#define WPY_CALL_PROC_RETURN_CONST

/* Replace LOAD_GLOBAL LOAD_FAST CALL_FUNCTION with
   LOAD_GLOB_FAST_CALL_FUNC. */

#define WPY_LOAD_GLOB_FAST_CALL_FUNC

/* Replace LOAD_FAST LOAD_ATTR CALL_FUNCTION with
   FAST_ATTR_CALL_FUNC. */

#define WPY_FAST_ATTR_CALL_FUNC

/* Replace LOAD_FAST LOAD_ATTR CALL_FUNCTION POP_TOP with
   FAST_ATTR_CALL_PROC. */

#define WPY_FAST_ATTR_CALL_PROC


/* ************** MISCELLANEOUS ************** */

/* Enable small integer (0..255) super instructions. */

/* #define WPY_SMALLINT_SUPER_INSTRUCTIONS */

/* Uncomment this if you want to disable most optimizations making WPython
   similar (but not equal) to CPython. */

/* #define WPY_SAME_AS_CPTYHON */

#ifdef WPY_SAME_AS_CPTYHON

#undef WPY_DICT_TO_CONST
#undef WPY_IN_CONST_TO_PURE_CONST
#undef WPY_FUNCTION_DEF_CONSTANTS_COLLECTION
#undef WPY_LAMBDA_DEF_CONSTANTS_COLLECTION
#undef WPY_NO_DEEP_COPY_FOR_CONTENT_CONST_IN_FOR
#undef WPY_DROP_SETUP_IN_FOR
#undef WPY_DROP_SETUP_IN_WHILE
#undef WPY_LOAD_CONSTS_ON_IMPORT
#undef WPY_LOAD_NONE_TO_LOAD_CONST
#undef WPY_PARTIAL_CONSTANTS_IN_LIST
#undef WPY_PARTIAL_CONSTANTS_IN_TUPLE
#undef WPY_PARTIAL_CONSTANTS_IN_FUNCTION_CALL
#undef WPY_LOAD_CONSTS_ON_DICT_ITEM
#undef WPY_STATIC_BUFFER_ALLOCATION
#undef WPY_MULTIPLE_PASSES_ON_JUMP_IF
#define WPY_NO_SUPER_INSTRUCTIONS

#endif

/* Uncomment this if you want to disable all superinstructions! */

/* #define WPY_NO_SUPER_INSTRUCTIONS */

#ifdef WPY_NO_SUPER_INSTRUCTIONS

#undef WPY_RETURN_CONST
#undef WPY_CALL_PROCEDURE
#undef WPY_FAST_ADD
#undef WPY_CONST_ADD
#undef WPY_MOVE_FAST_FAST
#undef WPY_ADD_TO_FAST
#undef WPY_MOVE_FAST_FAST
#undef WPY_MOVE_CONST_FAST
#undef WPY_MOVE_GLOBAL_FAST
#undef WPY_MOVE_FAST_ATTR_FAST
#undef WPY_MOVE_FAST_FAST_ATTR
#undef WPY_MOVE_CONST_FAST_ATTR
#undef WPY_MOVE_FAST_ATTR_FAST_ATTR
#undef WPY_LOAD_FAST_ATTR
#undef WPY_STORE_FAST_ATTR
#undef WPY_FAST_ADD_FAST_TO_FAST
#undef WPY_FAST_INPLACE_ADD_FAST
#undef WPY_FAST_UNOP_TO_FAST
#undef WPY_FAST_INPLACE_BINOP_FAST
#undef WPY_FAST_QUICKOP_FAST_TO_FAST
#undef WPY_FAST_INPLACE_ADD_FAST
#undef WPY_CONST_ADD_FAST_TO_FAST
#undef WPY_FAST_ADD_CONST_TO_FAST
#undef WPY_FAST_INPLACE_ADD_CONST
#undef WPY_CONST_QUICKOP_FAST_TO_FAST
#undef WPY_FAST_QUICKOP_CONST_TO_FAST
#undef WPY_FAST_ADD_FAST
#undef WPY_FAST_BINOP_FAST
#undef WPY_CONST_ADD_FAST
#undef WPY_CONST_BINOP_FAST
#undef WPY_FAST_ADD_CONST
#undef WPY_FAST_BINOP_CONST
#undef WPY_FAST_ADD_TO_FAST
#undef WPY_FAST_BINOP_TO_FAST
#undef WPY_CONST_ADD_TO_FAST
#undef WPY_CONST_BINOP_TO_FAST
#undef WPY_UNOP_TO_FAST
#undef WPY_BINOP_TO_FAST
#undef WPY_FAST_UNOP
#undef WPY_FAST_BINOP
#undef WPY_CONST_BINOP
#undef WPY_LOAD_GLOBAL_ATTR
#undef WPY_CALL_PROC_RETURN_CONST
#undef WPY_LOAD_GLOB_FAST_CALL_FUNC
#undef WPY_FAST_ATTR_CALL_FUNC
#undef WPY_FAST_ATTR_CALL_PROC
#undef WPY_SMALLINT_SUPER_INSTRUCTIONS

#endif

#endif /* !Py_WPYTHON_H */
