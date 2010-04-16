
"""
opcode module - potentially shared between dis and other modules which
operate on bytecodes (e.g. peephole optimizers).
"""

__all__ = ["cmp_op", "unary_op", "binary_op", "ternary_op",
           "opname", "opmap", "opargs",
           "arg_not_used", "arg_int", "arg_const", "arg_local", "arg_free",
           "arg_name", "arg_quick_func", "arg_jrel", "arg_jabs",
           "arg_unary", "arg_binary", "arg_ternary",
           "HAVE_ARGUMENT", "EXTENDED_ARG16", "EXTENDED_ARG32",
           "TOTAL_OPCODES"]

cmp_op = ('BAD', 'exception match', 'is', 'is not', 'in', 'not in',
    '<', '<=', '==', '!=', '>', '>=')

unary_op = ('+', '-', 'not', 'repr', '~', 'slice_0', 'get_iter',
    'tuple_deep_copy', 'list_deep_copy', 'dict_deep_copy')

binary_op = ('**', '*', '/', '//', 'floor_div', '%',
             '-', '[]', '<<', '>>', '&', '^', '|', '+',
             '**=', '*=', '/=', '//=', 'floor_div=', '%=',
             '-=', '<<=', '>>=', '&=', '^=', '|=', '+=',
             'cmp_bad', '<', '<=', '==', '!=', '>', '>=',
             'is', 'is not', 'in', 'not in', 'cmp_exc_match',
             'slice_1', 'slice_2', 'build_slice_2', 'get_generator',
             '%', '%', 'join', 'join')

ternary_op = ('slice_3', 'build_slice_3', 'build_class')

arg_not_used = 0
arg_int = 1
arg_const = 2
arg_local = 3
arg_free = 4
arg_name = 5
arg_quick_func = 6
arg_jrel = 7
arg_jabs = 8
arg_unary = 9
arg_binary = 10
arg_ternary = 11

opmap = {}
opname = {}
opargs = {}
for op in xrange(256):
    opname[op] = '<%r>' % op
    opname[0, op] = '<UNARY %r>' % op
    opname[1, op] = '<BINARY %r>' % op
    opname[2, op] = '<TERNARY %r>' % op
    opname[3, op] = '<STACK %r>' % op
    opname[4, op] = '<STACK ERR %r>' % op
    opname[5, op] = '<MISC %r>' % op
    opname[124, op] = '<CALL_SUB %r>' % op
    opargs[op] = ()
    opargs[0, op] = ()
    opargs[1, op] = ()
    opargs[2, op] = ()
    opargs[3, op] = ()
    opargs[4, op] = ()
    opargs[5, op] = ()
    opargs[124, op] = ()

def def_op(name, op, *args):
    opname[op] = name
    opmap[name] = op
    opargs[op] = args

# Instruction opcodes for compiled code

# Opcodes from here haven't arguments

def_op("UNARY_OPS", 0)
def_op("BINARY_OPS", 1)
def_op("TERNARY_OPS", 2)
def_op("STACK_OPS", 3)
def_op("STACK_ERR_OPS", 4)
def_op("MISC_OPS", 5)

HAVE_ARGUMENT = 6  # Opcodes from here have an argument:

def_op("LOAD_CONST", 6, arg_const)
def_op("LOAD_FAST", 7, arg_local)
def_op("STORE_FAST", 8, arg_local)
def_op("DELETE_FAST", 9, arg_local)

def_op("LOAD_ATTR", 10, arg_name)
def_op("STORE_ATTR", 11, arg_name)
def_op("DELETE_ATTR", 12, arg_name)
def_op("LOAD_GLOBAL", 13, arg_name)
def_op("STORE_GLOBAL", 14, arg_name)
def_op("DELETE_GLOBAL", 15, arg_name)
# The following opcodes MUST start at multiple of 2 values
def_op("QUICK_CALL_FUNCTION", 16, arg_quick_func)
def_op("QUICK_CALL_PROCEDURE", 17, arg_quick_func)

def_op("LOAD_NAME", 18, arg_name)
def_op("STORE_NAME", 19, arg_name)

def_op("DELETE_NAME", 20, arg_name)
def_op("MAKE_FUNCTION", 21, arg_int)
def_op("LOAD_CONSTS", 22, arg_const)
def_op("RETURN_CONST", 23, arg_const)

# The following opcodes MUST start at multiple of 4 values
def_op("JUMP_IF_FALSE_ELSE_POP", 24, arg_jrel)
def_op("JUMP_IF_TRUE_ELSE_POP", 25, arg_jrel)
def_op("JUMP_IF_FALSE", 26, arg_jrel)
def_op("JUMP_IF_TRUE", 27, arg_jrel)

def_op("JUMP_FORWARD", 28, arg_jrel)
def_op("JUMP_ABSOLUTE", 29, arg_jabs)

def_op("BUILD_TUPLE", 30, arg_int)
def_op("BUILD_LIST", 31, arg_int)
def_op("BUILD_MAP", 32, arg_int)
def_op("IMPORT_NAME", 33, arg_name)
def_op("IMPORT_FROM", 34, arg_name)
def_op("SETUP_LOOP", 35, arg_jrel)
def_op("SETUP_EXCEPT", 36, arg_jrel)
def_op("SETUP_FINALLY", 37, arg_jrel)
def_op("CONTINUE_LOOP", 38, arg_jabs)
def_op("FOR_ITER", 39, arg_jrel)

def_op("LIST_APPEND_LOOP", 40, arg_jabs)
def_op("LOAD_DEREF", 41, arg_free)
def_op("STORE_DEREF", 42, arg_free)
def_op("UNPACK_SEQUENCE", 43, arg_int)
def_op("LOAD_CLOSURE", 44, arg_free)
def_op("MAKE_CLOSURE", 45, arg_int)
def_op("FAST_ADD", 46, arg_local)
def_op("CONST_ADD", 47, arg_const)

# Opcodes from here have arguments, and one extra word
def_op("EXTENDED_ARG16", 48)
EXTENDED_ARG16 = 48

def_op("MOVE_FAST_FAST", 49, arg_local, arg_local)

def_op("MOVE_CONST_FAST", 50, arg_const, arg_local)
def_op("MOVE_GLOBAL_FAST", 51, arg_name, arg_local)
def_op("MOVE_FAST_ATTR_FAST", 52, arg_local, arg_name, arg_local)
def_op("MOVE_FAST_FAST_ATTR", 53, arg_local, arg_local, arg_name)
def_op("MOVE_CONST_FAST_ATTR", 54, arg_const, arg_local, arg_name)
def_op("MOVE_FAST_ATTR_FAST_ATTR", 55, arg_local, arg_name, arg_name)
def_op("LOAD_FAST_ATTR", 56, arg_local, arg_name)
def_op("STORE_FAST_ATTR", 57, arg_local, arg_name)
def_op("FAST_ADD_FAST_TO_FAST", 58, arg_local, arg_local, arg_local)
def_op("FAST_INPLACE_ADD_FAST", 59, arg_local, arg_local)

def_op("FAST_UNOP_TO_FAST", 60, arg_local, arg_unary, arg_local)
def_op("FAST_INPLACE_BINOP_FAST", 61, arg_local, arg_local, arg_binary)
def_op("FAST_POW_FAST_TO_FAST", 62, arg_local, arg_local, arg_local)
def_op("FAST_MUL_FAST_TO_FAST", 63, arg_local, arg_local, arg_local)
def_op("FAST_DIV_FAST_TO_FAST", 64, arg_local, arg_local, arg_local)
def_op("FAST_T_DIV_FAST_TO_FAST", 65, arg_local, arg_local, arg_local)
def_op("FAST_F_DIV_FAST_TO_FAST", 66, arg_local, arg_local, arg_local)
def_op("FAST_MOD_FAST_TO_FAST", 67, arg_local, arg_local, arg_local)
def_op("FAST_SUB_FAST_TO_FAST", 68, arg_local, arg_local, arg_local)
def_op("FAST_SUBSCR_FAST_TO_FAST", 69, arg_local, arg_local, arg_local)

def_op("FAST_SHL_FAST_TO_FAST", 70, arg_local, arg_local, arg_local)
def_op("FAST_SHR_FAST_TO_FAST", 71, arg_local, arg_local, arg_local)
def_op("FAST_AND_FAST_TO_FAST", 72, arg_local, arg_local, arg_local)
def_op("FAST_XOR_FAST_TO_FAST", 73, arg_local, arg_local, arg_local)
def_op("FAST_OR_FAST_TO_FAST", 74, arg_local, arg_local, arg_local)
def_op("CONST_ADD_FAST_TO_FAST", 75, arg_const, arg_local, arg_local)
def_op("FAST_ADD_CONST_TO_FAST", 76, arg_local, arg_const, arg_local)
def_op("FAST_INPLACE_ADD_CONST", 77, arg_local, arg_const)
def_op("CONST_POW_FAST_TO_FAST", 78, arg_const, arg_local, arg_local)
def_op("CONST_MUL_FAST_TO_FAST", 79, arg_const, arg_local, arg_local)

def_op("CONST_DIV_FAST_TO_FAST", 80, arg_const, arg_local, arg_local)
def_op("CONST_T_DIV_FAST_TO_FAST", 81, arg_const, arg_local, arg_local)
def_op("CONST_F_DIV_FAST_TO_FAST", 82, arg_const, arg_local, arg_local)
def_op("CONST_MOD_FAST_TO_FAST", 83, arg_const, arg_local, arg_local)
def_op("CONST_SUB_FAST_TO_FAST", 84, arg_const, arg_local, arg_local)
def_op("CONST_SUBSCR_FAST_TO_FAST", 85, arg_const, arg_local, arg_local)
def_op("CONST_SHL_FAST_TO_FAST", 86, arg_const, arg_local, arg_local)
def_op("CONST_SHR_FAST_TO_FAST", 87, arg_const, arg_local, arg_local)
def_op("CONST_AND_FAST_TO_FAST", 88, arg_const, arg_local, arg_local)
def_op("CONST_XOR_FAST_TO_FAST", 89, arg_const, arg_local, arg_local)

def_op("CONST_OR_FAST_TO_FAST", 90, arg_const, arg_local, arg_local)
def_op("FAST_POW_CONST_TO_FAST", 91, arg_local, arg_const, arg_local)
def_op("FAST_MUL_CONST_TO_FAST", 92, arg_local, arg_const, arg_local)
def_op("FAST_DIV_CONST_TO_FAST", 93, arg_local, arg_const, arg_local)
def_op("FAST_T_DIV_CONST_TO_FAST", 94, arg_local, arg_const, arg_local)
def_op("FAST_F_DIV_CONST_TO_FAST", 95, arg_local, arg_const, arg_local)
def_op("FAST_MOD_CONST_TO_FAST", 96, arg_local, arg_const, arg_local)
def_op("FAST_SUB_CONST_TO_FAST", 97, arg_local, arg_const, arg_local)
def_op("FAST_SUBSCR_CONST_TO_FAST", 98, arg_local, arg_const, arg_local)
def_op("FAST_SHL_CONST_TO_FAST", 99, arg_local, arg_const, arg_local)

def_op("FAST_SHR_CONST_TO_FAST", 100, arg_local, arg_const, arg_local)
def_op("FAST_AND_CONST_TO_FAST", 101, arg_local, arg_const, arg_local)
def_op("FAST_XOR_CONST_TO_FAST", 102, arg_local, arg_const, arg_local)
def_op("FAST_OR_CONST_TO_FAST", 103, arg_local, arg_const, arg_local)
def_op("FAST_ADD_FAST", 104, arg_local, arg_local)
def_op("FAST_BINOP_FAST", 105, arg_local, arg_local, arg_binary)
def_op("CONST_ADD_FAST", 106, arg_const, arg_local)
def_op("CONST_BINOP_FAST", 107, arg_const, arg_local, arg_binary)
def_op("FAST_ADD_CONST", 108, arg_local, arg_const)
def_op("FAST_BINOP_CONST", 109, arg_local, arg_const, arg_binary)

def_op("FAST_ADD_TO_FAST", 110, arg_local, arg_local)
def_op("FAST_BINOP_TO_FAST", 111, arg_local, arg_binary, arg_local)
def_op("CONST_ADD_TO_FAST", 112, arg_const, arg_local)
def_op("CONST_BINOP_TO_FAST", 113, arg_const, arg_binary, arg_local)
def_op("UNOP_TO_FAST", 114, arg_unary, arg_local)
def_op("BINOP_TO_FAST", 115, arg_binary, arg_local)
def_op("FAST_UNOP", 116, arg_local, arg_unary)
def_op("FAST_BINOP", 117, arg_local, arg_binary)
def_op("CONST_BINOP", 118, arg_const, arg_binary)
def_op("LOAD_GLOBAL_ATTR", 119, arg_name, arg_name)

def_op("CALL_PROC_RETURN_CONST", 120, arg_quick_func, arg_const)
def_op("LOAD_GLOB_FAST_CALL_FUNC", 121, arg_name, arg_local, arg_quick_func)
def_op("FAST_ATTR_CALL_FUNC", 122, arg_local, arg_name)
def_op("FAST_ATTR_CALL_PROC", 123, arg_local, arg_name)
def_op("CALL_SUB", 124)
def_op("FAST_INPLACE_BINOP_CONST", 125, arg_const, arg_local, arg_binary)
def_op("INT_POW_FAST_TO_FAST", 126, arg_local, arg_int, arg_local)
def_op("INT_MUL_FAST_TO_FAST", 127, arg_local, arg_int, arg_local)
def_op("INT_DIV_FAST_TO_FAST", 128, arg_local, arg_int, arg_local)
def_op("INT_T_DIV_FAST_TO_FAST", 129, arg_local, arg_int, arg_local)

def_op("INT_F_DIV_FAST_TO_FAST", 130, arg_local, arg_int, arg_local)
def_op("INT_MOD_FAST_TO_FAST", 131, arg_local, arg_int, arg_local)
def_op("INT_SUB_FAST_TO_FAST", 132, arg_local, arg_int, arg_local)
def_op("INT_SUBSCR_FAST_TO_FAST", 133, arg_local, arg_int, arg_local)
def_op("INT_SHL_FAST_TO_FAST", 134, arg_local, arg_int, arg_local)
def_op("INT_SHR_FAST_TO_FAST", 135, arg_local, arg_int, arg_local)
def_op("INT_AND_FAST_TO_FAST", 136, arg_local, arg_int, arg_local)
def_op("INT_XOR_FAST_TO_FAST", 137, arg_local, arg_int, arg_local)
def_op("INT_OR_FAST_TO_FAST", 138, arg_local, arg_int, arg_local)
def_op("INT_ADD_FAST_TO_FAST", 139, arg_local, arg_int, arg_local)

def_op("FAST_POW_INT_TO_FAST", 140, arg_local, arg_int, arg_local)
def_op("FAST_MUL_INT_TO_FAST", 141, arg_local, arg_int, arg_local)
def_op("FAST_DIV_INT_TO_FAST", 142, arg_local, arg_int, arg_local)
def_op("FAST_T_DIV_INT_TO_FAST", 143, arg_local, arg_int, arg_local)
def_op("FAST_F_DIV_INT_TO_FAST", 144, arg_local, arg_int, arg_local)
def_op("FAST_MOD_INT_TO_FAST", 145, arg_local, arg_int, arg_local)
def_op("FAST_SUB_INT_TO_FAST", 146, arg_local, arg_int, arg_local)
def_op("FAST_SUBSCR_INT_TO_FAST", 147, arg_local, arg_int, arg_local)
def_op("FAST_SHL_INT_TO_FAST", 148, arg_local, arg_int, arg_local)
def_op("FAST_SHR_INT_TO_FAST", 149, arg_local, arg_int, arg_local)

def_op("FAST_AND_INT_TO_FAST", 150, arg_local, arg_int, arg_local)
def_op("FAST_XOR_INT_TO_FAST", 151, arg_local, arg_int, arg_local)
def_op("FAST_OR_INT_TO_FAST", 152, arg_local, arg_int, arg_local)
def_op("FAST_ADD_INT_TO_FAST", 153, arg_local, arg_int, arg_local)
def_op("FAST_INPLACE_BINOP_INT", 154, arg_local, arg_int, arg_binary)
def_op("INT_BINOP_FAST", 155, arg_local, arg_int, arg_binary)
def_op("FAST_BINOP_INT", 156, arg_local, arg_int, arg_binary)
def_op("INT_BINOP_TO_FAST", 157, arg_int, arg_binary, arg_local)
def_op("INT_BINOP", 158, arg_not_used, arg_int, arg_binary)

# Opcodes from here have arguments, and two extra words
def_op("EXTENDED_ARG32", 159)
EXTENDED_ARG32 = 159

TOTAL_OPCODES = 160


def def_op(name, arg, *args):
    opname[op, arg] = name
    opmap[name] = op, arg
    opargs[op, arg] = args


# Instruction opcodes for unary operators

op = 0 # UNARY_OPS
def_op("UNARY_POSITIVE", 0)
def_op("UNARY_NEGATIVE", 1)
def_op("UNARY_NOT", 2)
def_op("UNARY_CONVERT", 3)
def_op("UNARY_INVERT", 4)
def_op("SLICE_0", 5)
def_op("GET_ITER", 6)
def_op("TUPLE_DEEP_COPY", 7)
def_op("LIST_DEEP_COPY", 8)
def_op("DICT_DEEP_COPY", 9)

# Instruction opcodes for binary operators

op = 1 # BINARY_OPS
def_op("BINARY_POWER", 0)
def_op("BINARY_MULTIPLY", 1)
def_op("BINARY_DIVIDE", 2)
def_op("BINARY_TRUE_DIVIDE", 3)
def_op("BINARY_FLOOR_DIVIDE", 4)
def_op("BINARY_MODULO", 5)
def_op("BINARY_SUBTRACT", 6)
def_op("BINARY_SUBSCR", 7)
def_op("BINARY_LSHIFT", 8)
def_op("BINARY_RSHIFT", 9)

def_op("BINARY_AND", 10)
def_op("BINARY_XOR", 11)
def_op("BINARY_OR", 12)
def_op("BINARY_ADD2", 13)
def_op("INPLACE_POWER", 14)
def_op("INPLACE_MULTIPLY", 15)
def_op("INPLACE_DIVIDE", 16)
def_op("INPLACE_TRUE_DIVIDE", 17)
def_op("INPLACE_FLOOR_DIVIDE", 18)
def_op("INPLACE_MODULO", 19)

def_op("INPLACE_SUBTRACT", 20)
def_op("INPLACE_LSHIFT", 21)
def_op("INPLACE_RSHIFT", 22)
def_op("INPLACE_AND", 23)
def_op("INPLACE_XOR", 24)
def_op("INPLACE_OR", 25)
def_op("INPLACE_ADD2", 26)
def_op("CMP_BAD", 27, arg_binary)
def_op("CMP_LT", 28, arg_binary)
def_op("CMP_LE", 29, arg_binary)

def_op("CMP_EQ", 30, arg_binary)
def_op("CMP_NE", 31, arg_binary)
def_op("CMP_GT", 32, arg_binary)
def_op("CMP_GE", 33, arg_binary)

# The following opcodes MUST start at even values
def_op("CMP_IS", 34, arg_binary)
def_op("CMP_IS_NOT", 35, arg_binary)
def_op("CMP_IN", 36, arg_binary)
def_op("CMP_NOT_IN", 37, arg_binary)

def_op("CMP_EXC_MATCH", 38, arg_binary)
def_op("SLICE_1", 39)

def_op("SLICE_2", 40)
def_op("BUILD_SLICE_2", 41)
def_op("GET_GENERATOR", 42)
def_op("STRING_MODULO", 43)
def_op("UNICODE_MODULO", 44)
def_op("STRING_JOIN", 45)
def_op("UNICODE_JOIN", 46)


# Instruction opcodes for ternary operators

op = 2 # TERNARY_OPS
def_op("SLICE_3", 0)
def_op("BUILD_SLICE_3", 1)
def_op("BUILD_CLASS", 2)


# Instruction opcodes for stack instructions

op = 3 # STACK_OPS
def_op("POP_TOP", 0)
def_op("ROT_TWO", 1)
def_op("ROT_THREE", 2)
def_op("ROT_FOUR", 3)
def_op("DUP_TOP", 4)
def_op("DUP_TOP_TWO", 5)
def_op("DUP_TOP_THREE", 6)
def_op("DUP_TOP_ROT_THREE", 7)
def_op("ROT_TWO_POP_TOP", 8)


# Instruction opcodes for stack instructions that can fail

op = 4 # STACK_ERR_OPS
def_op("STORE_SLICE_0", 0)
def_op("STORE_SLICE_1", 1)
def_op("STORE_SLICE_2", 2)
def_op("STORE_SLICE_3", 3)
def_op("DELETE_SLICE_0", 4)
def_op("DELETE_SLICE_1", 5)
def_op("DELETE_SLICE_2", 6)
def_op("DELETE_SLICE_3", 7)
def_op("STORE_SUBSCR", 8)
def_op("DELETE_SUBSCR", 9)

def_op("STORE_MAP", 10)
def_op("PRINT_EXPR", 11)
def_op("PRINT_ITEM_TO", 12)
def_op("PRINT_ITEM", 13)
def_op("PRINT_NEWLINE_TO", 14)
def_op("PRINT_NEWLINE", 15)


# Instruction opcodes for miscellaneous instructions

op = 5 # MISC_OPS
def_op("NOP", 0)
def_op("STOP_CODE", 1)
def_op("BINARY_ADD", 2)
def_op("INPLACE_ADD", 3)
def_op("RETURN_LOCALS", 4)
def_op("EXEC_STMT", 5)
def_op("IMPORT_STAR", 6)
def_op("POP_BLOCK", 7)
def_op("POP_FOR_BLOCK", 8)
def_op("END_FINALLY", 9)

def_op("WITH_CLEANUP", 10)
def_op("RAISE_0", 11)
def_op("RAISE_1", 12)
def_op("RAISE_2", 13)
def_op("RAISE_3", 14)
def_op("BREAK_LOOP", 15)
def_op("RETURN_VALUE", 16)
def_op("YIELD_VALUE", 17)


# Instruction opcodes for CALL_SUB

op = 124 # CALL_SUB
# CALL_XXX opcodes defined below depend on this definition.
# Arguments are defined as #args and #kwargs.
# Opcodes must be chosen so that CALL_FUNCTION & 3 is 0,
# CALL_FUNCTION_VAR & 3 is 1, etc.
# So they must start at multiple of 4.
# Bit 2 will be 0 for FUNCTIONs and 1 for PROCEDUREs.
def_op("CALL_FUNCTION", 0, arg_not_used, arg_int, arg_int)
def_op("CALL_FUNCTION_VAR", 1, arg_not_used, arg_int, arg_int)
def_op("CALL_FUNCTION_KW", 2, arg_not_used, arg_int, arg_int)
def_op("CALL_FUNCTION_VAR_KW", 3, arg_not_used, arg_int, arg_int)
def_op("CALL_PROCEDURE", 4, arg_not_used, arg_int, arg_int)
def_op("CALL_PROCEDURE_VAR", 5, arg_not_used, arg_int, arg_int)
def_op("CALL_PROCEDURE_KW", 6, arg_not_used, arg_int, arg_int)
def_op("CALL_PROCEDURE_VAR_KW", 7, arg_not_used, arg_int, arg_int)


del op, def_op
