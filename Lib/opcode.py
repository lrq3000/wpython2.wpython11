
"""
opcode module - potentially shared between dis and other modules which
operate on bytecodes (e.g. peephole optimizers).
"""

__all__ = ["cmp_op", "unary_op", "binary_op",
           "hasconst", "hasname", "hasjrel", "hasjabs",
           "haslocal", "hascompare", "hasfree", "opname", "opmap",
           "HAVE_ARGUMENT", "EXTENDED_ARG16", "EXTENDED_ARG32",
           "TOTAL_OPCODES"]

cmp_op = ('BAD', 'exception match', 'is', 'is not', 'in', 'not in',
    '<', '<=', '==', '!=', '>', '>=')

unary_op = ('+', '-', 'not', 'repr', '~', 'slice_0', 'get_iter',
    'tuple_deep_copy', 'list_deep_copy', 'dict_deep_copy')

binary_op = ('**', '*', '/', '//', 'floor_div', '%',
             '-', '[]', '<<', '>>', '&', '^', '|',
             '**=', '*=', '/=', '//=', 'floor_div=', '%=',
             '-=', '<<=', '>>=', '&=', '^=', '|=',
             'slice_1', 'slice_2', 'build_slice_2',
             'cmp_bad', 'cmp_exc_match', 'is', 'is not', 'in', 'not in',
             '<', '<=', '==', '!=', '>', '>=')

hasconst = []
hasname = []
hasjrel = []
hasjabs = []
haslocal = []
hascompare = []
hasfree = []

opmap = {}
opname = {}
for op in xrange(256):
    opname[op] = '<%r>' % op
    opname[0, op] = '<UNARY %r>' % op
    opname[1, op] = '<BINARY %r>' % op
    opname[2, op] = '<TERNARY %r>' % op
    opname[3, op] = '<STACK %r>' % op
    opname[4, op] = '<STACK ERR %r>' % op
    opname[5, op] = '<MISC %r>' % op

def def_op(name, op):
    opname[op] = name
    opmap[name] = op

def name_op(name, op):
    def_op(name, op)
    hasname.append(op)

def jrel_op(name, op):
    def_op(name, op)
    hasjrel.append(op)

def jabs_op(name, op):
    def_op(name, op)
    hasjabs.append(op)

# Instruction opcodes for compiled code

# Opcodes from here haven't arguments

def_op("UNARY_OPS", 0)
def_op("BINARY_OPS", 1)
def_op("TERNARY_OPS", 2)
def_op("STACK_OPS", 3)
def_op("STACK_ERR_OPS", 4)
def_op("MISC_OPS", 5)

HAVE_ARGUMENT = 6  # Opcodes from here have an argument:

def_op("LOAD_CONST", 6)
hasconst.append(6)
def_op("LOAD_FAST", 7)
haslocal.append(7)
def_op("STORE_FAST", 8)
haslocal.append(8)
def_op("DELETE_FAST", 9)
haslocal.append(9)

name_op("LOAD_ATTR", 10)
name_op("STORE_ATTR", 11)
name_op("DELETE_ATTR", 12)
name_op("LOAD_GLOBAL", 13)
name_op("STORE_GLOBAL", 14)
name_op("DELETE_GLOBAL", 15)
# CALL_XXX opcodes defined below depend on this definition.
# Argument is defined as #args + (#kwargs<<4),
# or #args + (#kwargs<<8) if EXTENDED_ARG16.
# Opcodes must be chosen so that CALL_FUNCTION & 7 is 0,
# CALL_FUNCTION_VAR & 7 is 1, etc.
# So they must start at multiple of 8.
# Bit 2 will be 0 for FUNCTIONs and 1 for PROCEDUREs.
def_op("CALL_FUNCTION", 16)
def_op("CALL_FUNCTION_VAR", 17)
def_op("CALL_FUNCTION_KW", 18)
def_op("CALL_FUNCTION_VAR_KW", 19)

def_op("CALL_PROCEDURE", 20)
def_op("CALL_PROCEDURE_VAR", 21)
def_op("CALL_PROCEDURE_KW", 22)
def_op("CALL_PROCEDURE_VAR_KW", 23)
name_op("LOAD_NAME", 24)
name_op("STORE_NAME", 25)
name_op("DELETE_NAME", 26)
def_op("MAKE_FUNCTION", 27)
def_op("LOAD_CONSTS", 28)
hasconst.append(28)
def_op("RETURN_CONST", 29)
hasconst.append(29)

jrel_op("JUMP_FORWARD", 30)
jabs_op("JUMP_ABSOLUTE", 31)
# The following opcodes MUST start at multiple of 4 values
jrel_op("JUMP_IF_FALSE_ELSE_POP", 32)
jrel_op("JUMP_IF_TRUE_ELSE_POP", 33)
jrel_op("JUMP_IF_FALSE", 34)
jrel_op("JUMP_IF_TRUE", 35)

def_op("BUILD_TUPLE", 36)
def_op("BUILD_LIST", 37)
def_op("BUILD_MAP", 38)
name_op("IMPORT_NAME", 39)

name_op("IMPORT_FROM", 40)
jrel_op("SETUP_LOOP", 41)
jrel_op("SETUP_EXCEPT", 42)
jrel_op("SETUP_FINALLY", 43)
jabs_op("CONTINUE_LOOP", 44)
jrel_op("FOR_ITER", 45)
jabs_op("LIST_APPEND_LOOP", 46)
def_op("LOAD_DEREF", 47)
hasfree.append(47)
def_op("STORE_DEREF", 48)
hasfree.append(48)
def_op("UNPACK_SEQUENCE", 49)

def_op("LOAD_CLOSURE", 50)
hasfree.append(50)
def_op("MAKE_CLOSURE", 51)
def_op("FAST_ADD", 52)
haslocal.append(52)
def_op("CONST_ADD", 53)
hasconst.append(53)

# Opcodes from here have arguments, and one extra word
def_op("EXTENDED_ARG16", 54)
EXTENDED_ARG16 = 54
def_op("MOVE_FAST_FAST", 55)
def_op("MOVE_CONST_FAST", 56)
def_op("MOVE_GLOBAL_FAST", 57)
def_op("MOVE_FAST_ATTR_FAST", 58)
def_op("MOVE_FAST_FAST_ATTR", 59)

def_op("MOVE_CONST_FAST_ATTR", 60)
def_op("MOVE_FAST_ATTR_FAST_ATTR", 61)
def_op("LOAD_FAST_ATTR", 62)
def_op("STORE_FAST_ATTR", 63)
def_op("FAST_ADD_FAST_TO_FAST", 64)
def_op("FAST_INPLACE_ADD_FAST", 65)
def_op("FAST_UNOP_TO_FAST", 66)
def_op("FAST_INPLACE_BINOP_FAST", 67)
def_op("FAST_POW_FAST_TO_FAST", 68)
def_op("FAST_MUL_FAST_TO_FAST", 69)

def_op("FAST_DIV_FAST_TO_FAST", 70)
def_op("FAST_T_DIV_FAST_TO_FAST", 71)
def_op("FAST_F_DIV_FAST_TO_FAST", 72)
def_op("FAST_MOD_FAST_TO_FAST", 73)
def_op("FAST_SUB_FAST_TO_FAST", 74)
def_op("FAST_SUBSCR_FAST_TO_FAST", 75)
def_op("FAST_SHL_FAST_TO_FAST", 76)
def_op("FAST_SHR_FAST_TO_FAST", 77)
def_op("FAST_AND_FAST_TO_FAST", 78)
def_op("FAST_XOR_FAST_TO_FAST", 79)

def_op("FAST_OR_FAST_TO_FAST", 80)
def_op("CONST_ADD_FAST_TO_FAST", 81)
def_op("FAST_ADD_CONST_TO_FAST", 82)
def_op("FAST_INPLACE_ADD_CONST", 83)
def_op("CONST_POW_FAST_TO_FAST", 84)
def_op("CONST_MUL_FAST_TO_FAST", 85)
def_op("CONST_DIV_FAST_TO_FAST", 86)
def_op("CONST_T_DIV_FAST_TO_FAST", 87)
def_op("CONST_F_DIV_FAST_TO_FAST", 88)
def_op("CONST_MOD_FAST_TO_FAST", 89)

def_op("CONST_SUB_FAST_TO_FAST", 90)
def_op("CONST_SUBSCR_FAST_TO_FAST", 91)
def_op("CONST_SHL_FAST_TO_FAST", 92)
def_op("CONST_SHR_FAST_TO_FAST", 93)
def_op("CONST_AND_FAST_TO_FAST", 94)
def_op("CONST_XOR_FAST_TO_FAST", 95)
def_op("CONST_OR_FAST_TO_FAST", 96)
def_op("FAST_POW_CONST_TO_FAST", 97)
def_op("FAST_MUL_CONST_TO_FAST", 98)
def_op("FAST_DIV_CONST_TO_FAST", 99)

def_op("FAST_T_DIV_CONST_TO_FAST", 100)
def_op("FAST_F_DIV_CONST_TO_FAST", 101)
def_op("FAST_MOD_CONST_TO_FAST", 102)
def_op("FAST_SUB_CONST_TO_FAST", 103)
def_op("FAST_SUBSCR_CONST_TO_FAST", 104)
def_op("FAST_SHL_CONST_TO_FAST", 105)
def_op("FAST_SHR_CONST_TO_FAST", 106)
def_op("FAST_AND_CONST_TO_FAST", 107)
def_op("FAST_XOR_CONST_TO_FAST", 108)
def_op("FAST_OR_CONST_TO_FAST", 109)
def_op("FAST_ADD_FAST", 110)
def_op("FAST_BINOP_FAST", 111)
def_op("CONST_ADD_FAST", 112)
def_op("CONST_BINOP_FAST", 113)
def_op("FAST_ADD_CONST", 114)
def_op("FAST_BINOP_CONST", 115)
def_op("FAST_ADD_TO_FAST", 116)
def_op("FAST_BINOP_TO_FAST", 117)
def_op("CONST_ADD_TO_FAST", 118)
def_op("CONST_BINOP_TO_FAST", 119)

def_op("UNOP_TO_FAST", 120)
def_op("BINOP_TO_FAST", 121)
def_op("FAST_UNOP", 122)
def_op("FAST_BINOP", 123)
def_op("CONST_BINOP", 124)
def_op("LOAD_GLOBAL_ATTR", 125)
def_op("CALL_PROC_RETURN_CONST", 126)
def_op("LOAD_GLOB_FAST_CALL_FUNC", 127)
def_op("FAST_ATTR_CALL_FUNC", 128)
def_op("FAST_ATTR_CALL_PROC", 129)

# Opcodes from here have arguments, and two extra words
def_op("EXTENDED_ARG32", 130)
EXTENDED_ARG32 = 130

TOTAL_OPCODES = 131


def def_op(name, arg):
    opname[op, arg] = name
    opmap[name] = op, arg


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
def_op("INPLACE_POWER", 13)
def_op("INPLACE_MULTIPLY", 14)
def_op("INPLACE_DIVIDE", 15)
def_op("INPLACE_TRUE_DIVIDE", 16)
def_op("INPLACE_FLOOR_DIVIDE", 17)
def_op("INPLACE_MODULO", 18)
def_op("INPLACE_SUBTRACT", 19)

def_op("INPLACE_LSHIFT", 20)
def_op("INPLACE_RSHIFT", 21)
def_op("INPLACE_AND", 22)
def_op("INPLACE_XOR", 23)
def_op("INPLACE_OR", 24)
def_op("SLICE_1", 25)
def_op("SLICE_2", 26)
def_op("BUILD_SLICE_2", 27)
def_op("CMP_BAD", 28)
hascompare.append((op, 28))
def_op("CMP_EXC_MATCH", 29)
hascompare.append((op, 29))

# The following opcodes MUST start at even values
def_op("CMP_IS", 30)
hascompare.append((op, 30))
def_op("CMP_IS_NOT", 31)
hascompare.append((op, 31))
def_op("CMP_IN", 32)
hascompare.append((op, 32))
def_op("CMP_NOT_IN", 33)
hascompare.append((op, 33))

def_op("CMP_LT", 34)
hascompare.append((op, 34))
def_op("CMP_LE", 35)
hascompare.append((op, 35))
def_op("CMP_EQ", 36)
hascompare.append((op, 36))
def_op("CMP_NE", 37)
hascompare.append((op, 37))
def_op("CMP_GT", 38)
hascompare.append((op, 38))
def_op("CMP_GE", 39)
hascompare.append((op, 39))


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
def_op("LOAD_LOCALS", 4)
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


del op, def_op, name_op, jrel_op, jabs_op
