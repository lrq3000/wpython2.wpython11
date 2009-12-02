"""Disassembler of Python byte code into mnemonics."""

import sys
import types

from opcode import *
from opcode import __all__ as _opcodes_all

__all__ = ["dis","disassemble","distb","disco",
           "get_extended_opcode"] + _opcodes_all
del _opcodes_all

FAST_ADD = opmap['FAST_ADD']
CONST_ADD = opmap['CONST_ADD']
MOVE_FAST_FAST = opmap['MOVE_FAST_FAST']
MOVE_CONST_FAST = opmap['MOVE_CONST_FAST']
MOVE_GLOBAL_FAST = opmap['MOVE_GLOBAL_FAST']
MOVE_FAST_ATTR_FAST = opmap['MOVE_FAST_ATTR_FAST']
MOVE_FAST_FAST_ATTR = opmap['MOVE_FAST_FAST_ATTR']
MOVE_CONST_FAST_ATTR = opmap['MOVE_CONST_FAST_ATTR']
MOVE_FAST_ATTR_FAST_ATTR = opmap['MOVE_FAST_ATTR_FAST_ATTR']
LOAD_FAST_ATTR = opmap['LOAD_FAST_ATTR']
STORE_FAST_ATTR = opmap['STORE_FAST_ATTR']
FAST_ADD_FAST_TO_FAST = opmap['FAST_ADD_FAST_TO_FAST']
FAST_INPLACE_ADD_FAST = opmap['FAST_INPLACE_ADD_FAST']
FAST_UNOP_TO_FAST = opmap['FAST_UNOP_TO_FAST']
FAST_INPLACE_BINOP_FAST = opmap['FAST_INPLACE_BINOP_FAST']
FAST_POW_FAST_TO_FAST = opmap['FAST_POW_FAST_TO_FAST']
FAST_OR_FAST_TO_FAST = opmap['FAST_OR_FAST_TO_FAST']
CONST_ADD_FAST_TO_FAST = opmap['CONST_ADD_FAST_TO_FAST']
FAST_ADD_CONST_TO_FAST = opmap['FAST_ADD_CONST_TO_FAST']
FAST_INPLACE_ADD_CONST = opmap['FAST_INPLACE_ADD_CONST']
CONST_POW_FAST_TO_FAST = opmap['CONST_POW_FAST_TO_FAST']
CONST_OR_FAST_TO_FAST = opmap['CONST_OR_FAST_TO_FAST']
FAST_POW_CONST_TO_FAST = opmap['FAST_POW_CONST_TO_FAST']
FAST_OR_CONST_TO_FAST = opmap['FAST_OR_CONST_TO_FAST']
FAST_ADD_FAST = opmap['FAST_ADD_FAST']
FAST_BINOP_FAST = opmap['FAST_BINOP_FAST']
CONST_ADD_FAST = opmap['CONST_ADD_FAST']
CONST_BINOP_FAST = opmap['CONST_BINOP_FAST']
FAST_ADD_CONST = opmap['FAST_ADD_CONST']
FAST_BINOP_CONST = opmap['FAST_BINOP_CONST']
FAST_ADD_TO_FAST = opmap['FAST_ADD_TO_FAST']
FAST_BINOP_TO_FAST = opmap['FAST_BINOP_TO_FAST']
CONST_ADD_TO_FAST = opmap['CONST_ADD_TO_FAST']
CONST_BINOP_TO_FAST = opmap['CONST_BINOP_TO_FAST']
UNOP_TO_FAST = opmap['UNOP_TO_FAST']
BINOP_TO_FAST = opmap['BINOP_TO_FAST']
FAST_UNOP = opmap['FAST_UNOP']
FAST_BINOP = opmap['FAST_BINOP']
CONST_BINOP = opmap['CONST_BINOP']
LOAD_GLOBAL_ATTR = opmap['LOAD_GLOBAL_ATTR']
CALL_PROC_RETURN_CONST = opmap['CALL_PROC_RETURN_CONST']
LOAD_GLOB_FAST_CALL_FUNC = opmap['LOAD_GLOB_FAST_CALL_FUNC']
FAST_ATTR_CALL_FUNC = opmap['FAST_ATTR_CALL_FUNC']
FAST_ATTR_CALL_PROC = opmap['FAST_ATTR_CALL_PROC']

codeobject_types = frozenset((types.MethodType,
                    types.FunctionType,
                    types.CodeType,
                    types.ClassType))

def dis(x=None, deep=False):
    """Disassemble classes, methods, functions, or code.

    With no x argument, disassemble the last traceback.
    With deep = True, disassemble constant code objects.

    """
    if x is None:
        distb(None, deep)
        return
    if type(x) is types.InstanceType:
        x = x.__class__
    if hasattr(x, 'im_func'):
        x = x.im_func
    if hasattr(x, 'func_code'):
        x = x.func_code
    if hasattr(x, '__dict__'):
        items = x.__dict__.items()
        items.sort()
        for name, x1 in items:
            if type(x1) in codeobject_types:
                print "Disassembly of %s:" % name
                try:
                    dis(x1, deep)
                except TypeError, msg:
                    print "Sorry:", msg
                print
    elif hasattr(x, 'co_code'):
        disassemble(x, deep=deep)
    elif isinstance(x, str):
        disassemble_string(x, deep=deep)
    else:
        raise TypeError, \
              "don't know how to disassemble %s objects" % \
              type(x).__name__

def distb(tb=None, deep=False):
    """Disassemble a traceback (default: last traceback)."""
    if tb is None:
        try:
            tb = sys.last_traceback
        except AttributeError:
            raise RuntimeError, "no last traceback to disassemble"
        while tb.tb_next: tb = tb.tb_next
    disassemble(tb.tb_frame.f_code, tb.tb_lasti, deep)

def get_extended_opcode(code, i, op, oparg):
    """Checks for an extended opcode, giving back the real opcode,
argument and additional wordsize """
    if op >= EXTENDED_ARG32:
        return oparg, ord(code[i]) + ord(code[i + 1]) * 256 + \
            ord(code[i + 2]) * 65536 + ord(code[i + 3]) * 16777216, 2
    elif op == EXTENDED_ARG16:
        return oparg, ord(code[i]) + ord(code[i + 1]) * 256, 1
    elif op > EXTENDED_ARG16:
        return op, (oparg, ord(code[i]), ord(code[i + 1])), 1
    else:
        return op, oparg, 0

def disassemble(co, lasti=-1, deep=False):
    """Disassemble a code object."""
    common_disassemble(co.co_code, lasti, deep, dict(findlinestarts(co)),
                       co.co_varnames, co.co_names, co.co_consts,
                       co.co_cellvars + co.co_freevars)

def disassemble_string(code, lasti=-1, varnames=None, names=None,
                       constants=None, deep=False):
    common_disassemble(code, lasti, deep, {}, varnames, names, constants, None)

def common_disassemble(code, lasti, deep, linestarts,
                       varnames, names, constants, frees):
    """Disassemble a code object."""

    def get_const(index):
        if constants:
            const = constants[index]
            if deep and type(const) in codeobject_types:
              code_objects.append(const)
            return repr(const)
        else:
            return '(%d)' % index

    def get_name(index):
        if names is not None:
            return names[index]
        else:
            return '(%d)' % index

    def get_varname(index):
        if varnames is not None:
            return varnames[index]
        else:
            return '(%d)' % index

    def get_free(index):
        if frees is not None:
            return frees[index]
        else:
            return '(%d)' % index

    labels = findlabels(code)
    n = len(code)
    i = offset = 0
    code_objects = []
    while i < n:
        op = ord(code[i])
        oparg = ord(code[i + 1])
        i += 2
        if offset in linestarts:
            if offset > 0:
                print
            print "%3d" % linestarts[offset],
        else:
            print '   ',

        if offset == lasti: print '-->',
        else: print '   ',
        if offset in labels: print '>>',
        else: print '  ',
        print repr(offset).rjust(4),
        offset += 1
        if op >= HAVE_ARGUMENT:
            op, oparg, size = get_extended_opcode(code, i, op, oparg)
            i += size + size
            offset += size
            print opname[op].ljust(25),
            if EXTENDED_ARG16 < op < EXTENDED_ARG32:
                print ''.rjust(5),
                if op == MOVE_FAST_FAST:
                    print get_varname(oparg[0]) + ' -> ' + \
                        get_varname(oparg[1])
                elif op == MOVE_CONST_FAST:
                    print get_const(oparg[0]) + ' -> ' + \
                            get_varname(oparg[1])
                elif op == MOVE_GLOBAL_FAST:
                    print get_name(oparg[0]) + ' -> ' + \
                        get_varname(oparg[1])
                elif op == MOVE_FAST_ATTR_FAST:
                    print get_varname(oparg[0]) + '.' + get_name(oparg[1]) + \
                        ' -> ' + get_varname(oparg[2])
                elif op == MOVE_FAST_FAST_ATTR:
                    print get_varname(oparg[0]) + ' -> ' + \
                          get_varname(oparg[1]) + '.' + get_name(oparg[2])
                elif op == MOVE_CONST_FAST_ATTR:
                    print get_const(oparg[0]) + ' -> ' + \
                          get_varname(oparg[1]) + '.' + get_name(oparg[2])
                elif op == MOVE_FAST_ATTR_FAST_ATTR:
                    print get_varname(oparg[0]) + '.' + get_name(oparg[1]) + \
                          ' -> ' + get_varname(oparg[0]) + '.' + \
                          get_name(oparg[2])
                elif op == LOAD_FAST_ATTR:
                    print get_varname(oparg[0]) + '.' + get_name(oparg[1])
                elif op == STORE_FAST_ATTR:
                    print ' -> ' + get_varname(oparg[0]) + '.' + \
                          get_name(oparg[1])
                elif op == FAST_ADD_FAST_TO_FAST:
                    print get_varname(oparg[0]) + ' + ' + \
                        get_varname(oparg[1]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == FAST_INPLACE_ADD_FAST:
                    print get_varname(oparg[0]) + ' += ' + \
                        get_varname(oparg[1])
                elif op == FAST_UNOP_TO_FAST:
                    print unary_op[oparg[1]] + ' ' + get_varname(oparg[0]) + \
                        ' -> ' + get_varname(oparg[2])
                elif op == FAST_INPLACE_BINOP_FAST:
                    print get_varname(oparg[0]) + ' ' + binary_op[oparg[2]] + \
                        ' ' + get_varname(oparg[1]) + ' -> ' + \
                        get_varname(oparg[0])
                elif FAST_POW_FAST_TO_FAST <= op <= FAST_OR_FAST_TO_FAST:
                    print get_varname(oparg[0]) + ' ' + \
                          binary_op[op - FAST_POW_FAST_TO_FAST] + \
                        ' ' + get_varname(oparg[1]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == CONST_ADD_FAST_TO_FAST:
                    print get_const(oparg[0]) + ' + ' + \
                        get_varname(oparg[1]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == FAST_ADD_CONST_TO_FAST:
                    print get_varname(oparg[0]) + ' + ' + \
                        get_const(oparg[1]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == FAST_INPLACE_ADD_CONST:
                    print get_varname(oparg[0]) + ' += ' + \
                        get_const(oparg[1])
                elif CONST_POW_FAST_TO_FAST <= op <= CONST_OR_FAST_TO_FAST:
                    print get_const(oparg[0]) + ' ' + \
                          binary_op[op - CONST_POW_FAST_TO_FAST] + \
                        ' ' + get_varname(oparg[1]) + ' -> ' + \
                        get_varname(oparg[2])
                elif FAST_POW_CONST_TO_FAST <= op <= FAST_OR_CONST_TO_FAST:
                    print get_varname(oparg[0]) + ' ' + \
                          binary_op[op - FAST_POW_CONST_TO_FAST] + \
                        ' ' + get_const(oparg[1]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == FAST_ADD_FAST:
                    print get_varname(oparg[0]) + ' + ' + \
                        get_varname(oparg[1])
                elif op == FAST_BINOP_FAST:
                    print get_varname(oparg[0]) + ' ' + binary_op[oparg[2]] + \
                        ' ' + get_varname(oparg[1])
                elif op == CONST_ADD_FAST:
                    print get_const(oparg[0]) + ' + ' + \
                        get_varname(oparg[1])
                elif op == CONST_BINOP_FAST:
                    print get_const(oparg[0]) + ' ' + binary_op[oparg[2]] + \
                        ' ' + get_varname(oparg[1])
                elif op == FAST_ADD_CONST:
                    print get_varname(oparg[0]) + ' + ' + \
                        get_const(oparg[1])
                elif op == FAST_BINOP_CONST:
                    print get_varname(oparg[0]) + ' ' + binary_op[oparg[2]] + \
                        ' ' + get_const(oparg[1])
                elif op == FAST_ADD_TO_FAST:
                    print '+ ' + get_varname(oparg[0]) + ' -> ' + \
                        get_varname(oparg[1])
                elif op == FAST_BINOP_TO_FAST:
                    print binary_op[oparg[1]] + ' ' + \
                        get_varname(oparg[0]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == CONST_ADD_TO_FAST:
                    print '+ ' + get_const(oparg[0]) + ' -> ' + \
                        get_varname(oparg[1])
                elif op == CONST_BINOP_TO_FAST:
                    print binary_op[oparg[1]] + ' ' + \
                        get_const(oparg[0]) + ' -> ' + \
                        get_varname(oparg[2])
                elif op == UNOP_TO_FAST:
                    print unary_op[oparg[0]] + ' -> ' + \
                        get_varname(oparg[1])
                elif op == BINOP_TO_FAST:
                    print binary_op[oparg[0]] + ' -> ' + \
                        get_varname(oparg[1])
                elif op == FAST_UNOP:
                    print unary_op[oparg[1]] + ' ' + get_varname(oparg[0])
                elif op == FAST_BINOP:
                    print binary_op[oparg[1]] + ' ' + get_varname(oparg[0])
                elif op == CONST_BINOP:
                    print binary_op[oparg[1]] + ' ' + get_const(oparg[0])
                elif op == LOAD_GLOBAL_ATTR:
                    print get_name(oparg[0]) + '.' + get_name(oparg[1])
                elif op == CALL_PROC_RETURN_CONST:
                    print str(oparg[0]) + '; RETURN ' + get_const(oparg[1])
                elif op == LOAD_GLOB_FAST_CALL_FUNC:
                    print get_name(oparg[0]) + '; ' + get_varname(oparg[1]) + \
                          '; CALL ' + str(oparg[2])
                elif op == FAST_ATTR_CALL_FUNC:
                    print get_varname(oparg[0]) + '.' + get_name(oparg[1]) + \
                          '()'
                elif op == FAST_ATTR_CALL_PROC:
                    print get_varname(oparg[0]) + '.' + get_name(oparg[1]) + \
                          '()'
            else:
                print repr(oparg).rjust(5),
                if op in hasconst:
                    print '(' + get_const(oparg) + ')',
                elif op in hasname:
                    print '(' + get_name(oparg) + ')',
                elif op in hasjrel:
                    print '(to ' + repr(offset + oparg) + ')',
                elif op in haslocal:
                    print '(' + get_varname(oparg) + ')',
                elif op in hasfree:
                    print '(' + get_free(oparg) + ')',
        else:
            print opname[op, oparg].ljust(30),
            if (op, oparg) in hascompare:
                print ' (' + cmp_op[oparg - hascompare[0][1]] + ')',
        print
    check_code_objects(code_objects)

def check_code_objects(code_objects):
    """Disassembles a list of code objects """
    for code_object in code_objects:
        print '\nDisassembling', code_object
        dis(code_object, True)

disco = disassemble                     # XXX For backwards compatibility

def findlabels(code):
    """Detect all offsets in a byte code which are jump targets.

    Return the list of offsets.

    """
    labels = []
    n = len(code)
    i = offset = 0
    while i < n:
        op = ord(code[i])
        oparg = ord(code[i + 1])
        i += 2
        offset += 1
        if op >= HAVE_ARGUMENT:
            op, oparg, size = get_extended_opcode(code, i, op, oparg)
            i += size + size
            offset += size
            label = -1
            if op in hasjrel:
                label = offset + oparg
            elif op in hasjabs:
                label = oparg
            if label >= 0:
                if label not in labels:
                    labels.append(label)
    return labels

def findlinestarts(code):
    """Find the offsets in a word code which are start of lines in the source.

    Generate pairs (offset, lineno) as described in Python/compile.c.

    """
    byte_increments = [ord(c) for c in code.co_lnotab[0::2]]
    line_increments = [ord(c) for c in code.co_lnotab[1::2]]

    lastlineno = None
    lineno = code.co_firstlineno
    addr = 0
    for byte_incr, line_incr in zip(byte_increments, line_increments):
        if byte_incr:
            if lineno != lastlineno:
                yield (addr, lineno)
                lastlineno = lineno
            addr += byte_incr
        lineno += line_incr
    if lineno != lastlineno:
        yield (addr, lineno)

def _test():
    """Simple test program to disassemble a file."""
    if sys.argv[1:]:
        if sys.argv[2:]:
            sys.stderr.write("usage: python dis.py [-|file]\n")
            sys.exit(2)
        fn = sys.argv[1]
        if not fn or fn == "-":
            fn = None
    else:
        fn = None
    if fn is None:
        f = sys.stdin
    else:
        f = open(fn)
    source = f.read()
    if fn is not None:
        f.close()
    else:
        fn = "<stdin>"
    code = compile(source, fn, "exec")
    dis(code)

if __name__ == "__main__":
    _test()
