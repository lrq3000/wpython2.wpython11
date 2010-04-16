"""Disassembler of Python byte code into mnemonics."""

import sys
import types

from opcode import *
from opcode import __all__ as _opcodes_all

__all__ = ["dis","disassemble","distb","disco",
           "get_opcode_info"] + _opcodes_all
del _opcodes_all

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

def get_opcode_info(code, i, op, oparg):
    "Returns the real opcode, arguments, and additional wordsize "
    if op > EXTENDED_ARG16:
        if op < EXTENDED_ARG32:
            return op, (oparg, ord(code[i]), ord(code[i + 1])), 1
        elif op == EXTENDED_ARG32:
            return oparg, (ord(code[i]) + ord(code[i + 1]) * 256 + \
                ord(code[i + 2]) * 65536 + ord(code[i + 3]) * 16777216, ), 2
        else:
            return op, (oparg, ord(code[i]), ord(code[i + 1]),
                ord(code[i + 2]), ord(code[i + 3])), 2
    elif op == EXTENDED_ARG16:
        return oparg, (ord(code[i]) + ord(code[i + 1]) * 256, ), 1
    else:
        return op, (oparg, ), 0

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

    def eval_arg_not_used(arg):
        return ''

    def eval_arg_int(arg):
        return str(arg)

    def eval_arg_const(arg):
        if constants:
            const = constants[arg]
            if deep and type(const) in codeobject_types:
              code_objects.append(const)
            return '%d (%r)' % (arg, const)
        else:
            return '(%d)' % arg

    def eval_arg_local(arg):
        if varnames is not None:
            local = varnames[arg]
            return '%d (%s)' % (arg, local)
        else:
            return '(%d)' % arg

    def eval_arg_free(arg):
        if frees is not None:
            free = frees[arg]
            return '%d (%s)' % (arg, free)
        else:
            return '(%d)' % arg

    def eval_arg_name(arg):
        if names is not None:
            name = names[arg]
            return '%d (%s)' % (arg, name)
        else:
            return '(%d)' % arg

    def eval_arg_quick_func(arg):
        return '%d (%d %d)' % (arg, arg & 15, arg >> 4)

    def eval_arg_jrel(arg):
        return '%d (to %d)' % (arg, offset + arg)

    def eval_arg_jabs(arg):
        return repr(arg)

    def eval_arg_unary(arg):
        return '%d (%s)' % (arg, unary_op[arg])

    def eval_arg_binary(arg):
        return '%d (%s)' % (arg, binary_op[arg])

    def eval_arg_ternary(arg):
        return '%d (%s)' % (arg, ternary_op[arg])

    evaluators = (eval_arg_not_used, eval_arg_int, eval_arg_const,
        eval_arg_local, eval_arg_free, eval_arg_name,
        eval_arg_quick_func, eval_arg_jrel, eval_arg_jabs,
        eval_arg_unary, eval_arg_binary, eval_arg_ternary)

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
        op, args, size = get_opcode_info(code, i, op, oparg)
        i += size + size
        offset += size
        key = op, args[0]
        name = opname.get(key, None)
        if name is None:
            key = op
            name = opname[key]
        str_args_list = []
        append = str_args_list.append
        for arg, info in zip(args, opargs[key]):
            if info != arg_not_used:
                append(evaluators[info](arg))
        print name.ljust(30), ' '.join(str_args_list)
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
    labels = set()
    add = labels.add
    n = len(code)
    i = offset = 0
    while i < n:
        op = ord(code[i])
        oparg = ord(code[i + 1])
        i += 2
        offset += 1
        if op >= HAVE_ARGUMENT:
            op, args, size = get_opcode_info(code, i, op, oparg)
            i += size + size
            offset += size
            for arg, info in zip(args, opargs[op]):
                if info == arg_jrel:
                    add(offset + arg)
                elif info == arg_jabs:
                    add(arg)
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
