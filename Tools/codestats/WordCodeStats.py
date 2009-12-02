from __future__ import with_statement
import sys, types, os, traceback
from opcode import *
from dis import get_extended_opcode

MOVE_FAST_FAST = opmap['MOVE_FAST_FAST']
MOVE_CONST_FAST = opmap['MOVE_CONST_FAST']
MOVE_GLOBAL_FAST = opmap['MOVE_GLOBAL_FAST']
MOVE_FAST_ATTR_FAST = opmap['MOVE_FAST_ATTR_FAST']
MOVE_FAST_FAST_ATTR = opmap['MOVE_FAST_FAST_ATTR']
MOVE_CONST_FAST_ATTR = opmap['MOVE_CONST_FAST_ATTR']
LOAD_FAST_ATTR = opmap['LOAD_FAST_ATTR']

opt_print = False

op_args = {}
op_maxarg = {}
for op in xrange(256):
  op_args[op] = [0, 0, 0, 0]
  op_maxarg[op] = 0
  for i in xrange(6):
    op_args[i, op] = [0, 0, 0, 0]
singles = {}
pairs = {}
triples = {}


codeobject_types = frozenset((types.MethodType,
                    types.FunctionType,
                    types.CodeType,
                    types.ClassType))


def get_trace_back():
  'Get the last traceback message'
  return ''.join(traceback.format_exception(sys.exc_type, sys.exc_value, sys.exc_traceback))


def p(*args):
    if opt_print:
        print ' '.join(str(arg) for arg in args)


def pc(*args):
    if opt_print:
        print ' '.join(str(arg) for arg in args),


def dis(x):
    """Disassemble classes, methods, functions, or code.

    With no argument, disassemble the last traceback.

    """
    if type(x) is types.InstanceType:
        p('InstanceType!')
        x = x.__class__
    if hasattr(x, 'Has im_func'):
        p('im_func!')
        x = x.im_func
    if hasattr(x, 'func_code'):
        p('Has func_code!')
        x = x.func_code
    if hasattr(x, '__dict__'):
        p('Has __dict__!')
        items = x.__dict__.items()
        items.sort()
        for name, x1 in items:
            if type(x1) in codeobject_types:
                p('Got', str(type(x1)) + '!')
                p("Disassembly of %s:" % name)
                try:
                    dis(x1)
                except TypeError, msg:
                    p("Sorry:", msg)
                p()
    elif hasattr(x, 'co_code'):
        p('Has co_code!')
        disassemble(x)
    elif isinstance(x, str):
        p('Was str!')
        disassemble_string(x)
    else:
        raise TypeError, \
              "don't know how to disassemble %s objects" % \
              type(x).__name__


def disassemble(co, lasti=-1):
    """Disassemble a code object."""
    common_disassemble(co.co_code, lasti, True, dict(findlinestarts(co)),
                       co.co_varnames, co.co_names, co.co_consts,
                       co.co_cellvars + co.co_freevars)

def disassemble_string(code, lasti=-1, varnames=None, names=None,
                       constants=None):
    common_disassemble(code, lasti, True, {}, varnames, names, constants, None)


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

    global penultimate_op, last_op
    penultimate_op = last_op = 0
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
                p()
            pc("%3d" % linestarts[offset],)
        else:
            pc('   ',)

        if offset == lasti: pc('-->',)
        else: pc('   ',)
        if offset in labels: pc('>>',)
        else: pc('  ',)
        pc(repr(offset).rjust(4),)
        offset += 1
        if op >= HAVE_ARGUMENT:
            op, oparg, size = get_extended_opcode(code, i, op, oparg)
            i += size + size
            offset += size
            pc('len', size,)
            pc(opname[op].ljust(25),)
            if EXTENDED_ARG16 < op < EXTENDED_ARG32:
                pc(''.rjust(5),)
                if op == MOVE_FAST_FAST:
                    p(get_varname(oparg[0]) + ' -> ' + \
                        get_varname(oparg[1]))
                elif op == MOVE_CONST_FAST:
                    p(get_const(oparg[0]) + ' -> ' + \
                            get_varname(oparg[1]))
                elif op == MOVE_GLOBAL_FAST:
                    p(get_name(oparg[0]) + ' -> ' + \
                        get_varname(oparg[1]))
                elif op == MOVE_FAST_ATTR_FAST:
                    p(get_varname(oparg[0]) + '.' + get_name(oparg[1]) + \
                        ' -> ' + get_varname(oparg[2]))
                elif op == MOVE_FAST_FAST_ATTR:
                    p(get_varname(oparg[0]) + ' -> ' + \
                          get_varname(oparg[1]) + '.' + get_name(oparg[2]))
                elif op == MOVE_CONST_FAST_ATTR:
                    p(get_const(oparg[0]) + ' -> ' + \
                          get_varname(oparg[1]) + '.' + get_name(oparg[2]))
                elif op == LOAD_FAST_ATTR:
                    p(get_varname(oparg[0]) + '.' + get_name(oparg[1]))
            else:
              pc(repr(oparg).rjust(5),)
              if op in hasconst:
                  pc('(' + get_const(oparg) + ')',)
              elif op in hasname:
                  pc('(' + get_name(oparg) + ')',)
              elif op in hasjrel:
                  pc('(to ' + repr(offset + oparg) + ')',)
              elif op in haslocal:
                  pc('(' + get_varname(oparg) + ')',)
              elif op in hascompare:
                  pc('(' + cmp_op[oparg] + ')',)
              elif op in hasfree:
                  pc('(' + get_free(oparg) + ')',)
            size += 1
        else:
            size = 0
            pc('len', 1,)
            pc(opname[op, oparg].ljust(30),)
            if (op, oparg) in hascompare:
                pc(' (' + cmp_op[oparg - hascompare[0][1]] + ')',)
        p()
        update_stats(op, oparg, size)

    check_code_objects(code_objects)



def check_code_objects(code_objects):
    if code_objects:
        p('FOUND', len(code_objects))
        for code_object in code_objects:
          p('Disassembling', code_object)
          dis(code_object)


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

def update_stats(op, oparg, offset):
    global penultimate_op, last_op
    #pc('update_stats:', str(op).rjust(3), opname[op].ljust(25), str(oparg).rjust(8), offset)

    if op >= HAVE_ARGUMENT:
      op_maxarg[op] = max(op_maxarg[op], oparg)
    else:
      op = op, oparg

    op_args[op][offset] += 1
    #p('arg offset:', offset, op_args[op])
    singles[op] = singles.get(op, 0) + 1
    if last_op:
        code = last_op, op
        pairs[code] = pairs.get(code, 0) + 1
        if penultimate_op:
            code = penultimate_op, last_op, op
            triples[code] = triples.get(code, 0) + 1
    penultimate_op = last_op
    last_op = op


def display_stats():

    def display_opcode_stats(op):
        op_arg = op_args[op]
        total = sum(op_arg)
        if total:
            if isinstance(op, tuple):
                wordcode = str(op[0]) + ',' + str(op[1]).rjust(3)
            else:
                wordcode = str(op)
            pc(wordcode.rjust(6), opname[op].ljust(25),)
            op_arg.append(total)
            for i, (description, count) in enumerate(zip(('None:   ', '8 bits: ', '16 bits:', '32 bits:', 'total:  '), op_arg)):
                pc(description, str(count).rjust(8),)
                op_by_args[i] += count
            p()


    global opt_print
    opt_print = True
    p('\nDisplaying stats...')
    op_by_args = [0, 0, 0, 0, 0]
    for op in xrange(256):
      display_opcode_stats(op)
    for op in xrange(6):
      for oparg in xrange(256):
        display_opcode_stats((op, oparg))
    pc('\nArguments   counts.')
    for (description, count) in zip(('None:   ', '8 bits: ', '16 bits:', '32 bits:', 'total:  '), op_by_args):
        pc(description, str(count).rjust(8),)
    p()
    pc('Bytes        stats.')
    op_by_lens = [op_by_args[0] * 2, op_by_args[1] * 2, op_by_args[2] * 4, op_by_args[3] * 6]
    total = sum(op_by_lens)
    op_by_lens.append(total)
    for (description, count) in zip(('2 bytes:', '2 bytes:', '4 bytes:', '6 bytes:', 'total:  '), op_by_lens):
        pc(description, str(count).rjust(8),)
    p()

    p('Most frequent opcodes with argument (displaying the maximum argument):')
    stats = sorted(((count, code) for code, count in singles.iteritems() if not isinstance(code, tuple)), reverse = True)
    for count, code in stats:
        p(' ', opname[code].ljust(25), str(count).rjust(8), str(op_maxarg[code]).rjust(8))
    p()

    p('Most frequent opcodes without argument:')
    stats = sorted(((count, code) for code, count in singles.iteritems() if isinstance(code, tuple)), reverse = True)
    for count, code in stats:
        p(' ', opname[code].ljust(25), str(count).rjust(8))
    p()

    p('Most frequent couples:')
    stats = sorted(((count, code) for code, count in pairs.iteritems()), reverse = True)
    for count, code in stats[ : 20]:
        last_op, op = code
        p(' ', opname[last_op].ljust(25), opname[op].ljust(25), str(count).rjust(8))
    p()

    p('Most frequent triples:')
    stats = sorted(((count, code) for code, count in triples.iteritems()), reverse = True)
    for count, code in stats[ : 20]:
        penultimate_op, last_op, op = code 
        p(' ', opname[penultimate_op].ljust(25), opname[last_op].ljust(25), opname[op].ljust(25), str(count).rjust(8))
    p()


def load_py(filename):
    print 'Processing', filename + '...'
    with open(filename) as f:
        source = f.read()
    try:
      code_object = compile(source, filename, 'exec')
    except SyntaxError:
      print get_trace_back()
      print filename, 'skipped!'
      code_object = None
    if code_object:
      dis(code_object)


def scan_dir(path):
    print 'Scanning dir', path + '...'
    for root, dirs, files in os.walk(path, topdown = False):
        for name in files:
            if name.endswith('.py'):
                load_py(os.path.join(root, name))


def print_usage():
    print 'Usage:', sys.argv, '[-print] FileOrDir1 FileOrDir2 ... FileOrDirn'


args = sys.argv[1 : ]
if args:
    if args[0] == '-print':
        opt_print = True
        args.pop(0)

if not args:
    python_path = os.path.dirname(sys.executable)
    head, tail = os.path.split(python_path)
    if not tail.lower().startswith('python'):
        python_path = head
    print 'Getting directories from', python_path
    args = os.path.join(python_path, 'Lib'), os.path.join(python_path, 'Tools')
for arg in args:
    if arg.endswith('.py'):
        load_py(arg)
    else:
        scan_dir(arg)
display_stats()
