from __future__ import with_statement
import sys, types, os, traceback
from opcode import *

opt_print = False

op_args = [[0, 0, 0, 0] for i in xrange(256)]
op_lens = [[0, 0, 0, 0] for i in xrange(256)]
op_maxarg = [0] * 256
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
    global penultimate_op, last_op
    penultimate_op = last_op = 0
    code = co.co_code
    labels = findlabels(code)
    linestarts = dict(findlinestarts(co))
    n = len(code)
    i = oparg = extended_arg = extended_len = 0
    free = None
    code_objects = []
    while i < n:
        c = code[i]
        op_len = 1 + extended_len
        op = ord(c)
        if i in linestarts:
            if i > 0:
                p()
            pc("%3d" % linestarts[i],)
        else:
            pc('   ',)

        if i == lasti: pc('-->',)
        else: pc('   ',)
        if i in labels: pc('>>',)
        else: pc('  ',)
        pc(repr(i).rjust(4),)
        pc(opname[op].ljust(25),)
        i = i+1
        if op >= HAVE_ARGUMENT:
            op_len += 2
            pc('len', op_len,)
            oparg = ord(code[i]) + (ord(code[i+1]) << 8) + extended_arg
            extended_arg = 0
            extended_len = 0
            i = i+2
            if op == EXTENDED_ARG:
                extended_arg = oparg << 16
                extended_len = 3
            pc(repr(oparg).rjust(5),)
            if op in hasconst:
                const = co.co_consts[oparg]
                if type(const) in codeobject_types:
                  code_objects.append(const)
                pc('(' + repr(const) + ')',)
            elif op in hasname:
                pc('(' + co.co_names[oparg] + ')',)
            elif op in hasjrel:
                pc('(to ' + repr(i + oparg) + ')',)
            elif op in haslocal:
                pc('(' + co.co_varnames[oparg] + ')',)
            elif op in hascompare:
                pc('(' + cmp_op[oparg] + ')',)
            elif op in hasfree:
                if free is None:
                    free = co.co_cellvars + co.co_freevars
                pc('(' + free[oparg] + ')',)
        else:
            pc('len', op_len,)
            extended_len = 0
        p()
        update_stats(op, oparg, op_len)

    check_code_objects(code_objects)


def disassemble_string(code, lasti=-1, varnames=None, names=None,
                       constants=None):
    global penultimate_op, last_op
    penultimate_op = last_op = 0
    labels = findlabels(code)
    n = len(code)
    i = oparg = 0
    code_objects = []
    while i < n:
        c = code[i]
        op_len = 1
        op = ord(c)
        if i == lasti: pc('-->',)
        else: pc('   ',)
        if i in labels: pc('>>',)
        else: pc('  ',)
        pc(repr(i).rjust(4),)
        pc(opname[op].ljust(25),)
        i = i+1
        if op >= HAVE_ARGUMENT:
            op_len += 2
            pc('len', op_len,)
            oparg = ord(code[i]) + (ord(code[i+1]) << 8)
            i = i+2
            if op == EXTENDED_ARG:
              p('EXTENDED_ARG found in disassemble_string!!! Exiting')
              sys.exit(-1)
            pc(repr(oparg).rjust(5),)
            if op in hasconst:
                if constants:
                    const = constants[oparg]
                    if type(const) in codeobject_types:
                      code_objects.append(const)
                    pc('(' + repr(const) + ')',)
                else:
                    pc('(%d)' % oparg,)
            elif op in hasname:
                if names is not None:
                    pc('(' + names[oparg] + ')',)
                else:
                    pc('(%d)' % oparg,)
            elif op in hasjrel:
                pc('(to ' + repr(i + oparg) + ')',)
            elif op in haslocal:
                if varnames:
                    pc('(' + varnames[oparg] + ')',)
                else:
                    pc('(%d)' % oparg,)
            elif op in hascompare:
                pc('(' + cmp_op[oparg] + ')',)
        else:
            pc('len', op_len,)
        p()
        update_stats(op, oparg, op_len)

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
    i = 0
    while i < n:
        c = code[i]
        op = ord(c)
        i = i+1
        if op >= HAVE_ARGUMENT:
            oparg = ord(code[i]) + (ord(code[i+1]) << 8)
            i = i+2
            label = -1
            if op in hasjrel:
                label = i + oparg
            elif op in hasjabs:
                label = oparg
            if label >= 0:
                if label not in labels:
                    labels.append(label)
    return labels


def findlinestarts(code):
    """Find the offsets in a byte code which are start of lines in the source.

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

CALL_FUNCTION = opmap['CALL_FUNCTION']
CALL_FUNCTION_VAR = opmap['CALL_FUNCTION_VAR']
CALL_FUNCTION_KW = opmap['CALL_FUNCTION_KW']
CALL_FUNCTION_VAR_KW = opmap['CALL_FUNCTION_VAR_KW']
FUNCTIONS = frozenset((CALL_FUNCTION, CALL_FUNCTION_VAR, CALL_FUNCTION_KW, CALL_FUNCTION_VAR_KW))

def update_stats(op, oparg, op_len):
    global penultimate_op, last_op
    #pc('update_stats:', str(op).rjust(3), opname[op].ljust(25), str(oparg).rjust(8), op_len)
    offset = {1 : 0, 3 : 1, 4 : 2, 6 : 3}[op_len]
    op_lens[op][offset] += 1
    #pc('len offset:', offset, op_lens[op])

    if op >= HAVE_ARGUMENT:
      op_maxarg[op] = max(op_maxarg[op], oparg)
      if op in FUNCTIONS:
        low = oparg & 255
        high = (oparg >> 8) & 255
        if (low <= 15) and (high <= 15):
            offset = 1
        else:
            offset = 2
      else:
        if oparg >= 65536:
            offset = 3
        elif oparg >= 256:
            offset = 2
        else:
            offset = 1
    else:
        offset = 0
    op_args[op][offset] += 1
    #p('arg offset:', offset, op_args[op])
    singles[op] = singles.get(op, 0) + 1
    if last_op:
        code = (last_op << 8) + op
        pairs[code] = pairs.get(code, 0) + 1
        if penultimate_op:
            code += penultimate_op << 16
            triples[code] = triples.get(code, 0) + 1
    penultimate_op = last_op
    last_op = op


def display_stats():
    global opt_print
    opt_print = True
    p('\nDisplaying stats...')
    op_by_args = [0, 0, 0, 0, 0]
    op_by_lens = [0, 0, 0, 0, 0]
    for op in xrange(256):
        op_arg = op_args[op]
        total = sum(op_arg)
        if total:
            pc(str(op).rjust(4), opname[op].ljust(25),)
            op_arg.append(total)
            for i, (description, count) in enumerate(zip(('None:   ', '8 bits: ', '16 bits:', '32 bits:', 'total:  '), op_arg)):
                pc(description, str(count).rjust(8),)
                op_by_args[i] += count
            p()
            pc('                   opcode lens',)
            op_len = op_lens[op]
            total = sum(op_len)
            op_len.append(total)
            for i, (description, count) in enumerate(zip(('1 byte: ', '3 bytes:', '4 bytes:', '6 bytes:', 'total:  '), op_len)):
                pc(description, str(count).rjust(8),)
                op_by_lens[i] += count
            p()

    pc('\nArguments   counts.')
    for (description, count) in zip(('None:   ', '8 bits: ', '16 bits:', '32 bits:', 'total:  '), op_by_args):
        pc(description, str(count).rjust(8),)
    p()
    pc('Instructions bytes.')
    for (description, count) in zip(('1 byte: ', '3 bytes:', '4 bytes:', '6 bytes:', 'total:  '), op_by_lens):
        pc(description, str(count).rjust(8),)
    p()
    pc('Bytes        stats.')
    total = 0
    for (description, weight, count) in zip(('1 byte: ', '3 bytes:', '4 bytes:', '6 bytes:'), (1, 3, 4, 6), op_by_lens[ : -1]):
        op_bytes = weight * count
        total += op_bytes
        pc(description, str(op_bytes).rjust(8),)
    p('total:  ', str(total).rjust(8))
    pc('Wordcode     bytes.')
    op_by_args.pop()
    total = 0
    for (description, weight, count) in zip(('None:   ', '8 bits: ', '16 bits:', '32 bits:'), (2, 2, 4, 6), op_by_args):
        op_bytes = weight * count
        total += op_bytes
        pc(description, str(op_bytes).rjust(8),)
    p('total:  ', str(total).rjust(8))
    p()

    p('Most frequent opcodes with argument (displaying the maximum argument):')
    stats = sorted(((count, code) for code, count in singles.iteritems() if code >= HAVE_ARGUMENT), reverse = True)
    for count, code in stats:
        p(' ', opname[code].ljust(25), str(count).rjust(8), str(op_maxarg[code]).rjust(8))
    p()

    p('Most frequent opcodes without argument:')
    stats = sorted(((count, code) for code, count in singles.iteritems() if code < HAVE_ARGUMENT), reverse = True)
    for count, code in stats:
        p(' ', opname[code].ljust(25), str(count).rjust(8))
    p()

    p('Most frequent couples:')
    stats = sorted(((count, code) for code, count in pairs.iteritems()), reverse = True)
    for count, code in stats[ : 20]:
        op = code & 255
        last_op = code >> 8
        p(' ', opname[last_op].ljust(25), opname[op].ljust(25), str(count).rjust(8))
    p()

    p('Most frequent triples:')
    stats = sorted(((count, code) for code, count in triples.iteritems()), reverse = True)
    for count, code in stats[ : 20]:
        op = code & 255
        last_op = (code >> 8) & 255
        penultimate_op = code >> 16
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
