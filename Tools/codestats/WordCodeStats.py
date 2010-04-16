from __future__ import with_statement
import sys
import types
import os
import traceback
from opcode import *
from dis import get_opcode_info, findlabels, findlinestarts

opt_print = False

CALL_SUB = opmap['CALL_SUB']

op_args = {}
op_maxarg = {}
for op in xrange(256):
  op_args[op] = [0, 0, 0, 0]
  op_maxarg[op] = 0
  for i in xrange(6):
    op_args[i, op] = [0, 0, 0, 0]
  op_args[CALL_SUB, op] = [0, 0, 0, 0]
singles = {}
pairs = {}
triples = {}
max_code_len = 0
max_code_lens_info = []
code_len_stats = {}
max_stack_usage = 0
max_stack_usage_info = []
max_const = max_local = max_free = max_name = max_jrel = max_jabs = 0
int_stats = {}
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
    common_disassemble(co, co.co_code, lasti, True, dict(findlinestarts(co)),
                       co.co_varnames, co.co_names, co.co_consts,
                       co.co_cellvars + co.co_freevars)

def disassemble_string(code, lasti=-1, varnames=None, names=None,
                       constants=None):
    common_disassemble(None, code, lasti, True, {}, varnames, names, constants, None)


def common_disassemble(co, code, lasti, deep, linestarts,
                       varnames, names, constants, frees):
    """Disassemble a code object."""

    global next_to_last_op, last_op, max_code_len, max_stack_usage

    def eval_arg_not_used(arg):
        return ''

    def eval_arg_int(arg):
        return str(arg)

    def eval_arg_const(arg):
        global max_const
        if arg > max_const:
            max_const = arg
        if constants:
            const = constants[arg]
            if deep and type(const) in codeobject_types:
              code_objects.append(const)
            if isinstance(const, int):
                if 0 <= const <= 255:
                    int_stats[const] = int_stats.get(const, 0) + 1
            return '%d (%r)' % (arg, const)
        else:
            return '(%d)' % arg

    def eval_arg_local(arg):
        global max_local
        if arg > max_local:
            max_local = arg
        if varnames is not None:
            local = varnames[arg]
            return '%d (%s)' % (arg, local)
        else:
            return '(%d)' % arg

    def eval_arg_free(arg):
        global max_free
        if arg > max_free:
            max_free = arg
        if frees is not None:
            free = frees[arg]
            return '%d (%s)' % (arg, free)
        else:
            return '(%d)' % arg

    def eval_arg_name(arg):
        global max_name
        if arg > max_name:
            max_name = arg
        if names is not None:
            name = names[arg]
            return '%d (%s)' % (arg, name)
        else:
            return '(%d)' % arg

    def eval_arg_quick_func(arg):
        return '%d (%d %d)' % (arg, arg & 15, arg >> 4)

    def eval_arg_jrel(arg):
        global max_jrel
        if arg > max_jrel:
            max_jrel = arg
        return '%d (to %d)' % (arg, offset + arg)

    def eval_arg_jabs(arg):
        global max_jabs
        if arg > max_jabs:
            max_jabs = arg
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

    n = len(code)
    if n > max_code_len:
      max_code_len = n
      if co:
        max_code_lens_info.append((n, co.co_name, co.co_filename, co.co_firstlineno))
    code_len_stats[n >> 7] = code_len_stats.get(n >> 7, 0) + 1
    if co and max_stack_usage < co.co_stacksize:
      max_stack_usage = co.co_stacksize
      max_stack_usage_info.append((max_stack_usage, co.co_name, co.co_filename, co.co_firstlineno))
    next_to_last_op = last_op = 0
    labels = findlabels(code)
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
        p(name.ljust(30), ' '.join(str_args_list))
        update_stats(op, args[0], size + (op >= HAVE_ARGUMENT))

    check_code_objects(code_objects)


def check_code_objects(code_objects):
    if code_objects:
        p('FOUND', len(code_objects))
        for code_object in code_objects:
          p('Disassembling', code_object)
          dis(code_object)


def update_stats(op, oparg, offset):
    global next_to_last_op, last_op
    #pc('update_stats:', str(op).rjust(3), opname[op].ljust(25), str(oparg).rjust(8), offset)

    if op < HAVE_ARGUMENT or op == CALL_SUB:
      op = op, oparg
    else:
      op_maxarg[op] = max(op_maxarg[op], oparg)

    op_args[op][offset] += 1
    #p('arg offset:', offset, op_args[op])
    singles[op] = singles.get(op, 0) + 1
    if last_op:
        code = last_op, op
        pairs[code] = pairs.get(code, 0) + 1
        if next_to_last_op:
            code = next_to_last_op, last_op, op
            triples[code] = triples.get(code, 0) + 1
    next_to_last_op = last_op
    last_op = op


def display_stats():

    def display_opcode_stats(op):
        op_arg = op_args[op]
        total = sum(op_arg)
        if total:
            if isinstance(op, tuple):
                if op[0] == CALL_SUB:
                    wordcode = str(CALL_SUB) + ', ' + str(op[1])
                else:
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
    for oparg in xrange(256):
      display_opcode_stats((CALL_SUB, oparg))
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

    p('Max code len:', (max_code_len + 1) >> 1, 'words,', max_code_len, 'bytes.')
    for n, name, filename, lineno in max_code_lens_info:
        print '  Code length', n, 'FOR', name, 'IN', filename, 'AT', lineno, 'line.'

    p(' Words  Bytes    Count')
    for code_len in sorted(code_len_stats):
        p(str(code_len << 6).rjust(6), str(code_len << 7).rjust(6), str(code_len_stats[code_len]).rjust(8))

    p('Max StackUsage:', max_stack_usage)
    for n, name, filename, lineno in max_stack_usage_info:
        print '  Stack usage', n, 'FOR', name, 'IN', filename, 'AT', lineno, 'line.'

    p('max_const:', max_const, 'max_local:', max_local, 'max_free:', max_free, 'max_name:', max_name, 'max_jrel:', max_jrel, 'max_jabs:', max_jabs)

    p('small ints stats:')
    n = 0
    for i in sorted(int_stats):
        pc(str(i).rjust(4) + ':', str(int_stats[i]).rjust(8), ' ')
        n += 1
        if not(n & 7):
          p()
    if n & 7:
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
        next_to_last_op, last_op, op = code
        p(' ', opname[next_to_last_op].ljust(25), opname[last_op].ljust(25), opname[op].ljust(25), str(count).rjust(8))
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
