# Minimal tests for dis module

from test.test_support import run_unittest
import unittest
import sys
import dis
import StringIO


def _f(a):
    print a
    return 1

dis_f = """\
 %-4d         0 LOAD_FAST                     0 (a)
              1 PRINT_ITEM
              2 PRINT_NEWLINE

 %-4d         3 RETURN_CONST                  1 (1)
"""%(_f.func_code.co_firstlineno + 1,
     _f.func_code.co_firstlineno + 2)


def bug708901():
    for res in range(1,
                     10):
        pass

dis_bug708901 = """\
 %-4d         0 LOAD_GLOBAL                   0 (range)
              1 LOAD_CONSTS                   1 ((1, 10))
              2 CALL_FUNCTION                 2
              3 GET_ITER
        >>    4 FOR_ITER                      2 (to 7)
              5 STORE_FAST                    0 (res)

 %-4d         6 JUMP_ABSOLUTE                 4
        >>    7 RETURN_CONST                  0 (None)
"""%(bug708901.func_code.co_firstlineno + 1,
     bug708901.func_code.co_firstlineno + 3)


def bug1333982(x=[]):
    assert 0, ([s for s in x] +
              1)
    pass

dis_bug1333982 = """\
 %-4d         0 LOAD_CONST                    1 (0)
              1 JUMP_IF_TRUE                 14 (to 16)
              2 LOAD_GLOBAL                   0 (AssertionError)
              3 BUILD_LIST                    0
              4 DUP_TOP
              5 STORE_FAST                    1 (_[1])
              6 FAST_UNOP                       get_iter x

        >>    8 FOR_ITER                      4 (to 13)
              9 STORE_FAST                    2 (s)
             10 LOAD_FAST                     1 (_[1])
             11 LOAD_FAST                     2 (s)
             12 LIST_APPEND_LOOP              8
        >>   13 DELETE_FAST                   1 (_[1])

 %-4d        14 CONST_ADD                     2 (1)
             15 RAISE_2

 %-4d   >>   16 RETURN_CONST                  0 (None)
"""%(bug1333982.func_code.co_firstlineno + 1,
     bug1333982.func_code.co_firstlineno + 2,
     bug1333982.func_code.co_firstlineno + 3)

_BIG_LINENO_FORMAT_PEEPHOLE = """\
%3d           0 LOAD_GLOBAL                   0 (spam)
              1 POP_TOP
              2 RETURN_CONST                  0 (None)
"""

_BIG_LINENO_FORMAT_NO_PEEPHOLE = """\
%3d           0 LOAD_GLOBAL                   0 (spam)
              1 POP_TOP
              2 LOAD_CONST                    0 (None)
              3 RETURN_VALUE
"""

class DisTests(unittest.TestCase):
    def do_disassembly_test(self, func, expected):
        s = StringIO.StringIO()
        save_stdout = sys.stdout
        sys.stdout = s
        dis.dis(func)
        sys.stdout = save_stdout
        got = s.getvalue()
        # Trim trailing blanks (if any).
        lines = got.split('\n')
        lines = [line.rstrip() for line in lines]
        expected = expected.split("\n")
        import difflib
        if expected != lines:
            self.fail(
                "events did not match expectation:\n" +
                "\n".join(difflib.ndiff(expected,
                                        lines)))

    def test_opmap(self):
        self.assertEqual(dis.opmap["STOP_CODE"], (5, 1))
        self.assertEqual(dis.opmap["LOAD_FAST"], 7)
        self.assertEqual(dis.opmap["LOAD_CONST"] in dis.hasconst, True)
        self.assertEqual(dis.opmap["STORE_NAME"] in dis.hasname, True)

    def test_opname(self):
        self.assertEqual(dis.opname[dis.opmap["LOAD_FAST"]], "LOAD_FAST")

    def test_boundaries(self):
        self.assertEqual(dis.opmap["EXTENDED_ARG16"], dis.EXTENDED_ARG16)
        self.assertEqual(dis.opmap["EXTENDED_ARG32"], dis.EXTENDED_ARG32)
        self.assertEqual(dis.opmap["EXTENDED_ARG32"] + 1, dis.TOTAL_OPCODES)

    def test_dis(self):
        self.do_disassembly_test(_f, dis_f)

    def test_bug_708901(self):
        self.do_disassembly_test(bug708901, dis_bug708901)

    def test_bug_1333982(self):
        # This one is checking bytecodes generated for an `assert` statement,
        # so fails if the tests are run with -O.  Skip this test then.
        if __debug__:
            self.do_disassembly_test(bug1333982, dis_bug1333982)

    def test_big_linenos(self):
        def func(count):
            namespace = {}
            func = "def foo():\n " + "".join(["\n "] * count + ["spam\n"])
            exec func in namespace
            return namespace['foo']

        # Test all small ranges
        for i in xrange(1, 254):
            expected = _BIG_LINENO_FORMAT_PEEPHOLE % (i + 2)
            self.do_disassembly_test(func(i), expected)
        for i in xrange(254, 300):
            expected = _BIG_LINENO_FORMAT_NO_PEEPHOLE % (i + 2)
            self.do_disassembly_test(func(i), expected)

        # Test some larger ranges too
        for i in xrange(300, 5000, 10):
            expected = _BIG_LINENO_FORMAT_NO_PEEPHOLE % (i + 2)
            self.do_disassembly_test(func(i), expected)

def test_main():
    run_unittest(DisTests)


if __name__ == "__main__":
    test_main()
