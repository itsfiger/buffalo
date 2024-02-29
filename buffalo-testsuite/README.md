# buffalo-testsuite
Test Suite for buffalo - an LALR(1) and canonical LR(1) parser generator for Common Lisp

Different tests from the GNU Bison Test suite and a converter from GNU Bison XML grammar output to buffalo's format.

The tests include
- LALR(1) and LR(1) generator
- global associativity and global and local precedence
- unused rules/nonteminals/terminals/states due to conflict resolution
- ambiguous grammars
- production grammars for GNU pic, GNU Cim and GNU awk
