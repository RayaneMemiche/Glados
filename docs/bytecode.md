# Introduction

This document provides all the information about the bytecodes and the operands of the GLaDOS Virtual Machine.

## Instructions Set

[bytecode]  \[name\][arguments]      [description]

00      NOOP            (size = 1)

01      PUSH [i32]     (size = 5, add 1 value to the stack)

02      CALL [u32]      (size = 5)

03      CALLP [u8]      (size = 2)

04      RET             (size = 1)

05      JR [i32]        (size = 5)

06      JRZ [i32]       (size = 5)

07      STORE [u32]     (size = 5, take 1 value from the stack)

08      LOAD [u32]      (size = 5)

09      POP             (size = 1, take 1 value from the stack)

0a      DUP             (size = 1)

## Primitive functions

[bytecode]  [name]      [arguments -- return]

00      ADD             (x y -- z)

01      SUB             (x y -- z)

02      MUL             (x y -- z)

03      DIV             (x y -- z)

04      MOD             (x y -- z)


05      EQ              (x y -- z)

06      NEQ             (x y -- z)

07      LT              (x y -- z)

08      GT              (x y -- z)

09      LTE             (x y -- z)

0A      GTE             (x y -- z)


0B      DISP            (x -- x)


0C      AND             (x y -- z)

0D      OR              (x y -- z)

0E      XOR             (x y -- z)
