# TSTK

## Authors

- Taran Lynn

## Description

TSTK is a stack based programming language. Comments start with \# and extend to
the end of the line. All numbers are either arbitrary precision integers or have
a length equal to the current architectures bus width. Every token (see EBNF) is
indexed starting from zero as far as jumps are concerned.

##### EBNF

```
program = token program | token ;

token = command | integer | label | reference ;

label = ':', {alpha}, ':' ;

reference = '@', {alpha} ;

command = {alpha} ;

integer = '-'?, {digit} ;

alpha = upper | lower ;

lower = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l'
            | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w'
            | 'x' | 'y' | 'z' ;

upper = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L'
            | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W'
            | 'X' | 'Y' | 'Z' ;

digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
```

## Commands

The following commands are recognized.

*integer* - Pushes an integer onto the stack.

:*name*: - Labels the current position with *name*. Multiple occurences of the
same label is an error.

@*name* - Pushes the position of the label *name* onto the stack. Note that this
can be placed before the corresponding :*name*:.

add - Pops two numbers off the stack, adds them, and pushes the result.

sub - Pops two numbers off the stack, subtracts the second number from the first
number, and pushes the result.

mul - Pops two numbers off the stack, multiplies them, and pushes the result.

div - Pops two numbers off the stack, floor divides the second number by the
first number, and pushes the result.

dup - Duplicates the number on the top of the stack.

jmp - Pops a number off the stack and goes to that token in the program (indexed
from 0).

jeq - Pops three numbers of the stack, tests if first two are equal, and jumps
to the third one if they are.

jnq - Pops three numbers of the stack, tests if first two are equal, and jumps
to the third one if they aren't.

jgt - Pops three numbers of the stack, tests if the second is greater than the
first, and jumps to the third one if they are.

jlt - Pops three numbers of the stack, tests if the second is less than the
first, and jumps to the third one if they are.

nth - Pops a number n off the stack, duplicates the value of the nth number from
the top of the stack (counting from 0), and pushes that value on the stack.

pop - Pops the top of the stack.

ppos - Pushes the current memory location onto the stack.

print - Pops a number and prints it.

cprint - Pops a number and prints a unicode character with its value.

read - Read a number and pushes it onto the stack.

cread - Reads a unicode character and pushes its value onto the stack.

size - Pushes the size of the stack onto the stack.

swap - Swaps the two numbers on the top of the stack.

## Goals

TSTK was designed to be one of the simplest **usuable** programming
language. The usuable part of TSTK is why I developed a new language instead of
using one like brain\*\*\*\*.

Wishlist: