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

The following commands are recognized. I'll use stack notation `...,a,b` where
values to the right are on the top of the stack, and `->` to  denote the
transformation.

*integer* - Pushes an integer onto the stack. `... -> ...,n`

:*name*: - Labels the current position with *name*. Multiple occurences of the
same label is an error. `... -> ...`

@*name* - Pushes the position of the label *name* onto the stack. Note that this
can be placed before the corresponding :*name*:. `... -> ...,addr`

add - Pops two numbers off the stack, adds them, and pushes the result.
`...,a,b -> ...,(a+b)`

sub - Pops two numbers off the stack, subtracts the second number from the first
number, and pushes the result.
`...,a,b -> ...,(a-b)`

mul - Pops two numbers off the stack, multiplies them, and pushes the result.
`...,a,b -> ...,(a*b)`

div - Pops two numbers off the stack, floor divides the second number by the
first number, and pushes the result.
`...,a,b -> ...,(a//b)`

dup - Duplicates the number on the top of the stack. `...,a -> ...,a,a`

jmp - Pops a number off the stack and goes to that operation in the program.
Operations are indexed from 0 and exclude labels but not references.
`...,addr -> ...`

jeq - Pops three numbers of the stack, tests if last two are equal, and jumps
to the first one if they are. `...,a,b,addr -> ...` if `a = b`

jnq - Pops three numbers of the stack, tests if last two are equal, and jumps
to the first one if they aren't. `...,a,b,addr -> ...` if `a != b`

jgt - Pops three numbers of the stack, tests if the third is greater than the
second, and jumps to the first one if they are. `...,a,b,addr -> ...` if `a > b`

jlt - Pops three numbers of the stack, tests if the third is less than the
second, and jumps to the first one if they are. `...,a,b,addr -> ...` if `a < b`

get - Pops a number n off the stack, duplicates the value of the nth number from
the top of the stack (counting from 0), and pushes that value on the stack.
`...,a,...,n -> ...,a,...,a`

set - Pops two numbers off of the stack, and stores second number at a ways down
the stack indexed by the first number (from 0). `...,a,...,b,n -> ...,b,...`

pop - Pops the top of the stack. `...,a -> ...`

ppos - Pushes the current memory location onto the stack. `... -> ...,addr`

print - Pops a number and prints it. `...,a -> ...`

cprint - Pops a number and prints a unicode character with its value.
`...,a -> ...`

read - Read a number and pushes it onto the stack. `...,a -> ...`

cread - Reads a unicode character and pushes its value onto the stack.
`...,a -> ...`

size - Pushes the size of the stack onto the stack. `... -> ...,size`

swap - Swaps the two numbers on the top of the stack. `...,a,b -> ...,b,a`

dbg - Prints the entire stack (useful for debugging). Optional for some
implementations. `... -> ...`

## Goals

TSTK was designed be a simple, abstract, stack-based, and assembly-like
language. I found that I rather liked assembly as a language, so I wanted to
create a conceptually simple and architecture independent language that captured
some of the features I liked about it.

TODO:

- Rewrite parser to use a lexer.