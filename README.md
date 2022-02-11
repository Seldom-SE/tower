# T
# o
# w
# e
# r

[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](./LICENSE)

## What is...it?

It's a esolang with painfully vertical data storage. You only get 3 registers to work with, though you can compress the registers into archives, but an archive takes a register, and it's difficult to avoid overwriting your own data when extracting them.

It's still a work-in-progress while I figure out whether it's powerful enough to solve complex problems. I think it's almost done, though.

Yes, the name is written vertically like that. I'm avoiding writing it as much as I can.

## Syntax

### Data Types

There are two data types: numbers and archives. Numbers can also be treated as UTF-8 characters in certain statements and expressions. Numbers are stored as 32-bit signed integers. Nonzero numbers are truthy, and zero is falsy.

Archives have up to three registers of their own, each of which may contain a number or another archive. All archives are truthy.

### Statements

Any character tries to be a statement by default. A symbol only becomes an expression if a previous statement or expression coerces it into one. Later characters have coercion priority over earlier ones. For example (you may want to reference the tables below for this example), in `?&bc#a[`, `?` coerces `&` into an expression, which coerces `b` and `c` into expressions. `#` is the next available character, so `?` coerces it into a statement, and `#` coerces `a` into an expression. `?&bc#a` is a complete statement, so `[` becomes a statement by default.

| Statement | Description | Example | Example notes |
| --- | --- | --- | --- |
| `. <Expr>` | Print the expression as a number | `.:1` | Prints `1` |
| `, <Expr>` | Print the expression as a character | `,;A` | Prints `A` |
| `a <Expr>`, `b <Expr>`, `c <Expr>` | Store the expression in register `a`, `b`, or `c` | `a:1` | Stores `1` in register `a` |
| `? <Expr> <Stmt>` | Execute the statement if the expression is truthy | `?:0.:1` | Prints nothing |
| `[` | Jump forward, just after the matching `]` | `?:1[.:1].:0` | Skips `1` and prints `0` |
| `]` | Jump back, just after the matching `[` | `?:0[.:1].:0` | Prints `1` forever |
| `# <Expr>` | Extracts the expression, if it's an archive. Registers are overwritten with the registers stored in the archive. Registers that do not have a value in the archive are not overwritten. | `#a` | Extracts the archive `a` |

### Expressions

| Expression | Description | Example | Example notes |
| --- | --- | --- | --- |
| `: <Number>` | A number literal | `.:-1` | Prints `-1` |
| `; <Character>` | A character literal | `.;A` | Prints `65` |
| `.` | Read a number from the buffer. If the buffer is empty, it is refilled from stdin. | `a.` | Stores a number from stdin to register `a` |
| `,` | Read the next character from the buffer. If the buffer is empty, it is refilled from stdin. The buffer will always end with `\r`, then `\n`. | `,,` | Echos a character from the buffer to stdout |
| `a`, `b`, `c` | Gets the value of register `a`, `b`, or `c` | `.a` | Prints the value of register `a` as a number |
| `+ <Expr> <Expr>` | Add two numbers | `.+:1:2` | Prints `3` |
| `- <Expr> <Expr>` | Subtract two numbers | `.-:1:2` | Prints `-1` |
| `* <Expr> <Expr>` | Multiply two numbers | `.*:4:3` | Prints `12` |
| `/ <Expr> <Expr>` | Divide two numbers | `./:12:5` | Prints `2` |
| `% <Expr> <Expr>` | Modulo two numbers | `.%:12:5` | Prints `2` |
| `! <Expr>` | Logical NOT. Evaluates to `1` for any falsey value, and `0` for any truthy value. All archives are truthy. | `.!:-1` | Prints `0` |
| `& <Expr> <Expr>` | Logical AND | `.&:1a` | Prints `1` if register `a` is truthy, and `0` otherwise |
| <code>&#124; &lt;Expr&gt;</code> | Logical OR | <code>.&#124;:0a</code> | Prints `1` if register `a` is truthy, and `0` otherwise |
| `= <Expr> <Expr>` | Checks whether the expressions equal. Any archive is considered equal to any other archive. | `.=:-1-:3:2` | Prints `1` |
| `< <Expr> <Expr>` | Checks whether the first expression is less than the second | `.<:3:5` | Prints `1` |
| `> <Expr> <Expr>` | Checks whether the first expression is greater than the second | `.>:3:5` | Prints `0` |
| `? <Expr> <Expr> <Expr>` | Conditional. If the first expression is truthy, it resolves to the second expression. Otherwise, it resolves to the third expression. | `,?;A;B;C` | Prints `C` |
| `[ <Registers> ]` | Creates an archive of the given registers | `c[ac]` | Creates an archive containing the current values of registers `a` and `c`, and stores it in register `c` |

`+`, `-`, `*`, `/`, `%`, `>`, and `<` evaluate to `0` if either expression is an archive

## Examples

You can find the examples in the `examples` directory. I recommend trying to solve some of these problems yourself first, and looking at examples if you need help. If following examples are roughly ordered by difficulty.

| Example | Description |
| --- | --- |
| `hello_world.twr` | Prints `Hello, world!` |
| `fibonacci.twr` | Prints Fibonacci numbers until it crashes from overflow |
| `primes.twr` | Prints prime numbers |
| `factorial.twr` | Calculates the factorial of a given number |
| `99_bottles.twr` | Prints the lyrics to "99 Bottles of Beer". Reuses as much of the text as is practical. |
| `echo.twr` | Echos the given input to output |
| `bracket_checker.twr` | Checks whether given brackets are balanced |
| `sort.twr` | This is an unfinished implementation of a sorting algorithm. Once this example is completed, and necessary modifications are made to the language, I plan to release its 1.0. |

## Installation

Install Rust and clone the repo.

## Usage

Run a .twr file with `tower <file>`. If running through cargo (which you should), it will look like `cargo run <file>`.

## Contributing

If you have a suggestion, bug report, or contribution, feel free to open an issue or pr.

## License

T\
o\
w\
e\
r\
is dual-licensed under either

* MIT License (LICENSE-MIT or http://opensource.org/licenses/MIT)
* Apache License, Version 2.0 (LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)

at your option.