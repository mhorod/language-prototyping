# Language prototyping

The point of this repo is to collect ideas and experiments related to building a
simple interpreter from scratch (i.e. without external libraries). This includes
concepts like lexing, parsing, type checking, code execution, compilation to
bytecote, optimisations.

# Parsing

### Goals

- human friendly error recovery
- comfortable to implement
- translatable to languages such as C

### Sources of inspiration

[Rust](https://github.com/rust-lang/rust) - with hand-written lexer and parser

[Tao](https://github.com/zesterer/tao) - parser is written using parser
combinators from [Chumsky](https://github.com/zesterer/chumsky/)

[Fall](https://github.com/matklad/fall) - a parser generator
