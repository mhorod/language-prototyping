# lang-hs

Proof of concept of a simple compiler written in Haskell.

Currently only works on MacOS due to nasm differences, however making it work on Linux should be trivial.

## Usage

```bash
cabal run lang-hs -- program.txt
nasm -fmacho64 out.asm
clang lib.c out.o
./a.out
```