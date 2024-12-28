cabal run
nasm -fmacho64 out.asm
clang lib.c out.o
