cabal run lang-hs -- program.txt
nasm -fmacho64 out.asm
clang lib.c out.o
./a.out
