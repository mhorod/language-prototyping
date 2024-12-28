// Helper library for the program

#include <stdlib.h>
#include <stdio.h>

typedef struct {
    void* function;
    void* parent;
    long long arg;
} Closure;

// align the stack pointer to 16 bytes and call new_closure
asm(
    ".globl new_closure\n"
    "new_closure:\n"
    "mov %rsp, %r12\n"
    "and $15, %r12\n"
    "sub %r12, %rsp\n"
    "call _new_closure\n"
    "add %r12, %rsp\n"
    "ret\n"
);


void* new_closure(void* parent, void* function) {
    Closure* closure = malloc(sizeof(Closure));
    closure->function = function;
    closure->parent = parent;
    //printf("Created empty closure %p %p\n", function, parent);
    return closure;
}

// align the stack pointer to 16 bytes and call clone_closure
asm(
    ".globl clone_closure\n"
    "clone_closure:\n"
    "mov %rsp, %r12\n"
    "and $15, %r12\n"
    "sub %r12, %rsp\n"
    "call _clone_closure\n"
    "add %r12, %rsp\n"
    "ret\n"
);


void* clone_closure(Closure* old, long long arg) {
    //printf("Cloning closure %p %p with arg %lld (%p)\n", old->function, old->parent, arg, arg);
    Closure* closure = malloc(sizeof(Closure));
    closure->function = old->function;
    closure->parent = old->parent;
    closure->arg = arg;
    return closure;
}

// align the stack pointer to 16 bytes and call print_int
asm(
    ".globl print_int\n"
    "print_int:\n"
    "mov %rsp, %r12\n"
    "and $15, %r12\n"
    "sub %r12, %rsp\n"
    "call _print_int\n"
    "add %r12, %rsp\n"
    "ret\n"
);



void print_int(long long x) {
    printf("%lld\n", x);
}