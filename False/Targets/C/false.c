#include <stdlib.h>
#include <stdio.h>


typedef struct{
    size_t *first;
    size_t *cur;
    size_t *last;
}f_stack;


typedef void f_lambda(f_stack*);
typedef size_t f_namespace[26];


static f_namespace namespace;


void f_stackinit(f_stack *stack){
    stack->first = malloc(sizeof(size_t) * 1024);
    stack->cur = stack->first;
    stack->last = &stack->first[1023];
}


void f_stackpush(f_stack *stack,size_t v){
    size_t delta;
    if (stack->cur == stack->last){
        delta = (stack->last - stack->first) * 2;
        stack->first = realloc(stack->first,delta);
        stack->last = stack->first + delta;
        stack->cur = stack->first + (delta / 2);
    }
    stack->cur += sizeof(size_t);
    *stack->cur = v;
}


void f_namespaceinit(f_namespace ns,char **argv){
    ns[0] = (size_t) argv;
}


void f_init(f_namespace ns,f_stack *stack,char **argv){
    f_stackinit(stack);
    f_namespaceinit(ns,argv);
}


void f_add(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = b + a;
}


void f_sub(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = b - a;
}


void f_mul(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = b * a;
}


void f_div(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = b / a;
}


void f_neg(f_stack *stack){
    *stack->cur = -(*stack->cur);
}


void f_eq(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = a == b;
}


void f_gt(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = b > a;
}


void f_not(f_stack *stack){
    *stack->cur = !(*stack->cur);
}


void f_and(f_stack *stack){
    size_t a = *(stack->cur--);
    size_t b = *stack->cur;
    *stack->cur = b && a;
}


void f_or(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = b || a;
}


void f_assign(f_stack *stack){    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    stack->cur -= sizeof(size_t);
    namespace[a] = b;
}


void f_read(f_stack *stack){
    *stack->cur = namespace[*(stack->cur)];
}


void f_apply(f_stack *stack){
    size_t a = *stack->cur;
    stack->cur -= sizeof(size_t);
    ((f_lambda*) a)(stack);
}


void f_dup(f_stack *stack){
    f_stackpush(stack,*stack->cur);
}


void f_del(f_stack *stack){
    stack->cur -= sizeof(size_t);
}



void f_swap(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    *stack->cur = a;
    stack->cur += sizeof(size_t);
    *stack->cur = b;
}


void f_rot(f_stack *stack){
    size_t a;
    size_t b;
    size_t c;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    stack->cur -= sizeof(size_t);
    c = *stack->cur;
    *stack->cur = a;
    stack->cur += sizeof(size_t);
    *stack->cur = c;
    stack->cur += sizeof(size_t);
    *stack->cur = b;
}


void f_pick(f_stack *stack){
    size_t a = *stack->cur;
    stack->cur -= sizeof(size_t);
    f_stackpush(stack,stack->first[a]);
}


void f_if(f_stack *stack){
    size_t a;
    size_t b;
    a = *stack->cur;
    stack->cur -= sizeof(size_t);
    b = *stack->cur;
    stack->cur -= sizeof(size_t);
    if (b){
        ((f_lambda*) a)(stack);
    }
}


void f_while(f_stack *stack){
    f_lambda *cond;
    f_lambda *body;
    cond = (f_lambda*) *stack->cur;
    stack->cur -= sizeof(size_t);
    body = (f_lambda*) *stack->cur;
    stack->cur -= sizeof(size_t);
    cond(stack);
    while (*stack->cur){
        body(stack);
        cond(stack);
        stack->cur -= sizeof(size_t);
    }
}


void f_printi(f_stack *stack){
  printf("%ld",*(stack->cur--));
}


void f_printc(f_stack *stack){
    putchar(*(stack->cur--));
}


void f_input(f_stack *stack){
    f_stackpush(stack,getchar());
}


void f_flush(f_stack *stack){
    fflush(stdout);
}
