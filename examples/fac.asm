extern snprintf


        section .data

fmt:    db      `%ld`
lsp:    dq      0
str_3:  db      `\n`,0
len_3:  dq      $-str_3
str_2:  db      `illegal input!`,0
len_2:  dq      $-str_2
str_1:  db      `result: `,0
len_1:  dq      $-str_1
str_0:  db      `calculate the factorial of [1..8]: `,0
len_0:  dq      $-str_0


        section .bss

lstack: resq    256
io:     resd    1
pb:     resb    64
a:      resq    1
b:      resq    1
c:      resq    1
d:      resq    1
e:      resq    1
f:      resq    1
g:      resq    1
h:      resq    1
i:      resq    1
j:      resq    1
k:      resq    1
l:      resq    1
m:      resq    1
n:      resq    1
o:      resq    1
p:      resq    1
q:      resq    1
r:      resq    1
s:      resq    1
t:      resq    1
u:      resq    1
v:      resq    1
w:      resq    1
x:      resq    1
y:      resq    1
z:      resq    1


        section .text

global main

lambda_1:
        push    qword [rsp]
        push    0x1
        pop     rbx
        pop     rax
        sub     rax,rbx
        push    rax
        push    f
        pop     rax
        push    qword [rax]
        pop     rax
        mov     rbx,lstack
        add     rbx,[lsp]
        mov     qword [rbx],ret_0
        add     qword [lsp],0x8
        jmp     rax
ret_0:
        pop     rax
        pop     rbx
        imul    rax,rbx
        push    rax
        sub     qword [lsp],0x8
        mov     r8,[lsp]
        add     r8,lstack
        jmp     [r8]




lambda_0:
        push    qword [rsp]
        push    0x1
        pop     rax
        pop     rbx
        cmp     rax,rbx
        push    0x-1
        je      eq_end_1
        pop     rax
        push    0x0
eq_end_1:
        pop     rax
        not     rax
        push    rax
        push    lambda_1
        pop     rax
        pop     rbx
        cmp     rbx,0x0
        je      endif_2
        mov     rcx,lstack
        add     rcx,[lsp]
        mov     qword [rcx],endif_2
        add     qword [lsp],0x8
        jmp     rax
endif_2:
        sub     qword [lsp],0x8
        mov     r8,[lsp]
        add     r8,lstack
        jmp     [r8]




lambda_2:
        pop     rax
        pop     rbx
        push    rax
        push    rbx
        push    f
        pop     rax
        push    qword [rax]
        pop     rax
        mov     rbx,lstack
        add     rbx,[lsp]
        mov     qword [rbx],ret_3
        add     qword [lsp],0x8
        jmp     rax
ret_3:
        pop     rcx
        mov     rdi,pb
        mov     rsi,0x40
        mov     rdx,fmt
        call    snprintf
        mov     rdx,rax
        mov     rax,0x1
        mov     rdi,0x1
        mov     rsi,pb
        syscall
        sub     qword [lsp],0x8
        mov     r8,[lsp]
        add     r8,lstack
        jmp     [r8]




lambda_3:
        mov     rax,0x1
        mov     rdi,0x1
        mov     rsi,str_2
        mov     rdx,[len_2]
        syscall
        sub     qword [lsp],0x8
        mov     r8,[lsp]
        add     r8,lstack
        jmp     [r8]




main:
        push    lambda_0
        push    f
        pop     rax
        pop     rbx
        mov     [rax],rbx
        mov     rax,0x1
        mov     rdi,0x1
        mov     rsi,str_0
        mov     rdx,[len_0]
        syscall
        mov     rax,0x0
        mov     rdi,0x0
        mov     rsi,io
        mov     rdx,0x1
        syscall
        push    qword [io]
        push    0x30
        pop     rbx
        pop     rax
        sub     rax,rbx
        push    rax
        push    qword [rsp]
        push    qword [rsp]
        push    0x0
        pop     rbx
        pop     rax
        push    0x-1
        cmp     rax,rbx
        jg      gt_end_4
        pop     rax
        push    0x0
gt_end_4:
        pop     rax
        not     rax
        push    rax
        pop     rax
        pop     rbx
        push    rax
        push    rbx
        push    0x8
        pop     rbx
        pop     rax
        push    0x-1
        cmp     rax,rbx
        jg      gt_end_5
        pop     rax
        push    0x0
gt_end_5:
        pop     rax
        pop     rbx
        or      rax,rbx
        push    rax
        push    qword [rsp]
        mov     rax,0x1
        mov     rdi,0x1
        mov     rsi,str_1
        mov     rdx,[len_1]
        syscall
        pop     rax
        not     rax
        push    rax
        push    lambda_2
        pop     rax
        pop     rbx
        cmp     rbx,0x0
        je      endif_6
        mov     rcx,lstack
        add     rcx,[lsp]
        mov     qword [rcx],endif_6
        add     qword [lsp],0x8
        jmp     rax
endif_6:
        push    lambda_3
        pop     rax
        pop     rbx
        cmp     rbx,0x0
        je      endif_7
        mov     rcx,lstack
        add     rcx,[lsp]
        mov     qword [rcx],endif_7
        add     qword [lsp],0x8
        jmp     rax
endif_7:
        mov     rax,0x1
        mov     rdi,0x1
        mov     rsi,str_3
        mov     rdx,[len_3]
        syscall
        mov     rax,0x3c
        mov     rbx,0x0
        syscall