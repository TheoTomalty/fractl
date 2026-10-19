    .global _start

    .set __NR_read, 63
    .set __NR_write, 64
    .set __NR_exit, 93

    /*
    %esi -> x20 (points to the current word being interpreted)
    %eax -> x21 (the jump address)
    %esp -> x22 (parameter stack)
    %ebp -> x23 (return stack)

    | LINK | flags | len | string             | code_* |
    | 64   | 32    | 32  | align(len * 8, 64) |        |
    */

    .macro NEXT
    ldr x21, [x20], 8
    ldr x30, [x21]
    br x30
    .endm

    .macro PUSHR reg
    str \reg, [x23, -8]!
    .endm

    .macro POPR reg
    ldr \reg, [x23], 8
    .endm

    .set DATA_SEGMENT_SIZE, 1048576
    .set RETURN_STACK_SIZE, 1048576
    .set BUFFER_SIZE, 4096

    .section .bss
    .align 12
data_segment:
    .space DATA_SEGMENT_SIZE
    .align 12
return_stack:
    .space RETURN_STACK_SIZE
return_stack_top:   
    .align 12
buffer:
    .space BUFFER_SIZE

    .section .text
docol:
    PUSHR x20
    add x20, x21, 8
    NEXT

_start:
    adr x20, cold_start
    mov x22, sp
    adr x9, var_sz
    str x22, [x9]
    adrp x23, return_stack_top
    add x23, x23, :lo12:return_stack_top
    NEXT

    .section .rodata
cold_start:
    .quad quit

    .set F_IMMED, 0x80
    .set F_HIDDEN, 0x20

    .set link, 0
    
    .macro defword name, namelen, flags=0, label
    .section .rodata
    .align 3
    .global name_\label
name_\label :
    .quad link
    .set link, name_\label
    .word \flags
    .word \namelen
    .ascii "\name"
    .align 3
    .global \label
\label :
    .quad docol
    .endm

    .macro defcode name, namelen, flags=0, label
    .section .rodata
	.align 3
	.global name_\label
name_\label :
	.quad link
	.set link, name_\label
    .word \flags
    .word \namelen
	.ascii "\name"
	.align 3
	.global \label
\label :
	.quad code_\label
	.section .text
	.align 2
	.global code_\label
code_\label :
    .endm

	.macro defvar name, namelen, flags=0, label, initial=0
	defcode \name,\namelen,\flags,\label
	adr x9, var_\label
    str x9, [x22, -8]!
	NEXT
	.section .data
	.align 3
var_\label :
	.quad \initial
	.endm

    .macro defconst name, namelen, flags=0, label, value
    defcode \name, \namelen, \flags, \label
    ldr x9, var_\label
    str x9, [x22, -8]!
    NEXT
    .section .rodata
    .align 3
    var_\label :
    .quad \value
    .endm

    defvar "state",5,,state,0
    defvar "here",4,,here,data_segment
    defvar "latest",6,,latest,name_print
    defvar "s0",2,,sz
    defvar "base",4,,base,10
    defvar "debug",5,,debug,0

    defconst "r0",2,,rz,return_stack_top
    defconst "docol",5,,__docol,docol

    defcode "brk",3,,brk
    brk 0
    NEXT

    defcode "drop",4,,drop
    add x22, x22, 8
    NEXT

    defcode "swap",4,,swap
    ldr x0, [x22]
    ldr x1, [x22, 8]
    str x1, [x22]
    str x0, [x22, 8]
    NEXT

    defcode "dup",3,,dup
    ldr x0, [x22]
    str x0, [x22, -8]!
    NEXT

    defcode "over",4,,over
    ldr x0, [x22, 8]
    str x0, [x22, -8]!
    NEXT

    defcode "rot",3,,rot
    ldr x0, [x22]
    ldr x1, [x22, 8]
    ldr x2, [x22, 16]
    str x2, [x22]
    str x0, [x22, 8]
    str x1, [x22, 16]
    NEXT

    defcode "-rot",4,,nrot
    ldr x0, [x22]
    ldr x1, [x22, 8]
    ldr x2, [x22, 16]
    str x1, [x22]
    str x2, [x22, 8]
    str x0, [x22, 16]
    NEXT

    defcode "2drop",5,,twodrop
    add x22, x22, 16
    NEXT

    defcode "2dup",4,,twodup
    ldr x0, [x22]
    ldr x1, [x22, 8]
    str x1, [x22, -8]!
    str x0, [x22, -8]!
    NEXT

    defcode "2swap",5,,twoswap
    ldr x0, [x22]
    ldr x1, [x22, 8]
    ldr x2, [x22, 16]
    ldr x3, [x22, 24]
    str x1, [x22]
    str x0, [x22, 8]
    str x3, [x22, 16]
    str x2, [x22, 24]
    NEXT

    defcode "?dup",4,,qdup
    ldr x0, [x22]
    cmp x0, 0
    beq 1f
    str x0, [x22, -8]!
1:  NEXT

    defcode "1+",2,,incr
    ldr x0, [x22]
    add x0, x0, 1
    str x0, [x22]
    NEXT

    defcode "1-",2,,decr
    ldr x0, [x22]
    sub x0, x0, 1
    str x0, [x22]
    NEXT

    defcode "8+",2,,incr8
    ldr x0, [x22]
    add x0, x0, 8
    str x0, [x22]
    NEXT

    defcode "8-",2,,decr8
    ldr x0, [x22]
    sub x0, x0, 8
    str x0, [x22]
    NEXT

    defcode "+",1,,add
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    add x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "-",1,,sub
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    sub x0, x1, x0
    str x0, [x22, -8]!
    NEXT

    defcode "*",1,,mul
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    mul x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "/mod",4,,divmod
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    udiv x2, x1, x0
    mul x3, x2, x0
    sub x3, x1, x3
    str x3, [x22, -8]!
    str x2, [x22, -8]!
    NEXT

    defcode "f+",2,,fadd
    ldr s1, [x22], 8
    ldr s0, [x22], 8
    fadd s0, s0, s1
    str d0, [x22, -8]!
    NEXT

    defcode "f-",2,,fsub
    ldr s1, [x22], 8
    ldr s0, [x22], 8
    fsub s0, s0, s1
    str d0, [x22, -8]!
    NEXT

    defcode "f*",2,,fmul
    ldr s1, [x22], 8
    ldr s0, [x22], 8
    fmul s0, s0, s1
    str d0, [x22, -8]!
    NEXT

    defcode "f/",2,,fdiv
    ldr s1, [x22], 8
    ldr s0, [x22], 8
    fdiv s0, s0, s1
    str d0, [x22, -8]!
    NEXT

    defcode "d+",2,,dadd
    ldr d1, [x22], 8
    ldr d0, [x22], 8
    fadd d0, d0, d1
    str d0, [x22, -8]!
    NEXT

    defcode "d-",2,,dsub
    ldr d1, [x22], 8
    ldr d0, [x22], 8
    fsub d0, d0, d1
    str d0, [x22, -8]!
    NEXT

    defcode "d*",2,,dmul
    ldr d1, [x22], 8
    ldr d0, [x22], 8
    fmul d0, d0, d1
    str d0, [x22, -8]!
    NEXT

    defcode "d/",2,,ddiv
    ldr d1, [x22], 8
    ldr d0, [x22], 8
    fdiv d0, d0, d1
    str d0, [x22, -8]!
    NEXT

    .macro defcmp name, length, label, cmp
    defcode \name, \length, 0, \label
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    cmp x1, x0
    cset x2, \cmp
    str x2, [x22, -8]!
    NEXT
    .endm

    .macro defcmpz name, length, label, cmp
    defcode \name, \length, 0, \label
    ldr x0, [x22], 8
    cmp x0, 0
    cset x2, \cmp
    str x2, [x22, -8]!
    NEXT
    .endm

    .macro defcmpf name, length, label, cmp
    defcode \name, \length, 0, \label
    ldr s0, [x22], 8
    ldr s1, [x22], 8
    fcmp s1, s0
    cset x0, \cmp
    str x0, [x22, -8]!
    NEXT
    .endm

    .macro defcmpd name, length, label, cmp
    defcode \name, \length, 0, \label
    ldr d0, [x22], 8
    ldr d1, [x22], 8
    fcmp d1, d0
    cset x0, \cmp
    str x0, [x22, -8]!
    NEXT
    .endm

    // Using \x3d instead of '=' because it messes up the assembler
    defcmp "\x3d",1,equ,eq
    defcmp "<>",2,nequ,ne
    defcmp "<",1,lt,lt
    defcmp ">",1,gt,gt
    defcmp "<\x3d",2,le,le
    defcmp ">\x3d",2,ge,ge

    defcmpz "0\x3d",2,zequ,eq
    defcmpz "0<>",3,znequ,ne
    defcmpz "0<",2,zlt,lt
    defcmpz "0>",2,zgt,gt
    defcmpz "0<\x3d",3,zle,le
    defcmpz "0>\x3d",3,zge,ge

    defcmpf "f\x3d",2,fequ,eq
    defcmpf "f<>",3,fnequ,ne
    defcmpf "f<",2,flt,lt
    defcmpf "f>",2,fgt,gt
    defcmpf "f<\x3d",3,fle,le
    defcmpf "f>\x3d",3,fge,ge

    defcmpd "d\x3d",2,dequ,eq
    defcmpd "d<>",3,dnequ,ne
    defcmpd "d<",2,dlt,lt
    defcmpd "d>",2,dgt,gt
    defcmpd "d<\x3d",3,dle,le
    defcmpd "d>\x3d",3,dge,ge

    defcode "and",3,,and
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    and x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "or",2,,or
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    orr x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "xor",3,,xor
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    eor x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "lsl",3,,lsl
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    lsl x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "lsr",3,,lsr
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    lsr x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "asr",3,,asr
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    asr x0, x0, x1
    str x0, [x22, -8]!
    NEXT

    defcode "cls",3,,cls
    ldr x0, [x22], 8
    cls x0, x0
    str x0, [x22, -8]!
    NEXT

    defcode "clz",3,,clz
    ldr x0, [x22], 8
    clz x0, x0
    str x0, [x22, -8]!
    NEXT

    defcode "sxtb",4,,sxtb
    ldr x0, [x22], 8
    sxtb x0, w0
    str x0, [x22, -8]!
    NEXT

    defcode "sxth",4,,sxth
    ldr x0, [x22], 8
    sxth x0, w0
    str x0, [x22, -8]!
    NEXT

    defcode "sxtw",4,,sxtw
    ldr x0, [x22], 8
    sxtw x0, w0
    str x0, [x22, -8]!
    NEXT

    defcode "invert",6,,invert
    ldr x0, [x22]
    mvn x0, x0
    str x0, [x22]
    NEXT

    defcode "exit",4,,exit
    POPR x20
    NEXT

    defcode "lit",3,,lit
    ldr x0, [x20], 8
    str x0, [x22, -8]!
    NEXT

    defcode "!",1,,store
    ldr x0, [x22], 8 // address
    ldr x1, [x22], 8 // data
    str x1, [x0]
    NEXT

    defcode "@",1,,fetch
    ldr x0, [x22], 8 // address
    ldr x1, [x0]
    str x1, [x22, -8]!
    NEXT

    defcode "+!",2,,addstore
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    ldr x2, [x0]
    add x2, x2, x1
    str x2, [x0]
    NEXT

    defcode "-!",2,,substore
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    ldr x2, [x0]
    sub x2, x2, x1
    str x2, [x0]
    NEXT

    defcode "c!",2,,storebyte
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    strb w1, [x0]
    NEXT

    defcode "c@",2,,fetchbyte
    ldr x0, [x22], 8
    ldrb w1, [x0]
    str x1, [x22, -8]!
    NEXT

    defcode "s!",2,,storehalf
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    strh w1, [x0]
    NEXT

    defcode "s@",2,,fetchhalf
    ldr x0, [x22], 8
    ldrh w1, [x0]
    str x1, [x22, -8]!
    NEXT

    defcode "w!",2,,storeword
    ldr x0, [x22], 8
    ldr x1, [x22], 8
    str w1, [x0]
    NEXT

    defcode "w@",2,,fetchword
    ldr x0, [x22], 8
    ldr w1, [x0]
    str x1, [x22, -8]!
    NEXT

    // defcode "c@c!"
    // defcode "cmove"

    defcode ">r",2,,tor
    ldr x0, [x22], 8
    PUSHR x0
    NEXT

    defcode "r>",2,,fromr
    POPR x0
    str x0, [x22, -8]!
    NEXT

    defcode "intw@",5,,intwfetch
    str x20, [x22, -8]!
    NEXT

    defcode "rsp@",4,,rspfetch
    str x23, [x22, -8]!
    NEXT

    defcode "rsp!",4,,rspstore
    ldr x23, [x22], 8
    NEXT

    defcode "rdrop",5,,rdrop
    add x23, x23, 8
    NEXT

    defcode "dsp@",4,,dspfetch
    mov x0, x22
    str x0, [x22, -8]!
    NEXT

    defcode "dsp!",4,,dspstore
    ldr x22, [x22]
    NEXT

    defcode "key",3,,key
    bl _key
    str x0, [x22, -8]!
    NEXT
_key:
    ldr x9, currkey
    ldr x10, bufftop
    cmp x9, x10
    bge 1f
    ldrb w0, [x9], 1
    adr x11, currkey
    str x9, [x11]
    ret

1:
    mov x0, 0
    adrp x1, buffer
    add x1, x1, :lo12:buffer
    mov x2, BUFFER_SIZE
    mov x8, __NR_read
    svc 0
    cmp w0, -1
    beq 2f
    cmp w0, 0
    beq 3f
    adr x11, currkey
    str x1, [x11]
    adr x11, bufftop
    add x9, x1, x0
    str x9, [x11]
    b _key

2:  // read error
    mov x0, 1
    mov x8, __NR_exit
    svc 0

3:
    mov w0, 0
    ret

    defcode "emit",4,,emit
    ldr x0, [x22], 8
    bl _emit
    NEXT
_emit:
    sub x22, x22, 8
    strb w0, [x22]
    mov x0, 1
    mov x1, x22
    mov x2, 1
    mov x8, __NR_write
    svc 0
    add x22, x22, 8
    ret

    .section .data
    .align 3
currkey:
    .quad buffer
bufftop:
    .quad buffer


    defcode "word",4,,word
    bl _word
    str x0, [x22, -8]!
    str x1, [x22, -8]!
    NEXT
_word:
    str x30, [x22, -16]!

1:  
    bl _key
    cmp w0, '\\'
    beq 3f
    cmp w0, ' '
    beq 1b
    cmp w0, '\n'
    beq 1b
    cmp w0, '\t'
    beq 1b
    adr x9, word_buffer
    cmp w0, 0
    beq 4f

2:
    strb w0, [x9], 1
    str x9, [x22, 8]
    bl _key
    ldr x9, [x22, 8]
    cmp w0, ' '
    beq 4f
    cmp w0, 0
    beq 4f
    cmp w0, '\n'
    beq 4f
    b 2b

4:
    adr x0, word_buffer
    sub x1, x9, x0
    ldr x30, [x22], 16
    ret

3:  
    bl _key
    cmp w0, '\n'
    beq 1b
    cmp w0, 0
    beq 1b
    b 3b

    .section .data
word_buffer:
    .space 128

    defcode "number",6,,number
    ldr x1, [x22], 8 // length of string
    ldr x0, [x22], 8 // address of string
    bl _number
    str x0, [x22, -8]! // parsed number
    str x1, [x22, -8]! // number of unused characters (0 = no error)
    NEXT

_number:
    // Select base
    // TODO: skip this check if var_base includes 'x' as a possible digit
    // TODO: do similar checks for binary and octal
    cmp x1, 2
    blt 9f
    ldrb w9, [x0]
    cmp w9, '0'
    bne 9f
    ldrb w9, [x0, 1]
    cmp w9, 'x'
    bne 9f
    sub x1, x1, 2
    add x0, x0, 2
    mov x9, 16
    b 10f

9:  ldr x9, var_base
10: cmp x1, 0
    bne 1f
    mov x0, 0
    mov x1, 0
    ret
1:
    mov x10, 0 // sum
    mov x11, x1 // chars left
    mov x12, x0 // currchar
    mov x13, 1 // sign

    cmp x11, 0
    beq 2f
    ldrb w14, [x12], 1 // char
    sub x11, x11, 1
    cmp x14, '-'
    bne 3f
    mov x13, -1
4:
    cmp x11, 0
    beq 2f
    ldrb w14, [x12], 1
    sub x11, x11, 1
3:
    cmp x14, '0'
    blt 5f
    cmp x14, '9'
    bgt 6f
    sub x14, x14, '0'
    b 7f
6:  cmp x14, 'A'
    blt 5f
    cmp x14, 'Z'
    bgt 8f
    sub x14, x14, 'A'
    add x14, x14, 10
    b 7f
8:  cmp x14, 'a'
    blt 5f
    cmp x14, 'z'
    bgt 5f
    sub x14, x14, 'a'
    add x14, x14, 10
7:
    cmp x14, x9
    bge 5f
    madd x10, x10, x9, x14
    b 4b
5:
    add x11, x11, 1
2:
    mov x1, x11
    mul x0, x10, x13
    ret

    defcode "find",4,,find
    ldr x1, [x22], 8 // length of string
    ldr x0, [x22], 8 // address of string
    bl _find
    str x0, [x22, -8]! // address of dictionary entry
    NEXT
_find:
    ldr x9, var_latest // candidate entry
5:
    cmp x9, 0
    beq 1f
    ldr w15, [x9, 8] // flags
    and w15, w15, F_HIDDEN
    cmp w15, 0
    bne 2f
    ldr w10, [x9, 12] // length
    cmp x10, x1
    bne 2f
    add x11, x9, 16 // string start
    mov x12, x0
3:
    cmp w10, 0
    beq 4f
    ldrb w13, [x11], 1 // ch1
    ldrb w14, [x12], 1 // ch2
    sub w10, w10, 1
    cmp w13, w14
    bne 2f
    b 3b
4:  
    mov x0, x9
    ret
2:
    ldr x9, [x9]
    b 5b
1:
    mov x0, 0
    ret

    defcode ">cfa",4,,tcfa
    ldr x0, [x22], 8
    bl _tcfa
    str x0, [x22, -8]!
    NEXT
_tcfa:
    ldr w9, [x0, 12]
    add w9, w9, 7
    and w9, w9, ~7
    add x0, x0, 16
    add x0, x0, x9
    ret

    defword ">dfa",4,,tdfa
    .quad tcfa
    .quad incr8
    .quad exit

    defcode "create",6,,create
    ldr x9, [x22], 8   // length
    ldr x10, [x22], 8  // address
    ldr x11, var_here
    ldr x12, var_latest
    adr x13, var_latest
    str x11, [x13]
    str x12, [x11], 8
    str wzr, [x11], 4
    str w9, [x11], 4
1:
    cmp w9, 0
    beq 2f
    ldrb w13, [x10], 1
    sub w9, w9, 1
    strb w13, [x11], 1
    b 1b
2:
    add x11, x11, 7
    and x11, x11, ~7
    adr x12, var_here
    str x11, [x12]
    NEXT

    defcode ",",1,,comma
    ldr x0, [x22], 8
    bl _comma
    NEXT
_comma:
    adr x9, var_here
    ldr x10, [x9]
    str x0, [x10], 8
    str x10, [x9]
    ret

    defcode "[",1,F_IMMED,lbrac
    adr x9, var_state
    mov x10, 0
    str x10, [x9]
    NEXT

    defcode "]",1,,rbrac
    adr x9, var_state
    mov x10, 1
    str x10, [x9]
    NEXT

    defword ":",1,,colon
    .quad word
    .quad create
    .quad lit, docol, comma
    .quad latest, fetch, hidden
    .quad rbrac
    .quad exit

    defword ";",1,F_IMMED,semicolon
    .quad lit, exit, comma
    .quad latest, fetch, hidden
    .quad lbrac
    .quad exit

    defcode "immediate",9,F_IMMED,immediate
    ldr x9, var_latest
    ldr w10, [x9, 8]
    eor w10, w10, F_IMMED
    str w10, [x9, 8]
    NEXT

    defcode "hidden",6,,hidden
    ldr x9, [x22], 8
    ldr w10, [x9, 8]
    eor w10, w10, F_HIDDEN
    str w10, [x9, 8]
    NEXT

    defword "hide",4,,hide
    .quad word
    .quad find
    .quad hidden
    .quad exit

    defcode "'",1,,tick
    ldr x0, [x20], 8
    str x0, [x22, -8]!
    NEXT

    defcode "branch",6,,branch
    ldr x9, [x20]
    add x20, x20, x9
    NEXT

    defcode "0branch",7,,zbranch
    ldr x9, [x22], 8
    cmp x9, 0
    beq code_branch
    add x20, x20, 8
    NEXT

    defcode "litstring",9,,litstring
    ldr x0, [x20], 8 // string length
    str x20, [x22, -8]! // string address
    str x0, [x22, -8]!
    add x0, x0, 1
    add x20, x20, x0
    add x20, x20, 7
    and x20, x20, ~7
    NEXT

    defcode "tell",4,,tell
    mov x0, 1
    ldr x2, [x22], 8
    ldr x1, [x22], 8
    mov x8, __NR_write
    svc 0
    NEXT

    defword "quit",4,,quit
    .quad rz, rspstore
    .quad interpret
    .quad branch, -16

    defcode "interpret",9,,interpret
    sub x22, x22, 32
    // | string/8 | length/8 | is_lit/4 | number/8 |
    mov w9, 0
    str w9, [x22, 16]
    bl _word
    cmp x1, 0
    beq code_stop
    str x0, [x22]
    str x1, [x22, 8]

    // Lookup in dictionary.
    bl _find
    cmp x0, 0
    beq 1f

    // In dictionary.
    ldr w9, [x0, 8]
    str w9, [x22, -8]!
    bl _tcfa
    ldr w9, [x22], 8
    and w9, w9, F_IMMED
    cmp w9, 0
    bne 4f
    b 2f

1:  // Not in dictionary.
    mov w9, 1
    str w9, [x22, 16]
    ldr x0, [x22]
    ldr x1, [x22, 8]
    bl _number
    cmp x1, 0
    bne 6f
    str x0, [x22, 24]
    adr x0, lit

2:  // Are we compiling or executing?
    ldr x9, var_state
    cmp x9, 0
    beq 4f
    
    // Compiling
    bl _comma
    ldr w9, [x22, 16]
    cmp w9, 0
    beq 3f
    ldr x0, [x22, 24]
    bl _comma
3:  add x22, x22, 32
    NEXT
    
4:  // Executing
    ldr w9, [x22, 16]
    cmp w9, 0
    bne 5f
    
    // Executing not a literal
    add x22, x22, 32
    mov x21, x0
    ldr x30, [x21]
    br x30

5:  // Executing a literal
    add x22, x22, 24 // the number is already on the stack
    NEXT

6:  // Parse error
    mov x0, 2
    adr x1, parse_error_msg
    mov x2, parse_error_msg_end - parse_error_msg
    mov x8, __NR_write
    svc 0
    mov x0, 2
    ldr x1, [x22]
    ldr x2, [x22, 8]
    mov x8, __NR_write
    svc 0
    mov x0, 2
    adr x1, parse_error_msg_end
    mov x2, 1
    mov x8, __NR_write
    svc 0
    mov x0, 1
    mov x8, __NR_exit
    svc 0
    add x22, x22, 32
    NEXT
    
    .section .rodata
parse_error_msg:
    .ascii "PARSE ERROR: "
parse_error_msg_end:
    .ascii "\n"

    defcode "char",4,,char
    bl _word
    cmp x1, 0
    beq 1f
    ldrb w0, [x0]
    b 2f
1:  mov x0, 0
2:  str x0, [x22, -8]!
    NEXT

    defcode "execute",7,,execute
    ldr x21, [x22], 8
    ldr x30, [x21]
    br x30

    defcode "syscall6",8,,syscall6
    ldr x8, [x22], 8
    ldr x5, [x22], 8
    ldr x4, [x22], 8
    ldr x3, [x22], 8
    ldr x2, [x22], 8
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    svc 0
    str x0, [x22, -8]!
    NEXT

    defcode "syscall4",8,,syscall4
    ldr x8, [x22], 8
    ldr x3, [x22], 8
    ldr x2, [x22], 8
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    svc 0
    str x0, [x22, -8]!
    NEXT

    defcode "syscall3",8,,syscall3
    ldr x8, [x22], 8
    ldr x2, [x22], 8
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    svc 0
    str x0, [x22, -8]!
    NEXT

    defcode "syscall2",8,,syscall2
    ldr x8, [x22], 8
    ldr x1, [x22], 8
    ldr x0, [x22], 8
    svc 0
    str x0, [x22, -8]!
    NEXT

    defcode "syscall1",8,,syscall1
    ldr x8, [x22], 8
    ldr x0, [x22], 8
    svc 0
    str x0, [x22, -8]!
    NEXT

    defcode "syscall0",8,,syscall0
    ldr x8, [x22], 8
    svc 0
    str x0, [x22, -8]!
    NEXT

    defcode "stop",4,,stop
    mov x0, 0
    mov x8, __NR_exit
    svc 0

    defcode "one",3,,one
    mov x0, 1
    str x0, [x22, -8]!
    NEXT

    defword "two",3,,two
    .quad one
    .quad one
    .quad add
    .quad exit

    defword "three",5,,three
    .quad two
    .quad one
    .quad add
    .quad exit

    // This must be the last word in the dictionary
    defcode "print",5,,print
    ldr x0, [x22]
    bl print_int
    NEXT
    
///////////
// Utils //
///////////
    
    .section .text
print_int:
    // x0: i
    sub x22, x22, 104
    stp x9, x10, [x22, 24]
    stp x11, x12, [x22, 40]
    stp x13, x14, [x22, 56]
    stp x1, x2, [x22, 72]
    stp x0, x8, [x22, 88]
    add x9, x22, 16 // ptr
    mov x10, 10 // char
    strb w10, [x9], -1

1:  
    cmp x22, x9
    bgt 2f
    mov x11, 16 // radix
    udiv x12, x0, x11 // new-i
    mul x13, x12, x11
    sub x13, x0, x13 // remainder
    adr x14, hex_chars
    add x14, x14, x13
    ldrb w10, [x14]
    strb w10, [x9], -1
    mov x0, x12
    b 1b
2:

    mov x0, 1
    mov x1, x22
    mov x2, 17
    mov x8, __NR_write
    svc 0

    ldp x9, x10, [x22, 24]
    ldp x11, x12, [x22, 40]
    ldp x13, x14, [x22, 56]
    ldp x1, x2, [x22, 72]
    ldp x0, x8, [x22, 88]
    add x22, x22, 104
    ret

    .section .rodata
hex_chars:
    .asciz "0123456789abcdef"


