bits 16

section _ENTRY class=code

extern _cstart_
global entry

entry:
    cli
    
    ; setup stack
    ; ss = ds, bp = sp
    mov ax, ds
    mov ss, ax
    mov sp, 0
    mov bp, sp

    sti

    ;expect boot drive in DL, send it as argument to cstart function
    xor dh, dh
    push dx
    call _cstart_

    cli 
    hlt
