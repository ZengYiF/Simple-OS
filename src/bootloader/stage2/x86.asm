bits 16
section _TEXT class=CODE

;
; U4D
;
; Operation:      Unsigned 4 byte divide
; Inputs:         DX;AX   Dividend
;                 CX;BX   Divisor
; Outputs:        DX;AX   Quotient
;                 CX;BX   Remainder
; Volatile:       none
;
global __U4D
__U4D:
    shl edx, 16         ; dx to upper half of edx
    mov dx, ax          ; edx - dividend
    mov eax, edx        ; eax - dividend
    xor edx, edx

    shl ecx, 16         ; cx to upper half of ecx
    mov cx, bx          ; ecx - divisor

    div ecx             ; eax - quot, edx - remainder
    mov ebx, edx
    mov ecx, edx
    shr ecx, 16

    mov edx, eax
    shr edx, 16

    ret

;
; U4M
; Operation:      integer four byte multiply
; Inputs:         DX;AX   integer M1
;                 CX;BX   integer M2
; Outputs:        DX;AX   product
; Volatile:       CX, BX destroyed
;
global __U4M
__U4M:
    shl edx, 16         ; dx to upper half of edx
    mov dx, ax          ; m1 in edx
    mov eax, edx        ; m1 in eax

    shl ecx, 16         ; cx to upper half of ecx
    mov cx, bx          ; m2 in ecx

    mul ecx             ; result in edx:eax (we only need eax)
    mov edx, eax        ; move upper half to dx
    shr edx, 16

    ret
    
global _x86_div64_32
_x86_div64_32:
    ;make new call frame
    push bp
    mov bp, sp 

    push bx

    ;divide upper 32 bits
    mov eax, [bp + 8]         ; eax = upper 32 bits if dividend
    mov ecx, [bp + 12]        ; ecx = divisor
    xor edx, edx
    div ecx                 ; exc - quot , edx = reminder

    ;store upper 32 bits of quotient
    mov bx, [bp+16]
    mov [bx+4], eax

    ;divide lower 32 bits
    mov eax, [bp+4]         ; eax = lower 32 bits of dividend
    div ecx                 ; edx = old remainder

    ;store results
    mov [bx], eax
    mov bx, [bp + 18]
    mov [bx], edx

    pop bx

    ; restore old call frame
    mov sp, bp
    pop bp
    ret



;
; int 10h ah=0Eh
; args: character, page
;
global _x86_Video_WriteCharTeletype
_x86_Video_WriteCharTeletype:
    
    ; make new stack frame
    push bp
    mov bp, sp

    push bx

    ; [bp + 0] : old stack frame
    ; [bp + 2] : return address
    ; [bp + 4] : first arg(character)
    ; [bp + 6] : second arg(page)


    mov ah, 0X0E
    mov al, [bp + 4]
    mov bh, [bp + 6]

    int 0x10

    pop bx

    ; restore old call frame
    mov sp, bp
    pop bp
    ret


;
; bool _cdecl x86_Disk_Reset(uint8_t drive);
;

global _x86_Disk_Reset
_x86_Disk_Reset:
    push bp
    mov bp, sp

    mov ah, 0
    mov dl, [bp + 4]
    stc
    int 0x13
    
    mov ax, 1
    sbb ax, 0           ; 1 on success, 0 on fail

    ; restore old call frame
    mov sp, bp
    pop bp
    ret
;
; bool _cdecl x86_Disk_Read(uint8_t drive,
;                           uint16_t cyclinder,
;                           uint16_t sector,
;                           uint16_t head,
;                           uint8_t count,
;                           void far* dataOut);
global _x86_Disk_Read
_x86_Disk_Read:
    push bp
    mov bp, sp

    push bx
    push es

    ;set args
    mov dl, [bp+4]          ; dl = drive

    mov ch, [bp+6]          ; ch = cylinder(lower 8 bits)
    mov cl, [bp+7]          ; cl = cylinder to bits 6-7
    shl cl, 6

    mov al, [bp+8]          ; cl = sectors to bits 0-5
    and al, 0x3F
    or cl, al

    mov dh, [bp + 10]       ; dh = head
    mov al, [bp+12]         ; al = count
    
    mov bx, [bp+16]         ; es:bx = buffer
    mov es, bx
    mov bx, [bp+14]         

    mov ah,  0x02
    stc
    int 0x13
    
    mov ax, 1
    sbb ax, 0           ; 1 on success, 0 on fail

    pop es
    pop bx

    ; restore old call frame
    mov sp, bp
    pop bp
    ret
;
; bool _cdecl x86_Disk_GetDrivePramas(uint8_t drive,
;                                     uint8_t* driveTypeOut,
;                                     uint16_t* cyclindersOut,
;                                     uint16_t* sectorsOut,
;                                     uint16_t* headsOut);
;       x86_Disk_GetDriveParams

global _x86_Disk_GetDriveParams
_x86_Disk_GetDriveParams:
    push bp
    mov bp, sp

    push es
    push bx
    push si 
    push di

    mov dl, [bp+4]
    mov ah, 0x08
    mov di, 0
    mov es, di

    stc 
    int 0x13

    ; return
    mov ax, 1
    sbb ax, 0           ; 1 on success, 0 on fail

    ; out params

    mov si, [bp+6]      ; drive type from bl
    mov [si], bl        ; drive = bl

    mov bl, ch          ; cylinders = lower bits in ch
    mov bh, cl          ; cylinders = upper bits in cl(6-7)
    shr bh, 6
    mov si, [bp+8]
    mov [si], bx          ; cyclindersOut = bx

    xor ch, ch          ; sectors - lower 5 bits in cl
    and cl, 0x3F
    mov si, [bp+10]
    mov [si], cx        ; sectorsOut = cx

    mov cl, dh          ; head = dh
    mov si, [bp+12]
    mov [si], cx        ; headsOut = dx 

    pop di
    pop si
    pop bx
    pop es

    ; restore old call frame
    mov sp, bp
    pop bp
    ret