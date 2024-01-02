org 0x7c00
bits 16

%define ENDL 0x0D,0x0A

;
; fat12 header
;

;
;https://wiki.osdev.org/FAT
;
; BIOS Parameter Block 
;
jmp short start
nop

bpb_oem:                        db 'MSWIN4.1'  ;8 bytes
bpb_bytes_per_sector:           dw 0x200       
bpb_sectors_per_cluster:        db 1
bpb_reserved_sectors:           dw 1
bpb_fat_count:                  db 2
bpb_dir_entries_count:          dw 0xE0
bpb_total_sectors:              dw 2880         ;2880 * 512B = 1.44MB
bpb_media_descriptor_type:      db 0xF0
bpb_sectors_per_fat:            dw 9
bpb_sectors_per_track:          dw 18
bpb_heads:                      dw 2
bpb_hidden_sectors:             dd 0
bpb_large_sector_count:         dd 0


;
; Extended Boot Record
;

ebr_driver_number:              db 0
ebr_flags_in_winnt:             db 0
ebr_signature:                  db 0X29
ebr_volume_id:                  db 0x22,0x03,0x03,0x20
ebr_volume_label_string:        db 'fat12 byzyf'
ebr_system_id_string:           db 'FAT32   '

;
; Boot code
;

start:
    ; setup ds, es
    mov ax, 0
    mov ds, ax
    mov es, ax

    ; setup ss-sp
    mov ss, ax
    mov sp, 0x7C00

    push es
    push word .after
    retf

.after:
    ; BIOS should set DL to the drive number
    mov [ebr_driver_number], dl

    ; show loading message
    mov si, msg_loading
    call puts

    ; read dirver parameters
    ; instead of relying on data on formatted disk

    push es
    mov ah, 0x08
    int 0x13
    jc floppyy_error
    pop es

    and cl, 0X3F                        ; CL(lower 6 bits) = sectors per track
    xor ch, ch                          ; CH = 0
    mov [bpb_sectors_per_track], cx     ; sector count

    inc dh                          ; DH = number of sides (0 based)
    mov [bpb_heads], dh             ; head count

    ; LBA of the root dir = reserved + fats_cnt * sectors_per_fat
    ; 
    mov ax, [bpb_sectors_per_fat]   ; ax = sectors_per_fat
    mov bl, [bpb_fat_count]         ; bl = fats_cnt
    xor bh, bh                      ; bh = 0
    mul bx                          ; DX:AX = AX * BX 
                                    ; AX = sectors_per_fat * fats_cnt
                                    ; ???

    add ax, [bpb_reserved_sectors]  ; ax += reserved_sectors

    push ax                         ; ax = LBA of root dir

    mov ax, [bpb_dir_entries_count] ; 
    shl ax, 5                       ; ax*= 32 (the size of per dir entry is 32B)
    xor dx, dx                      ; 

    div word [bpb_bytes_per_sector] ; ax = the number of dir sectors to read
                                    ; dx = the remaind byte
    test dx, dx
    jz .root_dir_after
    inc ax

.root_dir_after:
    ; read root dir
    mov cl, al                      ; cl = number of sectors to read
    pop ax                          ; ax = LBA of root dir
    mov dl, [ebr_driver_number]     
    mov bx, buffer                  ; es:bx = buffer
    call disk_read
    xor bx, bx
    mov di, buffer

.search_kernel:
    mov si, file_kernel_bin
    mov cx, 11
    push di
    repe cmpsb
    pop di                      ; 0x7cae
    je .found_kernel

    add di, 32
    inc bx
    cmp bx, [bpb_dir_entries_count]
    jl .search_kernel

    ; not found kernel
    jmp kernel_not_found_error

.found_kernel:
    ; di should have the address to the entry
    mov ax, [di+26]                 ; first logical cluster field(offset 26) ;0x7cbd
    mov [stage2_cluster], ax

    ; load FAT from disk into memory
    mov ax, [bpb_reserved_sectors]
    mov bx, buffer
    mov cl, [bpb_sectors_per_fat]
    mov dl, [ebr_driver_number]

    call disk_read                  ; 0x7ccd

    ; read kernel and process FAT chain
    mov bx, KERNEL_LOAD_SEGMENT
    mov es, bx
    mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:
    ; read next cluster
    mov ax, [stage2_cluster]

    ; note : hardcoded value

    add ax, 31                      ; first cluster = (kernel_cluster - 2) * serctors_per_cluster + start_sector
                                    ; start sector = reserved + fats + root dir = 1 + 18 + 134 = 33
    
    mov cl, 1
    mov dl, [ebr_driver_number]
    call disk_read
    add bx, [bpb_bytes_per_sector]

    ; compute location of next cluster
    mov ax, [stage2_cluster]
    mov cx, 3
    mul cx 
    mov cx, 2
    div cx

    mov si, buffer
    add si, ax
    mov ax, [ds:si]
    or dx, dx
    jz .even

.odd:
    shr ax, 4
    jmp .next_cluster_after

.even:
    and ax, 0X0FFF
.next_cluster_after:
    cmp ax, 0X0FF8
    jae .read_finish
    
    mov [stage2_cluster], ax
    jmp .load_kernel_loop

.read_finish:
    ; jump to our kernel

    mov dl, [ebr_driver_number]
    mov ax, KERNEL_LOAD_SEGMENT
    mov ds, ax
    mov es, ax

    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET      ; enter the kernel

    jmp wait_key_and_reboot     ; should never happen

    cli 
    hlt

;
; Error handlers
;
floppyy_error:
    mov si,msg_read_failed
    call puts
    jmp wait_key_and_reboot

kernel_not_found_error:
    mov si, msg_kernel_not_found
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 0x16
    jmp 0xFFFF:0                    ; jmp to the Beginning os BIOS          

.halt:
    cli                             ; Clear Interrupt Flag
    hlt                             ; halt until get interrupt or reset signal


; Prints a string to the screen
; Params:
;   ds:si points to string
puts:
    push si
    push ax
    push bx

.loop:
    lodsb                   ;load next character in al
    or al, al               
    jz .done

    mov ah, 0x0e       
    mov bh, 0     
    int 0x10                ; INT10,E
                            ; AH = 0E
                            ; AL = ASCII character to write
                            ; BH = page number (text modes)
	                        ; BL = foreground pixel color (graphics modes)
    jmp .loop  

.done:
    pop bx
    pop ax
    pop si
    ret


;
; Disk routines
;
; translate LBA to CHS
;
; cylinder (10bits) = (LBA / sectors per track) / heads
; head     (8bits)  = (LBA / sectors per track) % heads
; sector   (6bits)  = (LBA % sectors per track) + 1
;
; LBA       -> ax
; cylinder  -> cx[6-15bit]
; head      -> dh
; sector    -> cx[0-5bit]


lba_to_chs:
    push ax
    push dx 

    xor dx, dx                          ; dx = 0 
    div word [bpb_sectors_per_track]    ; DX-AX / [MEM] = AX ... DX
                                        ; ax = LBA / sectors per track
                                        ; dx = LBA % sectors per track
    inc dx                              ; dx = (LBA % sectors per track) + 1 = sector
    mov cx, dx                          ; cx = dx = sector

    xor dx, dx                          ; dx = 0
    div word [bpb_heads]                ; ax = (LBA / sectors per track) / heads = cylinder
                                        ; dx = (LBA / sectors per track) % heads = head
    
    mov dh, dl                          ; dl = head
    mov ch, al                          ; ch = cylinder (lower 8 bits)
    shl ah, 6                           ;
    or cl, ah                           ; put upper 2 bits of cylinder in CL

    pop ax
    mov dl, al
    pop ax
    ret

;
; Read sectors from a fist
; 
; ax: LBA 
; cl: number of sectors to read(up to 128)
; dl: driver number
; es:bx: memory address where to store read data

disk_read:

    push ax
    push bx
    push cx
    push dx
    push di


    push cx                             ; temporarily save CL(number of sector()s to read
    call lba_to_chs
    pop ax                              ; AL = number of sectors to read

    mov ah, 0x02
    mov di, 3


.retry:
    pusha                                   
    stc                                 ; set CF = 1
    int 0x13                            ; CF == 0, successful
    jnc .done                           ; if CF == 0, jmp .done 
    
    ; read failed
    popa
    call disk_reset

    dec di
    test di, di
    jnz .retry

.fail:
    jmp floppyy_error

.done:
    popa
    pop di
    pop dx
    pop cx
    pop bx
    pop ax 
    ret

disk_reset:
    pusha
    mov ah, 0x0
    stc
    int 0x13
    jc floppyy_error
    popa
    ret


msg_loading:                            db 'Loading...', ENDL, 0
msg_read_failed:                        db 'Read from disk failed!', ENDL, 0
msg_kernel_not_found:                   db 'STAGE2.BIN file not found', ENDL, 0
file_kernel_bin:                        db 'STAGE2  BIN'
stage2_cluster:                         dw  0

KERNEL_LOAD_SEGMENT:                    equ 0x2000
KERNEL_LOAD_OFFSET:                     equ 0

times 510-($-$$)                    db 0
dw 0xAA55

buffer: