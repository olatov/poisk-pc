bits 16
org 0 

; -------------------------------------------------
; BIOS entry point (where reset vector will jump)
; -------------------------------------------------
bios_entry:
    sti
    mov al, 0x69
    out 0x68, al

    ; clear screen
    mov ax, 0xbc00
    mov es, ax
    xor di, di
    xor ax, ax
    mov cx, 16384
    cld    
    rep stosw

    ; print text
    mov ax, cs
    mov ds, ax
    mov si, even_blob
    mov ax, 0xbc00
    mov es, ax
    xor di, di
    mov cx, 160
    rep movsw
    mov si, odd_blob
    mov di, 0x2000
    mov cx, 160
    rep movsw

    ; color bars
    mov dx, 160 * 6;  6 lines

    mov di, 320 * 7
    mov bx, di
    mov ax, 0x5555  ; color 1
    mov cx, dx
    rep stosw
    mov di, bx
    add di, 0x2000
    mov cx, dx
    rep stosw

    sub di, 0x2000
    mov bx, di
    mov ax, 0xAAAA  ; color 2
    mov cx, dx
    rep stosw
    mov di, bx
    add di, 0x2000
    mov cx, dx
    rep stosw

    sub di, 0x2000
    mov bx, di
    mov ax, 0xFFFF  ; color 3
    mov cx, dx
    rep stosw
    mov di, bx
    add di, 0x2000
    mov cx, dx
    rep stosw

    mov al, 0x69
repeat:
    mov bl, 8
L0: 
    mov cx, 0xFFFF
L1: loop L1 
    dec bl
    jnz L0

    xor al, 0x20
    out 0x68, al    
    jmp repeat

even_blob:
    incbin "even.bin"

odd_blob:
    incbin "odd.bin"

; -------------------------------------------------
; Pad BIOS until reset vector
; -------------------------------------------------
times 0x1FF0 - ($ - $$) db 0

; -------------------------------------------------
; Reset vector at FFFF:0000 (physical 0xFFFF0)
; -------------------------------------------------
reset_vector:
    jmp 0xFE00:bios_entry
    nop                 ; padding (classic BIOS style)

; -------------------------------------------------
; Pad to exactly 8 KB
; -------------------------------------------------
times 0x2000 - ($ - $$) db 0
