bits 16
org 0 

; -------------------------------------------------
; BIOS entry point (where reset vector will jump)
; -------------------------------------------------
bios_entry:
    sti
    mov al, 0x68
    out 0x68, al

    cld
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

    hlt    

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
