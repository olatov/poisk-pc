mov ax, cs
add ax, 0x10
mov ds, ax
mov es, ax
mov ss, ax
mov sp, 0xfffe 
push ax
xor ax, ax
push ax
retf
