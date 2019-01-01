; Ascii Star Wars player for Sharp MZ-800

; mzf header
.db 1           ; type
.db "SWP",13    ; name
.ds 13
.dw endpgm-init ; size
.dw 0c000h      ; start
.dw 0e800h      ; exec
.ds 104         ; comment

.org 0c000h

init:
  ld a,4
  out (0ceh),a  ; dmd 640*200
  ;in a,(0e0h)  ; CGROM and VRAM
  out (0e4h),a  ; CGROM and VRAM
  ld e,' '      ; define ascii chars starting from space

  ld sp,cgdata
  ld a,28
copychars:
  ld c,1
  pop hl
  dec h
  inc h
  jr nz,havechar
  ld c,l
  pop hl
havechar:
  dec sp

  ld h,2
  add hl,hl
  add hl,hl
  add hl,hl
char:
  ld d,0d0h
  ld b,8
  ex af,af'
pattern:
  ld a,(hl)
  inc hl
  ld (de),a
  inc d
  djnz pattern

  ex af,af'
  inc e
  dec c
  jr nz,char

  dec a
  jr nz,copychars

  out (0e0h),a  ; CGROM off

  ; clear screen
  ld b,80h
clrscrn:
  ld (bc),a
  inc bc
  bit 6,b
  jr z,clrscrn

  ld hl,dscroll
  ld bc,04cfh
  otdr

  ; palette
  ld a,1fh
  out (0f0h),a

  ld sp,stack
  ld d,25
  push de
  ld hl,data
  ld c,128
  jp frame

  .db 0c0h,3 ; SOF
dscroll:
  .db 125    ; SW

cgdata:
  .db 0
  .db 9,0, 61h  ; !
  .db 6bh
  .db 6ah
  .db 2fh
  .db 2ah
  .db 2eh
  .db 76h        ; /
  .db 10,0, 20h  ; 0
  .db 4fh

  .db 2ch
  .db 51h
  .db 2bh
  .db 57h
  .db 49h
  .db 55h
  .db 26,0, 01h  ; A
  .db 52h
  .db 77h
  .db 54h

  .db 0beh
  .db 3ch
  .db 0a4h
  .db 26,0, 81h  ; a
  .db 0bch
  .db 79h
  .db 40h
  .db 0a5h


areastart:
  ; hl -data ptr
  ; c -data byte

  ; read next area bbox
  ld a,(hl)   ; x0 + bit islast
  inc hl
  ld (islast),a
  ex af,af'
  ld d,(hl)   ; xs + bit clear
  inc hl
  ld a,(hl)   ; y0/(ys-1)
  and 15
  inc a
  ld e,a      ; e=ys
  ld a,(hl)
  inc hl
  exx
  and 0f0h
  ; set cursor to top left
  ; y0*5*8
  ld h,0
  ld l,a
  ld d,h
  ld e,l
  add hl,hl
  add hl,hl
  add hl,de

  ex af,af'
  add hl,hl
  add hl,hl
  add hl,hl

  rra         ; remove bit 0
  ld d,80h
  ld e,a
  add hl,de   ; +8000h +x0

  ld de,80
  exx

  srl d
  jr c,cleararea

  ; write area
  ld a,128
  sub d
  ld (inextrow+1),a
  ld b,d

  ld sp,-559  ; next cursor pos
  jr nextpos

cleararea:
  ld a,d      ; size x
  ex af,af'

  ld a,e
  rlca        ; size y *8
  rlca
  rlca
  exx
  ; de=80     next row delta

clrline:
  ex af,af'
  ld b,a
  push hl

  srl b
  jr nc,$+4
  ld (hl),d
  inc hl
  jr z,endline
  ld (hl),d
  inc hl
  ld (hl),d
  inc hl
  djnz $-4
endline:

  pop hl
  add hl,de
  ex af,af'
  dec a
  jr nz,clrline
  exx

  jp endarea

  ; ---
nextbitc:
  jr nz,skip
  ld c,(hl)
  inc hl
  rl c
  jr havebitc

nextbit:
  jr z,nextbyte

  ; handle mask bits for skip, skip5, space
complex:
  ; cy=1
  sla c
havebitc:
  jr c,nextbitc

  ; space or skip5
  ; cy=0
  rl c
  jp nz,havebit5
  ; cy=1
  ld c,(hl)
  inc hl
  rl c

havebit5:
  jr nc,skip5       ; 100 -skip5
                    ; 101 -space
  exx
  ld bc,0d000h+' '
  jp printchar      ; exx


nextbyte:
  ; cy=1
  ld c,(hl)
  inc hl
  rl c
  jp havebit

; -----
skip5:
  ; skip five pos
  ld a,18h         ; jr skipmore
  ld (nextpos),a   ; rewrite nextpos
  ld a,-25         ; jr skipmore
  ld (nextpos+1),a
  ld a,4           ; skip 4 later
  jr more

skipmore:
  ld a,(dskip)
  dec a
  jr nz,more
  ld a,0cbh        ; sla c
  ld (nextpos),a
  ld a,21h         ; sla c
  ld (nextpos+1),a
more:
  ld (dskip),a
skip:              ; 11 -skip
  ; skip one pos
  exx
  inc hl
  jr posdone       ; exx

; ===== write at the cursor position
nextpos:
  ; jr skipmore  / jr -25

  ; cy=?
  ; get next mask bit
  sla c
havebit:
  jr c,nextbit   ; not a char
                 ; 0 - char

  ld a,(hl)      ; char to be printed
  inc hl

  exx
  ; ld de,80

  ld c,a         ; de -char bmap ptr
  ld b,0d0h
printchar:
  ld a,(bc)      ; 1
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 2
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 3
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 4
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 5
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 6
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 7
  inc b
  ld (hl),a
  add hl,de
  ld a,(bc)      ; 8
  ld (hl),a

  ; ld sp,-559
  add hl,sp

posdone:
  exx
  djnz nextpos

  ; move to next row
  exx
inextrow:
  ld bc,640     ; next row VRAM
  add hl,bc
  exx
  ld b,d        ; positions remaining in a row
  dec e         ; area height
  ; cy=0
  jr nz,nextpos

  ; end of area
  ld sp,stack-4
endarea:
  ld a,(islast)
  rra
  jp nc,areastart

  ret

; -----
framemark:
  inc a
  jr z,end

  ; set up scrolling
  pop de
  ld d,0
  push de

delay:
  ; read delay
  ld a,(hl)
  inc hl
  cp 254
  jr nc,framemark

  ; wait
  ld de,23c0h
  dec de
  bit 7,d
  jr z,$-3
  dec a
  jr nz,$-9

  ; test scroll
  pop de
  ld a,d
  cp 25
  jr z,noscroll

  inc d
  exx
  ld d,0
  ld e,a
  ; *5*8
  ld h,d
  rlca
  ld l,a
  add hl,hl
  add hl,de
  add hl,hl
  add hl,hl
  add hl,hl

  ld bc,01cfh
  out(c),l
  inc b
  out(c),h
  exx
noscroll:
  push de

frame:
  call areastart
  jr delay

end:
  ld a,8
  out (0d0h),a
waitesc:
  in a,(0d1h)
  inc a
  jr z,waitesc
  jp 0e800h
  

dskip:
  .db 0
endpgm:
islast .equ dskip+1
stack .equ islast+7

data .equ 10


; -----
; data part header
;  ld hl,0c000h
;  ld a,(hl)
;  cp 3eh
;  jp nz,0e800h
;  jp (hl)
; 21 00 C0 7E FE 3E C2 00 E8 E9

; Memory map
; 0000-7fff  data
; 8000-bfff  VRAM 640*200
; c000-      player
; d000-      char bmaps
; e000-ffff  ROM

; ipl:
; out (e0),a ; 0-7fff is RAM
