; Omnicalc v1.26
; (C) 2002-2005 Michael Vincent.
;
;	This program is free software; you can redistribute it and/or modify
;	it under the terms of the Omnicalc license included with this source code.
;
;	This program is distributed in the hope that it will be useful,
;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
 INCLUDE "parser.asm"
 INCLUDE "token.asm"
 INCLUDE "cxmain.asm"
 INCLUDE "keyhook.asm"
 INCLUDE "getcsc.asm"
 INCLUDE "fonthook.asm"
 ;INCLUDE "character.asm"
manateemessagelen EQU 38
	if 0
Manatee_Hook:
	add a,e
	push af
	push bc
	ld a,b
	inc a
	jr nz,Manatee_Hook_Exit
	ld a,(manateebyte)
	inc a
	ld (manateebyte),a
	cp manateemessagelen+1
	jr nc,Manatee_TooHigh
	ld d,0
	ld e,a
	ld hl,Manatee_Message
	add hl,de
	ld a,(hl)
	set textInverse,(iy+textFlags)
	B_CALL PutMap
	res textInverse,(iy+textFlags)
Manatee_Hook_Exit:
	pop bc
	pop de
	xor a
	ld a,b
	ret
Manatee_TooHigh:
	res CursorHook,(iy+Hooks1)
	jr Manatee_Hook_Exit
Manatee_Message:
	DB " SAVE THE MANATEE CLUB:"
	DB "  "
	DB "1-800-432-JOIN"
	endif

