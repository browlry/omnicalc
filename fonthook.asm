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
Font_Hook:
	add a,e
	cp 1
	jp nz,Font_Hook_Small
	ld c,b
	;fontHookPtr+3 - lower byte
	;cxMainHookPtr+3 - upper byte
	ld hl,fontHookPtr+3
	ld e,(hl)
	ld hl,cxMainHookPtr+3
	ld d,(hl)
	ld hl,FontSet_Check
	ld b,13
	call CompareStrings
	push de
	jp nz,Font_Hook_Reget
Font_Hook_Reloop:
	ld h,0
	ld l,c
	ld d,h
	ld e,l
	add hl,hl
	add hl,hl
	add hl,hl
	or a
	sbc hl,de
	ex de,hl
	pop hl
	add hl,de
	push bc
	ld de,lFont_record
	ld bc,7
	ldir
	ld hl,lFont_record
	pop bc
	ld b,c
	xor a
	ret
Font_Hook_Reget:
	pop de
	push bc
	call GetRealSettings
	ld de,4
	add hl,de
	rst 20h
	B_CALL ChkFindSym
	jr c,Font_Hook_Remove
	ld a,b
	or a
	jr nz,Font_Hook_Remove
	ld hl,fontHookPtr+3
	ld (hl),e
	ld hl,cxMainHookPtr+3
	ld (hl),d
	pop bc
	ld hl,13
	add hl,de
	push hl
	jr Font_Hook_Reloop
Font_Hook_Remove:
	call GetRealSettings_Unarchive
	inc hl
	inc hl
	inc hl
	res 0,(hl)
	res FontHook,(iy+Hooks2)
	pop bc
	xor a
Font_Hook_Small:
	inc a
	ret

