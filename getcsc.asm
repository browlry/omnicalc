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
csc_Hook:
	add a,e
	cp 1Ah
	jp z,csc_Hook_MirageOSCheck
	ld a,b
	cp skAlpha
	ret nz
	in a,(4)
	bit 3,a
	jp z,csc_APD_Good
	ld a,b
	or a
	ret
csc_APD_Good:
	;Now APD
	ld a,1
	ld (apdTimer),a
	ld (apdSubTimer),a
	xor a
	ret
csc_Hook_MirageOSCheck:
	ld a,(menucurrent)
	cp 2	;Apps menu
	jr nz,csc_Hook_MirageOSCheckBad
	in a,(4)
	bit 3,a
	jr nz,csc_Hook_MirageOSCheckBad
	;Now check for MirageOS
	ld hl,MirageOS
	rst 20h
	B_CALL FindApp
	jr c,csc_Hook_MirageOSCheckBad
	;Now run MirageOS
	ld hl,MirageOS+1
	ld de,progToEdit
	ld bc,8
	ldir
	B_CALL CloseEditEqu
	B_CALL ExecuteApp
csc_Hook_MirageOSCheckBad:
	ld a,1Ah
	or a
	ret
 ;INCLUDE "clipboard.asm"
