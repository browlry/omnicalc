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
Hook_Key_Mode:
	in a,(4)
	bit 3,a
	jp nz,Hook_Key_Mode_ModeReturn
	;res onInterrupt,(iy+onFlags)
	in a,(2)
	rla
	jp nc,Hook_Key_Mode_ModeReturn
	;On pressed and SE. Now check for setting set
	call GetRealSettings
	bit 5,(hl)
	jp z,Hook_Key_Mode_ModeReturn
	pop hl
	;We are go for transfer
	;First we check to see if this is the first time switching states
	ld a,1
	out (20h),a
	di
	;First we put the checksum and stuff on the current page
	ld hl,Hook_Key_Mode_Checksum
	ld de,8000h
	ld bc,17
	ldir
	ld (86ECh),sp
	ld hl,9872h
	call SaveDisplay
	;Now check
	ld a,84h
	out (7),a
	ld hl,8000h
	ld de,Hook_Key_Mode_Checksum
	ld b,17
	call CompareStrings
	jp nz,Hook_Key_Mode_Setup
	ld a,84h
	out (7),a
	ld a,(8020h)
	xor 1
	push af
	ld a,81h
	out (7),a
	pop af
	ld (8020h),a
	;Swap pages
	ld de,8483h
	call CopyRAMPageD_To_E
	ld de,8184h
	call CopyRAMPageD_To_E
	ld de,8381h
	call CopyRAMPageD_To_E
	;Lower page swapped now
	ld de,8583h
	call CopyRAMPageD_To_E
	ld de,8085h
	call CopyRAMPageD_To_E
	;Now we must do a manual copy because of SP issues
	ld hl,8000h
	ld bc,4000h
	ld a,83h
	out (7),a
	ld de,0C000h
	ldir
	ld a,81h
	out (7),a
	if 0
	;Now fix archived variables in the VAT
	;This is new to version 1.2, it now correctly handles them
	;We save some variables in 8700h...
	ld hl,0
	ld (8700h),hl	;the number of bytes to shift up the program section, and the OP stack
	;Let's start by cleaning up the non-(appvar, program, list) entries first..i.e. real, equation, et cetera
	ld hl,symTable
VCalc_SymbolTable_Remove_Loop:
	ld de,(progPtr)
	or a
	sbc hl,de
	jr nz,VCalc_SymbolTable_Remove_Go
	jr VCalc_SymbolTable_Remove_Over
VCalc_SymbolTable_Remove_Go:
	add hl,de
	;Get to the page byte
	ld de,-5
	add hl,de
	ld a,(hl)
	ld de,-4
	add hl,de	;HL is on next entry now
	or a
	jr z,VCalc_SymbolTable_Remove_Loop
	;Archived variable!...kill it
	push hl
	;Calculate how many bytes to shift up
	ld bc,(progPtr)
	or a
	sbc hl,bc		;HL will be the number of bytes
	;HL is the number of bytes to shift...superfluous comments, don't you love them ? :)
	ld a,h
	or l
	jr z,VCalc_SymbolTable_Remove_SkipShift		;skip shift if zero bytes
	ld b,h
	ld c,l
	ld hl,(progPtr)
	ld de,9
	add hl,de
	ld (progPtr),hl
	ld hl,(8700h)
	add hl,de
	ld (8700h),hl		;count how many bytes removed
	pop de
	push de
	ld hl,9
	add hl,de
	ex de,hl
	lddr
VCalc_SymbolTable_Remove_SkipShift:
	pop hl
	jr VCalc_SymbolTable_Remove_Loop
VCalc_SymbolTable_Remove_Over:
	;Now shift up the operator stack and the other portion
	ld bc,(8700h)
	ld a,b
	or c
	jr z,VCalc_SymbolTable_Remove_Over_Skip
	ld hl,(progPtr) ;HL is the highest byte of the area below (well, plus 1)
	ld de,(8700h)
	or a
	sbc hl,de
	ld de,(ops)
	or a
	sbc hl,de
	push hl		;HL is the number of bytes to shift
	ld hl,(progPtr)
	ld de,(8700h)
	or a
	sbc hl,de
	ld de,(progPtr)
	pop bc
	lddr
	ld bc,(8700h)
	ld hl,(pTemp)
	add hl,bc
	ld (pTemp),hl
	ld hl,(opbase)
	add hl,bc
	ld (opbase),hl
	ld hl,(ops)
	add hl,bc
	ld (ops),hl
VCalc_SymbolTable_Remove_Over_Skip:
	;It's time to do the program, list, group, and appvar portion
	ld bc,0
	ld (8700h),bc
	ld hl,(progPtr)
VCalc_ProgTable_Remove_Loop:
	ld de,(pTemp)
	or a
	sbc hl,de
	jr z,VCalc_ProgTable_Remove_Over
	jr c,VCalc_ProgTable_Remove_Over
	add hl,de
	;Get to the page byte
	ld de,-5
	add hl,de
	ld a,(hl)
	dec hl
	ld d,0
	ld e,(hl)
	scf
	sbc hl,de	;HL is on next entry now
	or a
	jr z,VCalc_ProgTable_Remove_Loop
	;Archived variable!...kill it
	push hl
	;E+7 is the length of the variable
	ld bc,(pTemp)
	or a
	sbc hl,bc		;HL will be the number of bytes
	;HL is the number of bytes to shift...superfluous comments, don't you love them ? :)
	ld a,h
	or l
	jr z,VCalc_ProgTable_Remove_SkipShift		;skip shift if zero bytes
	push hl
	ld hl,(pTemp)
	ld bc,7
	add hl,bc
	add hl,de
	ld (pTemp),hl
	pop hl
	push de
	ld b,h
	ld c,l
	ld hl,(8700h)
	add hl,de
	ld de,7
	add hl,de
	ld (8700h),hl		;count how many bytes removed
	pop de
	ld hl,7
	add hl,de
	pop de
	push de
	add hl,de
	ex de,hl
	lddr
VCalc_ProgTable_Remove_SkipShift:
	pop hl
	jr VCalc_ProgTable_Remove_Loop
VCalc_ProgTable_Remove_Over:
	;Now we have to shift up the other stuff, namely the operator stack
	ld bc,(8700h)
	ld a,b
	or c
	jr z,VCalc_ProgTable_Remove_Over_Skip
	ld hl,(pTemp) ;HL is the highest byte of the area below (well, plus 1)
	ld de,(8700h)
	or a
	sbc hl,de
	ld de,(ops)
	or a
	sbc hl,de
	push hl		;HL is the number of bytes to shift
	ld hl,(pTemp)
	ld de,(8700h)
	or a
	sbc hl,de
	ld de,(pTemp)
	pop bc
	ld a,b
	or c
	jr z,VCalc_ProgTable_Remove_Over_Skip
	lddr
VCalc_ProgTable_Remove_Over_Skip:
	;Now we recreate the archived VAT entries
	;Let's find where the correct routine is, because I was too lazy to write my own
	ld hl,VCalc_CreateEntries
	ld de,8700h
	ld bc,VCalc_CreateEntries_End-VCalc_CreateEntries
	ldir
	call 8700h
	endif
	;Now for SP and screen
	ld hl,9872h
	call CopyLayer
	;Number indicating which state
	ld a,(8020h)
	and 1
	xor 1
	add a,'1'
	ld hl,(currow)
	push hl
	ld hl,15*256
	ld (currow),hl
	B_CALL PutMap
	pop hl
	ld (currow),hl
	ld sp,(86ECh)
	xor a
	ei
	ret
Hook_Key_Mode_ModeReturn:
	ld a,kMode
	ld e,a
	or a
	ret
Hook_Key_Mode_Setup:
	xor a
	ld (8020h),a
	ld de,8184h
	call CopyRAMPageD_To_E
	ld de,8085h
	call CopyRAMPageD_To_E
	ld a,81h
	out (7),a
	ld a,'2'
	ld hl,(currow)
	push hl
	ld hl,15*256
	ld (currow),hl
	B_CALL PutMap
	pop hl
	ld (currow),hl
	xor a
	ei
	ret
SaveDisplay:
	di
	ld b,64
	ld a,07h
	out (10h),a
	ld a,80h-1
Slayerloop1:
	push bc
	ld b,0Ch
	inc a
	ld (OP1),a
	call lcddelay
	out (10h),a
	call lcddelay
	ld a,20h
	out (10h),a
	call lcddelay
	in a,(11h)
Slayerloop2:
	call lcddelay
	in a,(11h)
	ld (hl),a
	inc hl
	djnz Slayerloop2
	pop bc
	ld a,(OP1)
	djnz Slayerloop1
	ld a,05h
	call lcddelay
	out (10h),a
	ret
CopyLayer:
	di
	ld b,64
	ld a,07h
	out (10h),a
	ld a,80h-1
layerloop1:
	push bc
	ld b,0Ch
	inc a
	ld (OP1),a
	call lcddelay
	out (10h),a
	call lcddelay
	ld a,20h
	out (10h),a
layerloop2:
	ld a,(hl)
	call lcddelay
	out (11h),a
	inc hl
	djnz layerloop2
	pop bc
	ld a,(OP1)
	djnz layerloop1
	ld a,5
	call lcddelay
	out (10h),a
	ret
lcddelay:
	push af
lcddelayloop:
	in a,(2)
	and 2
	jr z,lcddelayloop
	pop af
	ret
CopyRAMPageD_To_E:
	;Assumes interrupts disabled
	;D is source page
	;E is destination page
	ld hl,8000h
	ld bc,4000h
	ld a,d
	out (7),a
	ld a,e
	res 7,a
	out (5),a
	ld de,0C000h
	ldir
	xor a
	out (5),a
	ret
Hook_Key_Mode_Checksum:
	;Length - 17 bytes
	DB "Save the Manatees"
