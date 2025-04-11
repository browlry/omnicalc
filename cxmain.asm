; Omnicalc v1.27
; (C) 2002-2005 Michael Vincent.
;	Modified in 2025 by Benjamin Allred to remap Up Arrow key to [Entry] on home screen (so it behaves more like a command prompt or shell)
;
;	This program is free software; you can redistribute it and/or modify
;	it under the terms of the Omnicalc license included with this source code.
;
;	This program is distributed in the hope that it will be useful,
;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
cxMain_Hook:
	add a,e
	or a
	jp z,cxMain_BaseDisplay
	cp 3
	jp z,cxMain_BackupRAM
	cp 1
	jp nz,cxMain_Exit
	ld a,b		;load keycode of the key that was pressed b into the accumulator a
	cp kUp ;003h 	; check if up arrow was pressed; if so, 1 is stored as z (because a - 003h = 0, so the zero flag z is set to 1)
	jp z,cxMain_Up_Arrow ; if z is 1, run the cxMain_Up_Arrow function
	cp kClear 	; compare A register with the keycode for [Clear]
	jp z,cxMain_Clear  ; if it matches, call the "clear" function
	cp 86h		;check if right parenthesis was pressed
	jp nz,cxMain_Exit ; if not, then exit
	push bc
	call GetRealSettings ; load settings address into h1
	bit 1,(hl) ; check bit one of the settings byte (located at the address in h1) to see if Parentheses Assistant is turned on; if it's zero, set the z flag.
	ld a,1 ; load one into the accumulator
	jp z,cxMain_Exit2 ; if bit 1 of the settings bit is off, then exit (z flag on means the setting is off)
	push af
	B_CALL RunIndicOff
	pop af
	res evilQuotes,(iy+parens)
	ld c,a
	ld hl,(editCursor)
	ld a,(currow) ; load the cursor row into the accumulator
	add a,a
	add a,a
	add a,a
	add a,a
	ld b,a
	ld a,(curcol) ; load the cursor column into the accumulator
	add a,b
	ld d,0
	ld e,a
	ld (bufferpos),de
cxMain_Search:
	ld de,(editTop)
	or a
	sbc hl,de
	jp z,cxMain_NotValid
	add hl,de
	dec hl
	dec hl
	sbc hl,de
	push af
	add hl,de
	pop af
	jr c,cxMain_Search_1
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr nz,cxMain_Search_1
	dec hl
cxMain_Search_1:
	inc hl
	ld a,(hl)
	cp tRParen
	jp z,cxMain_Search_RParen
	cp tString
	call z,cxMain_Search_Quote
	push bc
	push hl
	B_CALL Get_Tok_Strng
	ld hl,(bufferpos)
	or a
	sbc hl,bc
	ld (bufferpos),hl
	ld hl,OP3
	ld d,b
	ld e,c
	ld a,LlParen
	cpir
	ex de,hl
	pop hl
	pop bc
	jr nz,cxMain_Search
	bit evilQuotes,(iy+parens)
	jr nz,cxMain_Search
	dec c
	jr nz,cxMain_Search
cxMain_Found:
	ld hl,OP3
	ex de,hl
	scf
	sbc hl,de
	ld bc,(bufferpos)
	add hl,bc
	;HL is now the position of the left parenthesis...is it on the screen?
	ld bc,128
	or a
	sbc hl,bc
	ld de,(currow)
	push de
	push af
	add hl,bc
	push hl
	ld a,')'
	set preClrForMode,(iy+newDispF)
	set textInverse,(iy+textFlags)
	B_CALL PutMap
	pop hl
	pop af
	jr nc,cxMain_SkipLeft
	ld a,l
	and 15
	ld (curcol),a
	ld a,l
	srl a
	srl a
	srl a
	srl a
	ld (currow),a
	ld a,'('
	B_CALL PutMap
	ld b,60
	call Pause
	ld a,(tempkey)
	ld (kbdScanCode),a
	res preClrForMode,(iy+newDispF)
	res textInverse,(iy+textFlags)
	ld a,'('
	B_CALL PutMap
cxMain_SkipLeft:
	res preClrForMode,(iy+newDispF)
	res textInverse,(iy+textFlags)
	pop de
	ld (currow),de
cxMain_ExitParens:
	pop bc
	ld a,1
	cp a
	ret
cxMain_Search_RParen:
	ld de,(bufferpos)
	dec de
	ld (bufferpos),de
	bit evilQuotes,(iy+parens)
	jp nz,cxMain_Search
	inc c
	jr z,cxMain_Found
	jp cxMain_Search
cxMain_NotValid:
	bit evilQuotes,(iy+parens)
	jr nz,cxMain_ExitParens
	pop bc
	ld a,1
	or a
	ret
cxMain_Exit3:
	ld a,3
cxMain_Exit:
	cp a
	ret
cxMain_Exit2:
	pop bc
	cp a
	ret
cxMain_Search_Quote:
	ld a,(iy+parens)
	xor 1
	ld (iy+parens),a
	ret
cxMain_Up_Arrow:
	push bc ; add the bc register to the stack (in case it gets changed by GetRealSettings)
	call GetRealSettings ; check the settings and load their *location* into hl (High Low register)
	pop bc ; pop the bc register from the stack
	inc hl ; add 1 to the address stored in hl (so it refers to the next byte?)
	bit 4,(hl) ; check bit 4 of the settings byte ; if it's off (zero), then the zero flag (z) is set to 1.
	jr z,cxMain_Up_Arrow_Exit ; if the setting is off (zero flag is not set), then jump to cxMain_Up_Arrow_Exit
;	jr nz,cxMain_Last_Entry ; if the setting is on (zero flat is set), then jump to cxMain_Last_Entry
;cxMain_Last_Entry:
	ld b,kLastEnt 	; load kLastEnt [Entry] 00Dh key into b register (i.e. return Entry instead of Up arrow)
	ld a,1 ; i don't know what these two lines do. I just copied them from cxMain_Clear_Exit
	cp a
	ret
cxMain_Up_Arrow_Exit:
	ld b,kUp ; load the up arrow key into the b register, so the up arrow key works as it normally does
	ld a,1 ; i don't know what these two lines do. I just copied them from cxMain_Clear_Exit
	cp a
	ret
cxMain_Clear:
	;89-like Clear functionality
	ld hl,(editBtm)
	ld de,(editTail)
	or a
	sbc hl,de
	jr z,cxMain_Clear_Exit	;Cursor is at end of line, let OS handle it
	;Now we have the cursor in the middle. Time to wipe everything after the cursor
	;B_CALL BufLeft
	ld de,(editTail)
	ld a,e
	ld (editBtm),a
	ld a,d
	ld (editBtm+1),a
	B_CALL EraseEOW
	B_CALL DispEOW
	ld b,kClear
	or b
	ret
cxMain_Clear_Exit:
	ld b,kClear
	ld a,1
	cp a
	ret
cxMain_BackupRAM:
	ld a,b ; load the keypress b into the accumulator a
	cp 40h ; compare the accumulator a to 40h kQuit (If a - 40h = 0, set zero flag z to 1)
	jr nz,cxMain_Exit3 ; if the zero flag is not set (z != 0 because a - 40h != 0 because the key pressed was not Quit), then just exit
	in a,(2)
	rla
	jr nc,cxMain_Exit3
	call GetRealSettings
	bit 4,(hl) ; check if "Ram Recovery" setting is on; if it's off, then set the z flag
	jr z,cxMain_Exit3 ; if the setting is off (z is set), then exit
	;Calculator turning off, let's backup RAM.
	di
	ld a,1
	out (20h),a
	ld (savesscreen),sp
	;ld hl,appbackupscreen
	;call SaveDisplay
	ld hl,cxMain_BackupRAM_Checksum
	ld de,8000h
	ld bc,16
	ldir
	;Now copy upper checksum
	ld hl,0C000h
	ld bc,4000h
	ld ix,0
	ld d,0
cxMain_BackupRAM_ChecksumL:
	ld e,(hl)
	add ix,de
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,cxMain_BackupRAM_ChecksumL
	ld (8050h),ix
	ld a,87h
	out (7),a
	ld hl,0C000h
	ld de,08000h
	ld bc,04000h
	ldir
	;Now to copy lower page
	ld de,8186h
	call CopyRAMPageD_To_E
	ld a,81h
	out (7),a
	ei
	ret
cxMain_BackupRAM_Checksum:
	DB "OmnicalcBackupOK"
cxMain_Thousands:
	inc hl
	bit 1,(hl)
	ret z
	ld a,(iy+fmtFlags)
	and 3
	jp nz,cxMain_Thousands_Exit
	ld a,(OP1)
	cp RealObj
	jp nz,cxMain_Thousands_Exit
	ld a,(OP1+1)
	cp 83h
	jp c,cxMain_Thousands_Exit
	cp 89h+1
	jp nc,cxMain_Thousands_Exit
	ld a,15
	B_CALL FormReal
	res 0,(iy+asm_Flag1)
	ld hl,OP3-1
	add hl,bc
	ld a,'.'
	push hl
	push bc
	cpdr
	pop bc
	pop hl
	jr z,$F
	set 0,(iy+asm_Flag1)
$$:
	ld de,appbackupscreen+50
	ld bc,0
cxMain_Thousands_Loop:
	ld a,(hl)
	cp 2Eh
	jr nz,$F
	ld a,3Ah
	set 0,(iy+asm_Flag1)
	ld c,-1
$$:
	bit 0,(iy+asm_Flag1)
	jr z,$F
	inc c
	bit 2,c
	jr z,$F
	ld c,a
	ld a,tComma
	ld (de),a
	dec de
	ld a,c
	ld c,1
	inc b
$$:
	ld (de),a
	inc b
	dec hl
	dec de
	push de
	ld de,OP3-1
	or a
	sbc hl,de
	push af
	add hl,de
	pop af
	pop de
	jr nz,cxMain_Thousands_Loop
	ld l,b
	ld h,0
	push hl
	B_CALL ZeroOP1
	ld a,StrngObj
	ld (OP1),a
	ld a,24h
	ld (OP1+1),a
	ld hl,(pTempCnt)
	inc hl
	ld (pTempCnt),hl
	dec hl
	ld ix,OP1
	ld (ix+2),l
	ld (ix+3),h
	pop hl
	push hl
	ld a,16
	sub l
	push af
	ld l,16
	B_CALL CreateStrng
	ex de,hl
	inc hl
	inc hl
	pop af
	or a
	jr z,cxMain_Thousands_1
	ld b,a
$$:
	ld (hl),tSpace
	inc hl
	djnz $B
cxMain_Thousands_1:
	pop bc
	dec hl
	add hl,bc
	ex de,hl
	ld hl,appbackupscreen+50
	lddr
	B_CALL OP4ToOP1
	xor a
	ret
cxMain_BaseDisplay:
	res LinkHook,(iy+Hooks3)			;for play() function
	in a,(2)
	rla
	jr nc,cxMain_BaseDisplay2
	xor a
	out (0),a
	in a,(8)
	or a
	jr	z,cxMain_BaseDisplay2
	in a,(9)
	xor a
	out (8),a
cxMain_BaseDisplay2:
	ld a,(0FE67h)
	cp 0C9h
	jr nz,cxMain_BaseDisplay_NotSpecial
	xor a
	ld (0FE67h),a
	ld a,(getKeyHookPtr+3)
	or a
	jr z,cxMain_BaseDisplay_NotSpecial
	cp 2
	jr nc,$F
	ld a,10
$$:
	cp 37
	jr c,$F
	ld a,10
$$:
	ld (newbase),a
	xor a
	ld (getKeyHookPtr+3),a
	jr cxMain_TemporaryPoint
cxMain_BaseDisplay_NotSpecial:
	xor a
	ld (getKeyHookPtr+3),a
	ld de,appbackupscreen
	B_CALL MovFrOP1
	call GetRealSettings
	push hl
	ld hl,appbackupscreen
	rst 20h
	pop hl
	bit 6,(hl) ; check if bit 6 (the Base Conversion setting) is on. If it's zero, set the z flag.
	jp z,cxMain_Thousands ; if z flag is on (because the base conversion setting is off), jump to cxMain_Thousands
	inc hl
	inc hl
	ld a,(hl)
	ld (newbase),a
	dec hl
	dec hl
	cp 10
	jp z,cxMain_Thousands
	;We are good.
	;OP1 contains result.
	ld a,(OP1)
	cp RealObj
	jp nz,cxMain_Thousands
	B_CALL CkPosInt
	jp nz,cxMain_Thousands
cxMain_TemporaryPoint:
	call EConvOP1
	jp c,Parse_Error_Domain
	jr cxMain_BaseOutput_1
cxMain_BaseOutput_EntryPoint:
	;DE is number
	ld (oldnum),hl
	ld (oldnum+2),de
cxMain_BaseOutput_1:
	;Now start converting
	xor a
	ld (strlen),a
	ld ix,tempstring
cxMain_BaseDisplay_Build:
	ld a,(newbase)
	ld hl,(oldnum)
	ld de,(oldnum+2)
	call Play_Divide32
	ld (oldnum),hl
	ld (oldnum+2),de
	ld c,a
	ld a,h
	or l
	or e
	or d
	jr z,cxMain_BaseDisplay_Done
	;Now turn A into digit.
	ld b,0
	ld hl,Parse_Base_Table
	add hl,bc
	ld a,(hl)
	ld (ix),a
	inc ix
	ld hl,strlen
	inc (hl)
	jr cxMain_BaseDisplay_Build
cxMain_BaseDisplay_Done:
	ld b,0
	ld hl,Parse_Base_Table
	add hl,bc
	ld a,(hl)
	ld (ix),a
	inc ix
	ld hl,strlen
	inc (hl)
	ld a,(strlen)
	or a
	jp z,cxMain_BaseDisplay_Zero
	res appendbase,(iy+baseflags)
	ld a,(newbase)
	ld hl,cxMain_BaseDisplay_Bases
	ld bc,6
	cpir
	call z,cxMain_BaseAppend
	B_CALL ZeroOP1
	ld a,StrngObj
	ld (OP1),a
	ld a,24h
	ld (OP1+1),a
	ld hl,(pTempCnt)
	inc hl
	ld (pTempCnt),hl
	dec hl
	ld ix,OP1
	ld (ix+2),l
	ld (ix+3),h
	ld a,(strlen)
	ld h,0
	ld l,a
	bit appendbase,(iy+baseflags)
	jr z,cxMain_BaseDisplay_Create
	inc hl
	inc hl
cxMain_BaseDisplay_Create:
	ld a,(strlen)
	cp 16
	jr nc,cxMain_BaseDisplay_Create2
	ld b,a
	ld a,16
	sub b
	ld b,0
	ld c,a
	bit appendbase,(iy+baseflags)
	jr z,cxMain_BaseDisplay_Create_Go
	dec c
	dec a
cxMain_BaseDisplay_Create_Go:
	add hl,bc
	push af
	B_CALL CreateStrng
	inc de
	inc de
	pop bc
	ld a,b
	or a
	jr z,cxMain_BaseDisplay_CreateGo
cxMain_BaseDisplay_Fill:
	ld a,tSpace
	ld (de),a
	inc de
	djnz cxMain_BaseDisplay_Fill
	jr cxMain_BaseDisplay_CreateGo
cxMain_BaseDisplay_Create2:
	jp Parse_Error_Domain
	;bit appendbase,(iy+baseflags)
	;jr nz,cxMain_BaseDisplay_Create2_2
	;res appendbase,(iy+baseflags)
	;dec hl
	;dec hl
;cxMain_BaseDisplay_Create2_2:
;	B_CALL CreateStrng
;	inc de
;	inc de
cxMain_BaseDisplay_CreateGo:
	ld hl,tempstring
	ld a,(strlen)
	dec a
	ld c,a
	ld b,0
	add hl,bc
	ld b,a
	inc b
cxMain_BaseDisplay_InvertCopy:
	ld a,(hl)
	ld (de),a
	dec hl
	inc de
	djnz cxMain_BaseDisplay_InvertCopy
	;Now for base append
	bit appendbase,(iy+baseflags)
	jr z,cxMain_BaseDisplay_AllFinished
	ld a,(newbase)
	ld hl,cxMain_BaseDisplay_Bases
	ld bc,6
	cpir
	ld bc,5
	add hl,bc
	ld a,0BBh
	ld (de),a
	inc de
	ld a,(hl)
	ld (de),a
cxMain_BaseDisplay_AllFinished:
	B_CALL OP4ToOP1
	xor a
	ret
cxMain_BaseDisplay_Zero:
	B_CALL OP1Set0
cxMain_Thousands_Exit:
	xor a
	ret
cxMain_BaseDisplay_Bases:
	DB 2,3,5,8,10,16
	DB tLb,tLsmallt,tLq,tLo,tLd,tLh
cxMain_BaseAppend:
	set appendbase,(iy+baseflags)
	ret
DivideHLBy6A:
	ld bc,-1
	ld d,0
	ld e,a
DivideHLByA_Loop:
	inc bc
	or a
	sbc hl,de
	jr nc,DivideHLByA_Loop
	add hl,de
	;HL is remainder.
	;BC is the division number
	ld a,l
	ld h,b
	ld l,c
	;A is remainder
	;HL is the part that could not be divided
	ret
cxMain_BaseDisplay_OSHandle:
	xor a
	ret
EConvOP1:
	;Gets a number out of OP1
	;Returns C if the number was too large
	ld b,10
	ld hl,0
	ld (oldnum),hl
	ld (oldnum+2),hl
EConvOP1_Loop:
	push bc
	xor a
	ld hl,OP1+6
	rld
	dec hl
	rld
	dec hl
	rld
	dec hl
	rld
	dec hl
	rld
	call EConvOP1_Call
	jr c,EConvOP1_Return
	push hl
	ld hl,(oldnum)
	push hl
	call EConvOP1_Call
	jr c,EConvOP1_Return3
	call EConvOP1_Call
	jr c,EConvOP1_Return3
	ld de,(oldnum)
	pop hl
	add hl,de
	ld (oldnum),hl
	ld de,(oldnum+2)
	pop hl
	adc hl,de
	ld (oldnum+2),hl
	jr c,EConvOP1_Return
	;HL is times 10
	ld b,0
	ld c,a
	ld hl,(oldnum)
	add hl,bc
	ld (oldnum),hl
	ld hl,(oldnum+2)
	ld bc,0
	adc hl,bc
	ld (oldnum+2),hl
	jr c,EConvOP1_Return
	ld hl,OP1+1
	pop bc
	dec (hl)
	ret p
	djnz EConvOP1_Loop
	scf
	ret
EConvOP1_Call:
	ld hl,(oldnum)
	add hl,hl
	ld (oldnum),hl
	ld hl,(oldnum+2)
	adc hl,hl
	ld (oldnum+2),hl
	ret
EConvOP1_Return3:
	pop bc
EConvOP1_Return2:
	pop bc
EConvOP1_Return:
	pop bc
	ret
