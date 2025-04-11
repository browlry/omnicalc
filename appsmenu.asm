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
QApps:
	in a,(2)
	rla
	jp nc,Hook_Key_Apps_Return
	;It is a SE...
	ld a,1
	out (20h),a
	di
	;ld a,82h
	;out (7),a
	ld hl,QApps_Detect
	ld de,appbackupscreen
	ld bc,QApps_Detect_End-QApps_Detect
	ldir
	ld de,appnames+4000h
	ld a,2
	out (5),a
	xor a
	ld (numapps+4000h),a
	call appbackupscreen
	;Now let's try the bubblesort!
	xor a
	out (5),a
	ld a,82h
	out (7),a
	ld (backup_sp),sp
	ld a,(numapps)
	cp 2
	jr c,QApps_Sorted
	dec a
	ld c,a
QApps_OuterLoop:
	ld sp,apppointers
	ld b,c
QApps_InnerLoop:
	pop de
	pop hl
	push hl
QApps_CompareLoop:
	ld a,(de)
	cp (hl)
	inc hl
	inc de
	jr z,QApps_CompareLoop
	jr c,QApps_InnerLoop_1
	dec sp
	dec sp
	pop hl
	ex (sp),hl
	push hl
	pop hl
QApps_InnerLoop_1:
	djnz QApps_InnerLoop
	dec c
	jr nz,QApps_OuterLoop
QApps_Sorted:
	ld sp,(backup_sp)
	ld a,81h
	out (7),a
	B_CALL RunIndicOff
	B_CALL CursorOff
	ld hl,appbackupscreen
	call SaveDisplay
	B_CALL ClrLCDFull
	res indicrun,(iy+indicflags)
	ld a,(iy+appflags)
	ld (tempflags),a
	res apptextsave,(iy+appflags)
	call QApps_InitializeMenu
	di
	ld a,82h
	out (7),a
	ld a,(numapps)
	ld b,a
	ld a,81h
	out (7),a
	ld a,b
	dec a
	bit 3,(iy+asm_Flag1)
	jr z,$F
	inc a
$$:
	ld (enditem),a
	ld hl,(currow)
	ld (originalcur),hl
	ld hl,0
	ld (currow),hl
	ld hl,QApps_Menu_Title
	set textInverse,(iy+textFlags)
	call putstr
	res textInverse,(iy+textFlags)
QApps_Loop_Refresh:
	call QApps_Redisplay
QApps_Loop:
	set 0,(iy+onFlags)
	set 7,(iy+28h)
	B_CALL GetKey
	res 7,(iy+28h)
	res 0,(iy+onFlags)
	ld ix,QApps_Loop
	cp kOff
	jr z,QApps_PowerOff
	cp kClear
	jr z,QApps_Special_Exit
	cp kAppsMenu
	jr z,QApps_RepeatMenu
	cp kDown
	jr z,QApps_Down
	cp kUp
	jr z,QApps_Up
	cp kAlphaDown
	jp z,QApps_AlphaDown
	cp kAlphaUp
	jr z,QApps_AlphaUp
	cp kEnter
	jp z,QApps_Enter
	cp kCapA
	jr c,$F
	cp kCapZ+1
	jr nc,$F
	jp QApps_Letter
$$:
	cp k9+1
	jr nc,$F
	cp k0
	jr c,$F
	jr nz,QApps_Loop_1
	ld a,k1+9
QApps_Loop_1:
	sub k1
	ld (curitem),a
	jr QApps_Enter
$$:
	ld hl,QApps_SpecialKeys
	ld c,(hl)
	inc hl
	ld b,0
	cpir
	jr nz,QApps_Loop
	or a
	pop hl
QApps_Special_ExitCode:
	ld hl,(originalcur)
	ld (currow),hl
	res 0,(iy+asm_Flag1)
	ei
	push af
	ld hl,appbackupscreen
	di
	call CopyLayer
	ld a,(tempflags)
	ld (iy+appflags),a
	pop af
	ret
QApps_Special_Exit:
	pop hl
	call QApps_Special_ExitCode
	;B_CALL MenCatRet
	ld a,(menucurrent)
	or a
	ld a,0
	ret z
	ld a,kClear
	ret
QApps_PowerOff:
	call QApps_Special_ExitCode
	B_CALL PowerOff
QApps_RepeatMenu:
	call QApps_Special_ExitCode
	jp Hook_Key_Apps
QApps_Down:
	call Entries_MoveDown
	jp QApps_Loop_Refresh
QApps_Up:
	call Entries_MoveUp
	jp QApps_Loop_Refresh
QApps_AlphaUp:
	ld b,6
	set 2,(iy+asm_Flag1)
$$:
	push bc
	call Entries_MoveUp_Special
	pop bc
	djnz $B
	res 2,(iy+asm_Flag1)
	jp QApps_Loop_Refresh
QApps_AlphaDown:
	ld b,6
	set 2,(iy+asm_Flag1)
$$:
	push bc
	call Entries_MoveDown_Special
	pop bc
	djnz $B
	res 2,(iy+asm_Flag1)
	jp QApps_Loop_Refresh
QApps_Enter:
	ld a,(curitem)
	bit 3,(iy+asm_Flag1)
	jr z,$F
	dec a
	cp 0FFh
	jr nz,$F
	;Run Finance!
	ld hl,(originalcur)
	ld (currow),hl
	ld hl,appbackupscreen
	di
	call CopyLayer
	ld a,(tempflags)
	ld (iy+appflags),a
	res 0,(iy+asm_Flag1)
	ld a,kFin
	pop hl
	or a
	ei
	ret
$$:
	ld e,a
	di
	ld a,2
	out (5),a
	ld hl,apppointers+4000h
	ld d,0
	sla e
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	set 6,h
	ld l,a
	ld de,progToEdit
	ld bc,8
	ldir
	xor a
	out (5),a
	ei
	ld hl,(originalcur)
	ld (currow),hl
	ld a,(tempflags)
	ld (iy+appflags),a
	ld a,(menucurrent)
	or a
	jr z,QApps_Enter_NoMenu
	ld a,kClear
 	B_CALL MenCatRet
QApps_Enter_NoMenu:
	B_CALL PutAway
	B_CALL CloseEditEqu
	ld a,kExtApps
	ld (cxcurapp),a
	B_CALL ExecuteApp
	B_CALL Mon
	B_CALL JForceCmdNoChar
QApps_Letter:
	add a,('A'-kCapA)
	ld c,a
	di
	ld a,82h
	out (7),a
	ld hl,apppointers
	ld a,(numapps)
	ld b,a
QApps_Letter_Search:
	push bc
	push hl
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld a,c
	cp (hl)
	jr z,QApps_Letter_Match
	jr c,QApps_Letter_Match
	pop hl
	inc hl
	inc hl
	pop bc
	djnz QApps_Letter_Search
	ld a,81h
	out (7),a
	ld a,(enditem)
	jr QApps_Letter_1
QApps_Letter_Match:
	pop hl
	pop bc
	ld a,(numapps)
	sub b
QApps_Letter_1:
	ld b,a
	ld a,81h
	out (7),a
	bit 3,(iy+asm_Flag1)
	jr z,$F
	inc b
	ld a,(enditem)
	cp b
	jr nc,$F
	dec b
$$:
	ld a,b
	ld (curitem),a
	ld (menutop),a
	ld b,a
	ld a,1
	ld (itemrow),a
	ld a,(enditem)
	sub 6
	cp b
	jr nc,$F
	ld (menutop),a
	ld b,a
	ld a,(curitem)
	sub b
	inc a
	ld (itemrow),a
$$:
	jp QApps_Loop_Refresh
QApps_Redisplay:
	res 0,(iy+asm_Flag1)
	set 1,(iy+asm_Flag1)
	di
	call SuperMenu
	ei
	ret
QApps_Menu_Title:
	DB "APPLICATIONS",0
	jp QApps_Exit
QApps_Detect:
	in a,(6)
	push af
	ld b,69h
	in a,(2)
	bit 5,a
	jr z,$F
	in a,(21h)
	and 3
	jr nz,$F
	ld b,29h
$$:
	ld a,b
	ex af,af'
QApps_Detect_Loop:
	ex af,af'
	cp 0Ch
	jr c,QApps_Detect_Over
	out (6),a
	dec a
	ex af,af'
	ld hl,4000h
	ld a,(hl)
	cp 80h
	jr nz,QApps_Detect_Loop
	inc hl
	ld a,(hl)
	cp 0Fh
	jr nz,QApps_Detect_Loop
	ld bc,100
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	ld a,80h
	cp (hl)
	jr nz,QApps_Detect_Loop
$$:
	ld a,80h
	cpir
	ld a,(hl)
	cp 41h
	jr c,$B
	cp 48h+1
	jr nc,$B
	sub 40h
	ld c,a
	push bc
	inc hl
	push hl
	ld a,(numapps+4000h)
	add a,a
	ld c,a
	ld b,0
	ld hl,apppointers+4000h
	add hl,bc
	ld (hl),e
	inc hl
	res 6,d
	ld (hl),d
	set 6,d
	ld hl,numapps+4000h
	inc (hl)
	pop hl
	pop bc
	push bc
	ld b,0
	ldir
	pop bc
	ld b,c
	ld a,8
	sub b
	jr z,QApps_Detect_1
$$:
	xor a
	ld (de),a
	inc de
	djnz $B
QApps_Detect_1:
	ld bc,100
$$:
	ld a,80h
	cpir
	ld a,81h
	cp (hl)
	jr nz,$B
	inc hl
	ld b,(hl)
	dec b
	ex af,af'
	sub b
	ex af,af'
	jr QApps_Detect_Loop
QApps_Detect_Over:
	pop af
	out (6),a
	ret
QApps_Detect_End:
QApps_Exit:
	ei
	ld a,kAppsMenu
	ld e,a
	or a
	ret
QApps_SpecialKeys:
	DB 28
	DB kClear
	DB kPrgm,kZoom,kDraw,kSPlot,kStat,kMath
	DB kTest,kChar,kVars,kMem,kMatrix,kDist,kAngle,kList,kCalc
	DB kCatalog,kgraph,kMode,kWindow,kYequ,kTable,kTblSet
	DB	kStatP,kFormat,kTrace,kQuit
QApps_InitializeMenu:
	xor a
	ld (menutop),a
	ld (curitem),a
	inc a
	ld (itemrow),a
	ret
