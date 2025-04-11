;Omnicalc v1.26.1
; (C) 2002-2005 Michael Vincent.
; Modified 2025 by Benjamin Allred to remap Up Arrow to [Entry] on home screen
;
;	This program is free software; you can redistribute it and/or modify
;	it under the terms of the Omnicalc license included with this source code.
;
;	This program is distributed in the hope that it will be useful,
;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;
Adjust:
	B_CALL ForceFullScreen
	res plotloc,(iy+plotflags)
	set bufferonly,(iy+plotflag3)
	call GetRealSettings_Unarchive
	ld (settingbyte),hl
	xor a
	ld (settingplace),a
	ld (settingdisp),a
	B_CALL ClrLCDFull
	ld bc,76*256+7
	ld de,95*256+7
	push bc
	B_CALL DarkLine
	pop bc
	ld d,b
	ld e,0
	B_CALL DarkLine
Adjust_Loop_Refresh:
	call Adjust_DisplayScreen
Adjust_Loop:
	ei
	halt
	B_CALL GetCSC
	cp sk2nd
	jr z,Adjust_Selected
	cp skEnter
	jr z,Adjust_Selected
	cp skDown
	jr z,Adjust_MoveDown
	cp skGraph
	jr z,Adjust_Page
	cp skClear
	jp z,MainMenu
	cp skUp
	jr nz,Adjust_Loop
	ld a,(settingplace)
	sub 1
	jr c,Adjust_Loop
Adjust_Move_Jump:
	push af
	call Adjust_HidePointer
	pop af
	ld (settingplace),a
	call Adjust_ShowPointer
	jr Adjust_Loop
Adjust_Page:
	ld a,(settingdisp)
	or a
	jr z,Adjust_Page_1
	ld hl,(settingbyte)
	dec hl
	ld (settingbyte),hl
	xor a
	ld (settingdisp),a
	ld (settingplace),a
	jr Adjust_Loop_Refresh
Adjust_Page_1:
	ld hl,(settingbyte)
	inc hl
	ld (settingbyte),hl
	ld (settingplace),a
	inc a
	ld (settingdisp),a
	jr Adjust_Loop_Refresh
Adjust_MoveDown:
	ld a,(settingnum)
	ld b,a
	ld a,(settingplace)
	inc a
	cp b
	jr z,Adjust_Loop
	jr Adjust_Move_Jump
Adjust_Selected:
	ld a,(settingplace)
	ld b,a
	inc b
	ld a,00000001b
Adjust_Selected_Loop:
	rlca
	djnz Adjust_Selected_Loop
	ld hl,(settingbyte)
	xor (hl)
	ld (hl),a
	ld a,(settingplace)
	add a,a
	add a,a
	inc a
	add a,a
	ld h,a
	ld l,2
	add a,3
	ld d,a
	ld e,5
	srl c
	B_CALL InvertRect
	jp Adjust_Loop
Adjust_DisplayScreen:
	ld hl,plotsscreen
	ld de,plotsscreen+1
	ld bc,768-(8*12)
	ld (hl),0
	ldir
	set textwrite,(iy+sgrflags)
	set bufferonly,(iy+plotflags)
	ld hl,Settings_Page
	ld de,57*256+79
	ld (pencol),de
	call vputstr
	ld a,(settingdisp)
	or a
	ld hl,Adjust_Screen_1
	jr z,Adjust_DisplayScreen_Go
	ld hl,Adjust_Screen_2
Adjust_DisplayScreen_Go:
	ld b,(hl)
	ld a,b
	ld (settingnum),a
	push bc
	inc hl
	xor a
	ld (penrow),a
Adjust_DisplayScreen_Ready_Loop:
	ld a,16
	ld (pencol),a
	call vputstr
	ld a,(penrow)
	add a,8
	ld (penrow),a
	djnz Adjust_DisplayScreen_Ready_Loop
	ld hl,(settingbyte)
	pop bc
	ld c,(hl)
	srl c
	ld a,1
	ld (settingstemp),a
Adjust_DisplayScreen_Boxes_Loop:
	ld a,(settingstemp)
	ld h,a
	ld l,1
	add a,5
	ld d,a
	ld e,6
	srl c
	push bc
	jr nc,Adjust_DisplayScreen_Boxes_Empty
	B_CALL FillRect
	jr Adjust_DisplayScreen_Boxes_SkipOne
Adjust_DisplayScreen_Boxes_Empty:
	B_CALL DrawRectBorder
Adjust_DisplayScreen_Boxes_SkipOne:
	pop bc
	ld a,(settingstemp)
	add a,8
	ld (settingstemp),a
	djnz Adjust_DisplayScreen_Boxes_Loop
	;Now for the pointer
	ld a,(settingplace)
	add a,a
	add a,a
	add a,a
	ld (penrow),a
	ld a,11
	ld (pencol),a
	ld a,05h
	B_CALL VPutMap
	set fullscrndraw,(iy+apiflg4)
	res textwrite,(iy+sgrflags)
	res bufferonly,(iy+plotflags)
	B_CALL GrBufCpy
	ret
Adjust_ShowPointer:
	ld b,5
	jr $F
Adjust_HidePointer:
	ld b,6
$$:
	set textwrite,(iy+sgrflags)
	set bufferonly,(iy+plotflags)
	ld a,(settingplace)
	add a,a
	add a,a
	add a,a
	ld (penrow),a
	ld a,11
	ld (pencol),a
	ld a,b
	B_CALL VPutMap
	set fullscrndraw,(iy+apiflg4)
	res textwrite,(iy+sgrflags)
	res bufferonly,(iy+plotflags)
	B_CALL GrBufCpy
	ret
Adjust_Screen_1:
	DB 7
	DB "Parentheses  Assistant",0
	DB "Memory  Protection",0
	DB "Entries  Menu",0
	DB "RAM  Recovery  (SE  only)",0
	DB "Virtual  Calc  (SE  only)",0
	DB "Base  Conversion",0
	DB "Lowercase  Letters",0
Adjust_Screen_2:
	DB 4
	DB "Thousands  Separators",0
	DB "Quick  APPS  (SE  only)",0
	DB 6,6,"Hide  Finance  app",0
	DB "UpKeyPastesPrevEntry",0
FontSets:
	xor a
	ld (numsets),a
	ld hl,(progPtr)
FontSets_FirstLookLoop:
	ld bc,(pTemp)
	or a
	sbc hl,bc
	jr c,FontSets_FirstLook_Done
	jr z,FontSets_FirstLook_Done
	add hl,bc
	ld a,(hl)
	cp ProtProgObj
	jr nz,FontSets_FirstLook_Next
	;HL is at type byte
	push hl
	dec hl
	dec hl
	dec hl
	dec hl
	dec hl
	ld a,(hl)
	or a
	jr nz,FontSets_FirstLook_Nope
	inc hl
	inc hl
	ld a,(hl)
	dec hl
	ld h,(hl)
	ld l,a
	;HL is data (size bytes) now
	ld de,FontSet_Check
	ld b,13
	call CompareStrings
	jr nz,FontSets_FirstLook_Nope
	ld a,(numsets)
	inc a
	ld (numsets),a
	dec a
	add a,a
	ld d,0
	ld e,a
	ld hl,numsets+1
	add hl,de
	;store pointer to type byte
	pop de
	push de
	ld (hl),e
	inc hl
	ld (hl),d
FontSets_FirstLook_Nope:
	pop hl
FontSets_FirstLook_Next:
	ld de,-6
	add hl,de
	ld d,0
	ld e,(hl)
	inc e
	or a
	sbc hl,de
	jr FontSets_FirstLookLoop
FontSets_FirstLook_Done:
	;See if we found any
	ld a,(numsets)
	or a
	jp z,FontSets_NoSets
	;Main routine here, we found at least one font set (woohoo!)
	dec a
	add a,a
	ld d,0
	ld e,a
	ld hl,numsets+1
	add hl,de
	ld (setsend),hl
	ld hl,numsets+1
	ld (setpointer),hl
	ld a,'A'
	ld (curchar),a
	B_CALL ClrLCDFull
	B_CALL GrBufClr
	ld hl,0
	ld de,7*256+95
	B_CALL FillRect
	set textInverse,(iy+textFlags)
	ld hl,30
	ld (pencol),hl
	ld hl,FontSets_Title
	call vputstr
	res textInverse,(iy+textFlags)
	call FontSets_Display
	call FontSets_Status
FontSets_MainLoop:
	ei
	halt
	B_CALL GetCSC
	cp skLeft
	jp z,FontSets_MoveLeft
	cp skRight
	jp z,FontSets_MoveRight
	cp skUp
	jr z,FontSets_MoveUp
	cp skDown
	jr z,FontSets_MoveDown
	cp skEnter
	jr z,FontSets_Toggle
	cp sk2nd
	jr z,FontSets_Toggle
	cp skClear
	jr z,FontSets_Save
	jr FontSets_MainLoop
FontSets_Save:
	ld hl,OP3+1
	ld de,OP3+2
	ld (hl),0
	ld bc,7
	ldir
	ld hl,(setpointer)
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	;HL now points to type byte
	ld de,-6
	add hl,de
	ld b,(hl)
	dec hl
	ld de,OP3+1
FontSets_Save_Loop:
	ld a,(hl)
	ld (de),a
	inc de
	dec hl
	djnz FontSets_Save_Loop
	ld a,6
	ld (OP3),a
	call GetRealSettings_Unarchive
	ld de,4
	add hl,de
	ex de,hl
	ld hl,OP3
	ld bc,9
	ldir
	;fontHookPtr+3 - lower byte
	;cxMainHookPtr+3 - upper byte
	xor a
	ld (fontHookPtr+3),a
	ld (cxMainHookPtr+3),a
	jp MainMenu
FontSets_MoveUp:
	ld hl,curchar
	dec (hl)
	call FontSets_PreviewChar
	jr FontSets_MainLoop
FontSets_MoveDown:
	ld hl,curchar
	inc (hl)
	call FontSets_PreviewChar
	jr FontSets_MainLoop
FontSets_Toggle:
	call GetRealSettings_Unarchive
	inc hl
	inc hl
	inc hl
	bit 0,(hl)
	jr nz,FontSets_Toggle_Off
	set 0,(hl)
	call FontSets_Status
	jp FontSets_MainLoop
FontSets_Toggle_Off:
	res 0,(hl)
	res FontHook,(iy+Hooks2)
	call FontSets_Status
	jp FontSets_MainLoop
FontSets_MoveRight:
	ld hl,(setpointer)
	ld de,(setsend)
	or a
	sbc hl,de
	jp z,FontSets_MainLoop
	ld hl,(setpointer)
	inc hl
	inc hl
	ld (setpointer),hl
	call FontSets_Display
	jp FontSets_MainLoop
FontSets_MoveLeft:
	ld hl,(setpointer)
	ld de,numsets+1
	or a
	sbc hl,de
	jp z,FontSets_MainLoop
	ld hl,(setpointer)
	dec hl
	dec hl
	ld (setpointer),hl
	call FontSets_Display
	jp FontSets_MainLoop
FontSets_Display:
	ld hl,0*256+2
	ld (currow),hl
	ld hl,(setpointer)
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld de,-6
	add hl,de
	ld b,(hl)
	dec hl
FontSets_Display_Loop:
	push bc
	ld a,(hl)
	B_CALL PutC
	dec hl
	pop bc
	djnz FontSets_Display_Loop
	B_CALL EraseEOL
	;Now display sample character
FontSets_PreviewChar:
	ld hl,OP4
	ld (hl),7
	inc hl
	ld (hl),8
	inc hl
	ex de,hl
	push de
	ld hl,(setpointer)
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	;HL points to type byte
	dec hl
	dec hl
	dec hl
	ld a,(hl)
	dec hl
	ld h,(hl)
	ld l,a
	;HL points to size bytes
	ld de,13
	add hl,de
	push hl
	ld h,0
	ld a,(curchar)
	ld l,a
	ld e,l
	ld d,h
	add hl,hl
	add hl,hl
	add hl,hl
	or a
	sbc hl,de
	ex de,hl
	pop hl
	add hl,de
	;HL is font data now
	pop de
	ld bc,7
	ldir
	ld hl,OP4
	ld de,57*256+7
	B_CALL DisplayImage
	ld hl,FontSets_Preview_Title
	ld de,49*256
	ld (pencol),de
	jp vputstr
FontSets_Preview_Title:
	DB "Preview:",0
FontSets_Status:
	call GetRealSettings
	inc hl
	inc hl
	inc hl
	bit 0,(hl)
	jr nz,FontSets_Status_On
	ld hl,29*256+2
	ld de,35*256+8
	B_CALL DrawRectBorderClear
	ld de,29*256+12
	ld (pencol),de
	ld hl,FontSets_Question
	jp vputstr
FontSets_Status_On:
	ld hl,29*256+2
	ld de,35*256+8
	B_CALL FillRect
	ld de,29*256+12
	ld (pencol),de
	ld hl,FontSets_Question
	jp vputstr
FontSets_Title:
	DB "FONT  SETS",0
FontSets_Question:
	DB "Font set active?",0
FontSets_NoSets:
	B_CALL ClrLCDFull
	B_CALL GrBufClr
	ld hl,14*256+17
	ld (pencol),hl
	call DisplayMessageBox
	ld hl,FontSets_None
	call vputstr
	ld de,30*256+17
	ld (pencol),de
	call vputstr
FontSets_NoSets_Pause:
	ei
	halt
	B_CALL GetCSC
	or a
	jr z,FontSets_NoSets_Pause
	jp MainMenu
FontSets_None:
	DB "No font sets found.",0
	DB "Press any key",0
FontSet_Check:
	DW 1803
	DB 0BBh,06Dh,0C9h,"omnicalc"
Settings_Page:
	DB "PAGE",0
