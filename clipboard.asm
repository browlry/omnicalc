; Clipboard functionality removed from v1.10
	di
	ld a,(cxCurApp)
	cp 40h
	jp nz,csc_Hook_Alpha_Exit
	ld a,(menucurrent)
	or a
	jp nz,csc_Hook_Alpha_Exit
	;User pressed alpha
	;Now we see if right/left pressed, then go to right/left loop after initalization
csc_Alpha_Scroll1:
	ld a,0FFh
	out (1),a
	nop
	nop
	ld a,0DFh
	out (1),a
	nop
	nop
	in a,(1)
	cp 127	;Alpha
	jp nz,csc_Hook_Alpha_Exit
	;Check for left/right
	ld a,0FFh
	out (1),a
	nop
	nop
	ld a,0FEh	;Group 1
	out (1),a
	nop
	nop
	in a,(1)
	cp	0FEh	;down arrow
	jp z,csc_Select_Paste
	cp 251	;Right arrow
	jr z,csc_Alpha_FirstRight
	jr csc_Alpha_Scroll1
csc_Alpha_FirstRight:
	ld hl,(editTail)
	ld de,(editBtm)
	or a
	sbc hl,de
	jp z,csc_Hook_Alpha_Exit
	ld hl,(currow)
	ld (firstcur),hl
	ld hl,(editCursor)
	ld (selstart),hl
	xor a
	ld (sellength),a
	ld (firstdelay),a
csc_Select_Loop_Restart:
	xor a
	ld (didelay),a
csc_Select_Loop:
	ld b,255
csc_Select_Loop_Delay:
	ex (sp),hl
	ex (sp),hl
	ex (sp),hl
	ex (sp),hl
	djnz csc_Select_Loop_Delay
	ld a,0FFh
	out (1),a
	nop
	nop
	ld a,0DFh
	out (1),a
	nop
	nop
	in a,(1)
	cp 127
	jp nz,csc_Select_AlphaReleased
	ld a,0FFh
	out (1),a
	nop
	nop
	ld a,0FEh
	out (1),a
	nop
	nop
	in a,(1)
	cp 253	;left arrow
	jr z,csc_Select_Left
	cp 251	;right arrow
	jp z,csc_Select_Right		
	cp 0F7h	;up arrow
	jp z,csc_Select_Copy
	xor a
	ld (firstdelay),a
	jr csc_Select_Loop
csc_Select_Left:
	ld a,(firstdelay)
	inc a
	ld (firstdelay),a
	cp 01h
	jr z,csc_Select_Left_GoThrough
	cp 75h
	jp c,csc_Select_Loop_Restart
	ld a,0A0h
	ld (firstdelay),a
	ld a,(didelay)
	inc a
	ld (didelay),a
	cp 20h
	jp c,csc_Select_Loop
csc_Select_Left_GoThrough:
	xor a
	ld (didelay),a
	ld hl,(editCursor)
	ld de,(editTop)
	or a
	sbc hl,de
	jr z,csc_Select_Loop
	ld a,(sellength)
	or a
	jp z,csc_Select_Done
	ld a,(currow)
	or a
	jr nz,csc_Select_LeftStillGood
	ld a,(curcol)
	ld c,a
	push bc
	ld hl,(editCursor)
	dec hl
	dec hl
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr z,csc_Select_LeftTwoCursor
	inc hl
	ld e,(hl)
	ld d,0
	B_CALL GetTokLen
	pop bc
	cp c
	jr z,csc_Select_LeftStillGood
	jp nc,csc_Select_Loop
	jr csc_Select_LeftStillGood
csc_Select_LeftTwoCursor:
	ld d,(hl)
	inc hl
	ld e,(hl)
	B_CALL GetTokLen
	pop bc
	cp c
	jr z,csc_Select_LeftStillGood
	jp nc,csc_Select_Loop
csc_Select_LeftStillGood:
	ld hl,(editCursor)
	dec hl
	dec hl
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr z,csc_Select_Left_Two
	ld hl,sellength
	dec (hl)
	ld hl,(editCursor)
	dec hl
	push hl
	ld (editCursor),hl
	ld hl,(editTail)
	dec hl
	ld (editTail),hl
	B_CALL CursorOff
	pop hl
	ld e,(hl)
	ld d,0
	push hl
	B_CALL GetTokLen
	ld b,a
	ld a,(curcol)
	sub b
	jr c,csc_Select_Left_OneRowUp
	ld (curcol),a
	jr csc_Select_Left_OneOK
csc_Select_Left_OneRowUp:
	add a,16
	ld (curcol),a
	ld hl,currow
	dec (hl)
csc_Select_Left_OneOK:
	pop hl
	ld e,(hl)
	ld d,0
	ld bc,(currow)
	push bc
	B_CALL PutTokString
	pop bc
	ld (currow),bc
	B_CALL CursorOn
	set textInverse,(iy+textFlags)
	ld a,LblockArrow
	B_CALL PutMap
	res textInverse,(iy+textFlags)
	jp csc_Select_Loop
csc_Select_Left_Two:
	ld hl,sellength
	dec (hl)
	dec (hl)
	ld hl,(editCursor)
	dec hl
	dec hl
	push hl
	ld (editCursor),hl
	ld hl,(editTail)
	dec hl
	dec hl
	ld (editTail),hl
	B_CALL CursorOff
	pop hl
	ld d,(hl)
	inc hl
	ld e,(hl)
	push hl
	B_CALL GetTokLen
	ld b,a
	ld a,(curcol)
	sub b
	jr c,csc_Select_Left2_OneRowUp
	ld (curcol),a
	jr csc_Select_Left2_OneOK
csc_Select_Left2_OneRowUp:
	add a,16
	ld (curcol),a
	ld hl,currow
	dec (hl)
csc_Select_Left2_OneOK:
	pop hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	ld bc,(currow)
	push bc
	B_CALL PutTokString
	pop bc
	ld (currow),bc
	B_CALL CursorOn
	set textInverse,(iy+textFlags)
	ld a,LblockArrow
	B_CALL PutMap
	res textInverse,(iy+textFlags)
	jp csc_Select_Loop
csc_Select_Right:
	ld a,(firstdelay)
	inc a
	ld (firstdelay),a
	cp 01h
	jr z,csc_Select_Right_GoThrough
	cp 75h
	jp c,csc_Select_Loop_Restart
	ld a,0A0h
	ld (firstdelay),a
	ld a,(didelay)
	inc a
	ld (didelay),a
	cp 20h
	jp c,csc_Select_Loop
csc_Select_Right_GoThrough:
	xor a
	ld (didelay),a
	ld hl,(editTail)
	ld de,(editBtm)
	or a
	sbc hl,de
	jp z,csc_Select_Loop
	ld a,(currow)
	cp 7
	jr c,csc_Select_Right_StillGood
	ld a,(curcol)
	sub 15
	neg
	ld c,a
	push bc
	ld hl,(editCursor)
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr z,csc_Select_Right_TwoCursor
	ld hl,(editCursor)
	ld e,(hl)
	ld d,0
	B_CALL GetTokLen
	pop bc
	cp c
	jp nc,csc_Select_Right_ScrollScreen
	jr csc_Select_Right_StillGood
csc_Select_Right_TwoCursor:
	ld hl,(editCursor)
	ld d,(hl)
	inc hl
	ld e,(hl)
	B_CALL GetTokLen
	pop bc
	cp c
	jp nc,csc_Select_Right_ScrollScreen
csc_Select_Right_StillGood:
	ld hl,(editCursor)
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr z,csc_Select_Right_Two
	ld hl,sellength
	inc (hl)
	ld hl,(editCursor)
	push hl
	inc hl
	ld (editCursor),hl
	ld hl,(editTail)
	inc hl
	ld (editTail),hl
	B_CALL CursorOff
	pop hl
	ld e,(hl)
	ld d,0
	;Display token inverted
	set textInverse,(iy+textFlags)
	B_CALL PutTokString
	res textInverse,(iy+textFlags)
	B_CALL CursorOn
	set textInverse,(iy+textFlags)
	ld a,LblockArrow
	B_CALL PutMap
	res textInverse,(iy+textFlags)
	jp csc_Select_Loop
csc_Select_Right_Two:
	ld hl,sellength
	inc (hl)
	inc (hl)
	ld hl,(editCursor)
	push hl
	inc hl
	inc hl
	ld (editCursor),hl
	ld hl,(editTail)
	inc hl
	inc hl
	ld (editTail),hl
	B_CALL CursorOff
	pop hl
	ld d,(hl)
	inc hl
	ld e,(hl)
	set textInverse,(iy+textFlags)
	B_CALL PutTokString
	res textInverse,(iy+textFlags)
	B_CALL CursorOn
	set textInverse,(iy+textFlags)
	ld a,LblockArrow
	B_CALL PutMap
	res textInverse,(iy+textFlags)
	jp csc_Select_Loop
csc_Select_Right_ScrollScreen:
	jp csc_Select_Loop
	;
	;
	ld a,1
	B_CALL ScrollUp
	ld hl,currow
	dec (hl)
	jp csc_Select_Right_StillGood
	;Scroll LCD
	ld hl,86ECh
	B_CALL SaveDisp
	ld hl,86ECh+96
	ld b,64-8
	B_CALL RestoreDisp
	;Scroll textshadow
	ld hl,textshadow+16
	ld de,textshadow
	ld bc,128-16
	ldir
	;Clear bottom row
	ld hl,(currow)
	push hl
	ld b,16
	ld de,0*256+7
	ld (currow),de
csc_Select_Right_ScrollScreen_Loop:
	push bc
	ld a,' '
	B_CALL PutMap
	ld hl,curcol
	inc (hl)
	pop bc
	djnz csc_Select_Right_ScrollScreen_Loop
	pop hl
	dec l
	ld (currow),hl
	B_CALL DispEOW
	jp csc_Select_Right_StillGood
csc_Select_AlphaReleased:
	call ShowTextShadow
csc_Select_Done:
	xor a
	ret
csc_Select_Copy:
	ld a,(sellength)
	or a
	jr z,csc_Select_Done
	ld hl,(editCursor)
	ld de,(editTop)
	or a
	sbc hl,de
	push hl
	ld de,(iMathPtr1)
	push de
	B_CALL CloseEditEqu
	ld hl,csc_AppVar
	rst 20h
	B_CALL ChkFindSym
	jr c,csc_Select_CopyGo
	B_CALL DelVarArc
csc_Select_CopyGo:
	B_CALL MemChk
	ld de,(sellength)
	or a
	sbc hl,de
	jr c,csc_Select_GoRight_AllDone	;Not enough memory
	ld hl,csc_AppVar
	rst 20h
	ld a,(sellength)
	ld l,a
	ld h,0
	B_CALL CreateAppVar
	inc de
	inc de
	ld hl,(selstart)
	ld bc,(sellength)
	ldir
	pop de
	B_CALL EditProg
	set editOpen,(iy+editFlags)
	ld hl,(iMathPtr1)
	inc hl
	inc hl
	ld (editTop),hl
	ld hl,(iMathPtr2)
	ld (editCursor),hl
	ex de,hl
	ld hl,(iMathPtr3)
	B_CALL CpHLDE
	jr nc,SetupEditProgSwapSkip
	ex de,hl
SetupEditProgSwapSkip:
	ld (editBtm),hl
	ld (editTail),hl
	ld hl,(editCursor)
	ld de,(editTop)
	or a
	sbc hl,de
	ex de,hl
	pop hl
	or a
	sbc hl,de
	push af
	add hl,de
	pop af
	jr z,csc_Select_GoRight_AllDone
csc_Select_GoRight:
	push hl
	B_CALL BufLeft
	ld hl,(editCursor)
	ld de,(editTop)
	or a
	sbc hl,de
	ex de,hl	;DE is new offset, HL will be old
	pop hl
	or a
	sbc hl,de
	push af
	add hl,de
	pop af
	jr nz,csc_Select_GoRight
csc_Select_GoRight_AllDone:
	call ShowTextShadow
	xor a
	ei
	ret
csc_Hook_Alpha_Exit:
	ld a,skAlpha
	or a
	ei
	ret
csc_Select_Paste:
	ld hl,csc_AppVar
	rst 20h
	B_CALL ChkFindSym
	jp c,csc_Select_GoRight_AllDone
	ld a,b
	or a
	jr nz,csc_Select_GoRight_AllDone
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	;HL - data start, BC - length
	ld a,c
	ld (templength),a
	push bc
csc_Select_Paste_Loop:
	push bc
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr z,csc_Select_Paste_TwoByte
	ld e,(hl)
	ld d,0
	push hl
	B_CALL BufInsert
	pop hl
	inc hl
	pop bc
	dec bc
	ld a,b
	or c
	jr nz,csc_Select_Paste_Loop
csc_Select_Paste_FinishLine:
	pop bc
	push bc
	B_CALL CursorOff
	pop bc
	ld hl,(editCursor)
	or a
	sbc hl,bc
	ex de,hl
	ld a,(templength)
	ld b,a
csc_Select_Paste_MoveLeft:
	push bc
	B_CALL BufLeft
	pop bc
	djnz csc_Select_Paste_MoveLeft
	;
	B_CALL DispEOW
	ld a,(templength)
	ld b,a
csc_Select_Paste_CursorToEnd:
	push bc
	B_CALL CursorRight
	pop bc
	djnz csc_Select_Paste_CursorToEnd
	B_CALL CursorOn
	xor a
	ret
csc_Select_Paste_TwoByte:
	ld d,(hl)
	inc hl
	ld e,(hl)
	push hl
	B_CALL BufInsert
	ld hl,templength
	dec (hl)
	pop hl
	inc hl
	pop bc
	dec bc
	dec bc
	ld a,b
	or c
	jr nz,csc_Select_Paste_Loop
	jr csc_Select_Paste_FinishLine
csc_AppVar:
	DB AppVarObj," cliptxt"