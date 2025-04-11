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
PreParser:
	xor a
	ret
Hook_Parser:
	add a,e
	push af
	in a,(2)
	rla
	sbc a,a
	out (20h),a		;set 15 MHz if running on SE
	pop af
	;now check for preparser or parser
	or a
	jr z,PreParser
	push hl
	ld hl,0C0C0h
	or a
	sbc hl,bc
	pop hl
	jp z,Parse_Log
	push hl
	ld hl,8A8Ah
	or a
	sbc hl,bc
	pop hl
	jp nz,Parse_NotReal
	ld a,l
	or a
	ret z
	dec a
	ret z
	;Now find which token and appropriate parser
	;HL is number of arguments
	push hl
	B_CALL PushRealO1		;Now we can jumble OP1 without losing an argument
	pop hl
	push hl
	ld d,h
	ld e,l
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,de
	;HL now points to fps-HL
	push hl
	ex de,hl
	ld hl,(fps)
	or a
	sbc hl,de
	push hl
	;HL now if FPS where ## is
	rst 20h	;Store ## in OP1
	pop de	;DE to copy to.
	pop bc	;BC fps-HL, copy length
	ldir		;HL was setup by rst 20h
	ld hl,(fps)
	ld de,9
	or a
	sbc hl,de
	ld (fps),hl
	;Check to make sure we have a valid ##
	B_CALL CkPosInt
	jr z,Parser_Number_Ok
	pop hl
	B_JUMP ErrArgument
Parser_Number_Ok:
	B_CALL ConvOP1
	ld a,e
	cp 20
	jp c,Parser_Symbolic_Token
	cp last_token+1
	jp nc,Parse_Error_Argument
	sub 20
	ld e,a
	ld d,0
	ld hl,Parser_JumpTable
	sla e
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	pop de
	ld a,e	;A = number of arguments
	dec a
	jp (hl)
	;FPS contains arguments from last to first
Parse_Sprite_ConvOP1:
	B_CALL ConvOP1
	ld a,d
	or a
	jp nz,Parse_Error_Range
	ld a,e
	ret
Parse_Sprite:
	;Arguments - (picture, source X pixel, source Y pixel, width, height, destination X, destination Y
	res 0,(iy+asm_Flag1)
	res 1,(iy+asm_Flag1)
	cp 7
	jr z,Parse_Sprite_NormalStart
	cp 8
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	ld (iy+asm_Flag1),e
Parse_Sprite_NormalStart:
	;Get destination Y
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	cp 64
	jp nc,Parse_Error_Range
	ld (spritedy),a
	;Get destination X
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	cp 96
	jp nc,Parse_Error_Range
	ld (spritedx),a
	;Get sprite height (pixels)
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	or a
	jp z,Parse_Error_Range
	cp 65
	jp nc,Parse_Error_Range
	ld (spriteheight),a
	;Get sprite width (pixels)
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	or a
	jp z,Parse_Error_Range
	cp 97
	jp nc,Parse_Error_Range
	ld (spritewidth),a
	and 7
	jp nz,Parse_Error_Range
	;We now know that the width MOD 8 = 0
	;Get sprite Y position (0-62)
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	cp 63
	jp nc,Parse_Error_Range
	ld (spritey),a
	ld b,a
	ld a,(spriteheight)
	add a,b
	cp 65
	jp nc,Parse_Error_Range
	;Get sprite X position (0-88 inclusive)
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	cp 96
	jp nc,Parse_Error_Range
	ld (spritex),a
	ld b,a
	ld a,(spritewidth)
	add a,b
	cp 97
	jp nc,Parse_Error_Range
	ld a,(spritex)
	and 7
	jp nz,Parse_Error_Range
	;Get picture
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	call Parse_Sprite_ConvOP1
	cp 10
	jp nc,Parse_Error_Argument
	sub 1
	jr nc,Parse_Sprite_PicVar
	ld a,9
Parse_Sprite_PicVar:
	ld hl,OP1
	ld (hl),PictObj
	inc hl
	ld (hl),tVarPict
	inc hl
	ld (hl),a
	inc hl
	ld (hl),0
	rst rFINDSYM
	jp c,Parse_Error_Argument
	;All is now go.
	ld a,(spritewidth)
	sra a
	sra a
	sra a
	;sprite width is in bytes
	ld b,a
	ld a,12
	sub b
	ld (dataskip),a	;how many bytes to skip across each row
	ex de,hl
	inc hl
	inc hl			;HL source data start
	ld a,(spritey)
	or a
	jr z,Parse_Sprite_SkipYAdd
	ld b,a
	ld de,12
Parse_Sprite_YAdd:
	add hl,de
	djnz Parse_Sprite_YAdd
Parse_Sprite_SkipYAdd:
	ld a,(spritewidth)
	sra a
	sra a
	sra a
	ld (spriteacross),a
	ld a,(spritex)
	sra a
	sra a
	sra a
	ld d,0
	ld e,a
	add hl,de
	;HL now points to first sprite data
	;DE to storage place
	ld de,Sprite_Data
	ld a,(spriteheight)
	ld b,a
Parse_Sprite_Loop:
	push bc
	ld a,(spriteacross)
	ld b,a
Parse_Sprite_Loop2:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz Parse_Sprite_Loop2
	ld a,(dataskip)
	ld b,0
	ld c,a
	add hl,bc
	pop bc
	djnz Parse_Sprite_Loop
	;(Sprite_Data) is start of sprite...
	ld ix,Sprite_Data
	ld a,(spritewidth)
	sra a
	sra a
	sra a
	ld c,a
	ld a,(spriteheight)
	ld b,a
	ld a,(spritedy)
	ld l,a
	ld a,(spritedx)
	bit 1,(iy+asm_Flag1)	;reset (Z) if XOR, set (NZ) if OR
	jr nz,Parse_Sprite_ORLogic
	call largeSprite
Parse_Sprite_AfterRoutine:
	bit 0,(iy+asm_Flag1)
	jr nz,Parse_Sprite_NoCopyScreen
	bit plotDisp,(iy+plotFlags)
	jr nz,Parse_Sprite_Shown
	B_CALL PDspGrph
	jr Parse_Sprite_NoCopyScreen
Parse_Sprite_Shown:
	B_CALL GrBufCpy
Parse_Sprite_NoCopyScreen:
	;Switch context
	;ld a,44h
	;MICHAEL_CALL 4030h
	;set plotDisp,(iy+plotFlags)
	;res graphDraw,(iy+graphFlags)
	B_CALL OP1Set0
	jp Parse_Token_ExitGood
Parse_Sprite_ORLogic:
	call largeSpriteOR
	jr Parse_Sprite_AfterRoutine
Parse_RomanDecimal:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	ld a,(OP1)
	cp StrngObj
	jp nz,Parse_Error_DataType
	B_CALL ChkFindSym
	jp c,Parse_Error_Argument
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld a,b
	or c
	jp z,Parse_Error_Syntax
	ld de,-1
	ld ix,0
Parse_Roman_Loop:
	push bc
	push hl
	push de
	ld a,(hl)
	ld hl,Parse_Roman_Table
	push hl
	ld bc,7
	cpir
	jp nz,Parse_Error_Domain
	pop de
	scf
	sbc hl,de
	ld de,Parse_Roman_Values
	sla l
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	pop de
	push hl
	sbc hl,de
	push af
	add hl,de
	ex de,hl
	pop af
	jr c,Parse_Roman_Add
	jr z,Parse_Roman_Add
	ex de,hl
	or a
	sbc hl,de
	sbc hl,de
	ex de,hl
Parse_Roman_Add:
	add ix,de
	pop de
	pop hl
	pop bc
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,Parse_Roman_Loop
	push ix
	pop hl
	B_CALL SetXXXXOP2
	B_CALL OP2ToOP1
	jp Parse_Token_ExitGood
Parse_Roman_Table:
	DB "IVXLCDM"
Parse_Roman_Values:
	DW 1,5,10,50,100,500,1000
Parse_RadicalSimplify:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	B_CALL CkOP1Real
	jp nz,Parse_Error_DataType
	B_CALL CkPosInt
	jp nz,Parse_Error_Domain
	B_CALL PushRealO1
	call ConvOP1
	push af
	push de
	B_CALL PopRealO1
	pop de
	pop af
	jp c,Parse_Error_Domain
	ld a,e
	or d
	jp z,Parse_Radical_Done
	;Ready to go!
	ld (currentNum),de
	ex de,hl
	call Sqrt16
	ld a,d
	;HL is there
	ld (currentI),a
	ld a,1
	ld (currentCo),a
Parse_Radical_Loop:
	in a,(4)
	bit 3,a
	jp z,Parse_Error_Break
	;check for end
	ld a,(currentI)
	cp 1
	jr z,Parse_Radical_Over
	ld h,a
	ld e,a
	call Mult88
	ld de,(currentNum)
	ex de,hl
	call Div1616
	ld a,h
	or l
	jr nz,Parse_Radical_NoInt
	ld (currentNum),de
	ld a,(currentCo)
	ld h,a
	ld a,(currentI)
	ld e,a
	call Mult88
	ld a,l
	ld (currentCo),a
	;optional below
	ld hl,(currentNum)
	call Sqrt16
	ld a,(currentI)
	cp d
	jr nc,Parse_Radical_NoInt
	ld a,d
	ld (currentI),a
	jr Parse_Radical_Loop
Parse_Radical_NoInt:
	ld hl,currentI
	dec (hl)
	jr Parse_Radical_Loop
Parse_Radical_Over_Skip21:
	pop hl
	ld de,-2
	add hl,de
	push hl
	jr Parse_Radical_Over_Skip2
Parse_Radical_Over:
	;currentNum now is radicand
	;currentCo is the number in front of the radical sign
	ld hl,2
	push hl
	ld a,(currentCo)
	cp 1
	jr z,Parse_Radical_Over_Skip1
	call FormA
	pop hl
	add hl,bc
	push hl
Parse_Radical_Over_Skip1:
	ld hl,(currentNum)
	ld de,1
	or a
	sbc hl,de
	jr z,Parse_Radical_Over_Skip21
	inc hl
	B_CALL SetXXXXOP2
	B_CALL OP2ToOP1
	ld a,10
	B_CALL FormEReal
	pop hl
	add hl,bc	;HL now contains total length
	push hl
Parse_Radical_Over_Skip2:
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
	ld hl,OP1
	ld de,tempop1
	ld bc,9
	ldir
	pop hl
	;HL is length
	B_CALL CreateStrng
	ex de,hl
	inc hl
	inc hl
	;Ready to copy string
	push hl
	ld a,(currentCo)
	cp 1
	jr z,Parse_Radical_Over_Skip3
	call FormA
	pop de
	ld hl,OP3
	ldir
Parse_Radical_Over_Skip4:
	push de
	ld hl,(currentNum)
	ld de,1
	or a
	sbc hl,de
	jp z,Parse_Radical_Over_Skip5
	pop de
	ld a,tSqrt
	ld (de),a
	inc de
	push de
	ld hl,(currentNum)
	B_CALL SetXXXXOP2
	B_CALL OP2ToOP1
	ld a,10
	B_CALL FormEReal
	pop de
	ld hl,OP3
	ldir
	ld a,11h
	ld (de),a
	ld hl,tempop1
	ld de,OP1
	ld bc,9
	ldir
Parse_Radical_Done:
	xor a
	inc a
	ret
Parse_Radical_Over_Skip3:
	pop de
	jr Parse_Radical_Over_Skip4
Parse_Radical_Over_Skip5:
	pop de
	ld hl,tempop1
	ld de,OP1
	ld bc,9
	ldir
	jr Parse_Radical_Done
FormA:
	ld hl,OP3
	ld d,0
	ld c,-1
DispA_HundredsDigit:
	inc c
	sub 100
	jr nc,DispA_HundredsDigit
	add a,100
	ld b,c
	ld c,-1
DispA_TensDigit:
	inc c
	sub 10
	jr nc,DispA_TensDigit
	add a,10
	ld e,c
	;BEA
	push af
	ld a,b
	or a
	jr z,$F
	inc d
	add a,'0'
	ld (hl),a
	inc hl
$$:
	ld a,e
	or a
	jr z,$F
	inc d
	add a,'0'
	ld (hl),a
	inc hl
$$:
	pop af
	add a,'0'
	ld (hl),a
	inc hl
	ld (hl),0
	inc d
	ld c,d
	ld b,0
	ret
Mult88:
	ld d,0
	ld l,0
	sla h
	jr	nc,Mult88_Skip
	ld	l,e
Mult88_Skip:
	ld b,7
Mult88_Loop:
	add hl,hl
	jr	nc,$+3
	add hl,de
	djnz Mult88_Loop
	ret
Sqrt16:
	;Takes the square root of a 16-bit number in HL
	ld de,0040h
	ld a,l
	ld l,h
	ld h,d
	or a
	ld b,8
Sqrt16_Loop:
	sbc hl,de
	jr nc,Sqrt16_Skip
	add hl,de
Sqrt16_Skip:
	ccf
	rl d
	add a,a
	adc hl,hl
	add a,a
	adc hl,hl
	djnz Sqrt16_Loop
	ret
ConvOP1:
	;Gets a number out of OP1
	;Returns C if the number was too large
	ld b,5
	ld de,0
ConvOP1_Loop:
	push bc
	xor a
	ld hl,OP1+4
	rld
	dec hl
	rld
	dec hl
	rld
	ex de,hl
	add hl,hl
	push hl
	add hl,hl
	add hl,hl
	jr c,ConvOP1_Return2
	pop bc
	add hl,bc
	jr c,ConvOP1_Return
	;HL is times 10
	ld b,0
	ld c,a
	add hl,bc
	ex de,hl
	dec hl
	pop bc
	dec (hl)
	ret p
	djnz ConvOP1_Loop
	scf
	ret
ConvOP1_Return2:
	pop bc
ConvOP1_Return:
	pop bc
	ret
Div1616:
	ld b,16
	ld a,h
	ld c,l
	ld hl,0
Div1616_Loop:
	sla c
	inc c
	rla
	adc hl,hl
	sbc hl,de
	jr nc,$+4
	add hl,de
	dec c
	djnz Div1616_Loop
	ld e,c
	ld d,a		;DE is the quotient, HL is the remainder
	ret
Parse_FactorNumber:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	B_CALL CkOP1Real
	jp nz,Parse_Error_DataType
	B_CALL CkPosInt
	jp nz,Parse_Error_Domain
	call ConvOP1
	jp c,Parse_Error_Domain
	ld (currentNum2),de
	ld hl,0
	ld (factorsCount),hl
	ld hl,2
	ld (primeptr),hl
Parse_Factor_Loop:
	in a,(4)
	bit 3,a
	jp z,Parse_Error_Break
	ld hl,(currentNum2)
	ld de,(primeptr)
	or a
	sbc hl,de
	jr z,Parse_Factor_Over
	jr c,Parse_Factor_Over
	add hl,de
	call Div1616
	ld a,h
	or l
	jr nz,Parse_Factor_NoInt
	ld (currentNum2),de
	ld hl,(primeptr)
	;DE is the new factor
	call Parse_Factor_AddFactor
	jr Parse_Factor_Loop
Parse_Factor_NoInt:
	ld hl,(primeptr)
	ld de,2
	or a
	sbc hl,de
	jr nz,Parse_Factor_Two
	dec hl
Parse_Factor_Two:
	add hl,de
	inc hl
	inc hl
	ld (primeptr),hl
	jr Parse_Factor_Loop
Parse_Factor_Over:
	ld hl,(currentNum2)
	call Parse_Factor_AddFactor
	B_CALL ZeroOP1
	ld a,ListObj
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
	ld hl,OP1
	ld de,86ECh
	ld bc,9
	ldir
	ld hl,(factorsCount)
	B_CALL CreateRList
	inc de
	inc de
	;DE is where to put data
	ld bc,(factorsCount)
	ld ix,factorsList
Parse_Factor_StoreLoop:
	push bc
	push de
	push ix
	ld l,(ix)
	ld h,(ix+1)
	B_CALL SetXXXXOP2
	pop ix
	pop de
	ld hl,OP2
	ld bc,9
	ldir
	inc ix
	inc ix
	pop bc
	dec bc
	ld a,b
	or c
	jr nz,Parse_Factor_StoreLoop
	ld hl,86ECh
	rst 20h
	jp Parse_Token_ExitGood
Parse_Factor_AddFactor:
	push hl
	ld hl,(factorsCount)
	add hl,hl
	ex de,hl
	ld hl,factorsList
	add hl,de
	pop de
	ld (hl),e
	inc hl
	ld (hl),d
	ld hl,(factorsCount)
	inc hl
	ld (factorsCount),hl
	ret
Parse_Gamma:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
;	B_CALL CkOP1Real
;	jp nz,Parse_Error_DataType
	B_CALL LogGamma
	B_CALL TenX
	jp Parse_Token_ExitGood
Parse_Base:
	res convertbase,(iy+baseflags)
	cp 2
	jp z,Parse_Base_Start
	cp 3
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	call Parse_Sprite_ConvOP1
	cp 37
	jp nc,Parse_Error_Range
	cp 2
	jp c,Parse_Error_Range
	;A is base
	set convertbase,(iy+baseflags)
	ld (newbase),a
Parse_Base_Start:
	B_CALL PopRealO1
	;Get base
	call Parse_Sprite_ConvOP1
	cp 37
	jp nc,Parse_Error_Range
	cp 2
	jp c,Parse_Error_Range
	;Bases 2-36 valid
	ld (numbase),a
	ld a,1
	ld (maxnum),a
	;Now build multiplication table
	ld ix,multtable+4
	ld (ix-4),1
	xor a
	ld (ix-3),a
	ld (ix-2),a
	ld (ix-1),a
	ld hl,1
	ld de,0
Parse_Base_BuildTable:
	ld a,(numbase)
	call MultiplyDEHLByA
	jr c,Parse_Base_BuildTable_Over
	ld (ix),l
	inc ix
	ld (ix),h
	inc ix
	ld (ix),e
	inc ix
	ld (ix),d
	inc ix
	ld a,(maxnum)
	inc a
	ld (maxnum),a
	jr Parse_Base_BuildTable
Parse_Base_BuildTable_Over:
	B_CALL PopRealO1
	;Get string
	ld a,(OP1)
	cp StrngObj
	jp nz,Parse_Error_DataType
	B_CALL ChkFindSym
	jp c,Parse_Error_Argument
	ld a,b
	or a
	jp nz,Parse_Error_Argument
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld a,b
	or c
	jp z,Parse_Error_Argument
	;HL data, BC size
	ld a,b
	or a
	jp nz,Parse_Error_Argument
	ld a,(maxnum)
	cp c
	jp c,Parse_Error_Range
	add hl,bc
	dec hl
	;HL is end of string, BC is length to work backwards
	ld de,0
	ld (partialans),de
	ld (partialans+2),de
	push hl
	pop ix
	ld de,multtable
	ld b,c
Parse_Base_Loop:
	push bc
	ld a,(ix)
	ld hl,Parse_Base_Table
	ld bc,36
	cpir
	jp nz,Parse_Error_Argument
	;Found a key
	ld bc,35
	add hl,bc
	ld b,(hl)
	;Now check B with our base to see if it is an invalid character
	ld a,(numbase)
	dec a
	cp b
	jp c,Parse_Error_DataType
	ld h,d
	ld l,e
	inc de
	inc de
	inc de
	inc de
	push de
	push bc
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld h,b
	ld l,c
	pop af
	;A * DEHL..
	call MultiplyDEHLByA
	jp c,Parse_Error_Range
	push de
	ld de,(partialans)
	add hl,de
	ld (partialans),hl
	pop de
	ld hl,(partialans+2)
	adc hl,de
	ld (partialans+2),hl
	pop de
	dec ix
	pop bc
	djnz Parse_Base_Loop
	ld hl,(partialans+2)
	B_CALL SetXXXXOP2
	ld hl,num64k
	rst 20h
	B_CALL FPMult
	ld hl,(partialans)
	B_CALL SetXXXXOP2
	B_CALL FPAdd
	;OP1 here is decimal conversion
	bit convertbase,(iy+baseflags)
	jp z,Parse_Token_ExitGood
	bit 1,(iy+newDispF)
	jr nz,Parse_Base_String
	ld a,(newbase)
	ld (getKeyHookPtr+3),a
	ld a,0C9h
	ld (0FE67h),a
	jp Parse_Token_ExitGood
Parse_Base_String:
	ld hl,(partialans)
	ld de,(partialans+2)
	call cxMain_BaseOutput_EntryPoint
	inc a
	ret
num64k:
	DB 00h,84h,65h,53h,60h,00h,00h,00h,00h
Parse_Base_Table:
	DB t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,tA,tB,tC,tD,tE,tF,tG,tH,tI,tJ,tK,tL,tM,tN
	DB tO,tP,tQ,tR,tS,tT,tU,tV,tW,tX,tY,tZ
	DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27
	DB 28,29,30,31,32,33,34,35
MultiplyDEHLByA:
	or a
	jr z,MultiplyDEHLByA_Zero
	ld b,h
	ld c,l
	ld (ramcode),de
	dec a
	ret z
MultiplyDEHLByA_Loop:
	add hl,bc
	ex de,hl
	push bc
	ld bc,(ramcode)
	adc hl,bc
	pop bc
	ex de,hl
	ret c
	dec a
	jr nz,MultiplyDEHLByA_Loop
	ret
MultiplyDEHLByA_Zero:
	ld hl,0
	ld d,h
	ld e,l
	ret
Parse_LinkGet:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	B_CALL ConvOP1
	or a
	jr z,Parse_LinkGet_NoWait
	in a,(2)
	rla
	jr nc,Parse_LinkGet_Regular
Parse_LinkGet_SE:
	xor a
	out (8),a
	in a,(4)
	bit 3,a
	jp z,Parse_Error_Break
	in a,(9)
	bit 4,a
	jr z,Parse_LinkGet_SE
	jr Parse_LinkGet_ByteExit
Parse_LinkGet_Regular:
	in a,(4)
	bit 3,a
	jp z,Parse_Error_Break
	in a,(0)
	and 3
	cp 3
	jr z,Parse_LinkGet_Regular
Parse_LinkGet_ByteExit:
	B_CALL RecAByteIO
	ld h,0
	ld l,a
	B_CALL SetXXXXOP2
	B_CALL OP2ToOP1
	jp Parse_Token_ExitGood
Parse_LinkGet_NoWait:
	in a,(2)
	rla
	jr c,Parse_LinkGet_NoWaitSE
	in a,(0)
	and 3
	cp 3
	jr z,Parse_LinkGet_ReturnBad
	jr Parse_LinkGet_ByteExit
Parse_LinkGet_NoWaitSE:
	xor a
	out (8),a
	in a,(9)
	bit 4,a
	jr z,Parse_LinkGet_ReturnBad
	jr Parse_LinkGet_ByteExit
Parse_LinkGet_ReturnBad:
	ld hl,Parse_Negative1
	rst 20h
	jp Parse_Token_ExitGood
Parse_LinkSend:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	B_CALL ConvOP1
	ld a,d
	or a
	jp nz,Parse_Error_Range
	ld a,e
	AppOnErr Parse_LinkSend_Err
	B_CALL SendAByte
	AppOffErr
	B_CALL OP1Set0
	jp Parse_Token_ExitGood
Parse_LinkSend_Err:
	ld hl,Parse_Negative1
	rst 20h
	jp Parse_Token_ExitGood
Parse_Modulo:
	cp 2
	jp nz,Parse_Error_Argument
	B_CALL PopRealO2
	ld a,(OP2)
	or a
	jp nz,Parse_Error_DataType
	B_CALL PopRealO1
	ld a,(OP1)
	or a
	jp nz,Parse_Error_DataType
	rst rpushrealo1
	B_CALL PushRealO2
	B_CALL FPDiv
	B_CALL Trunc
	B_CALL PopRealO2
	B_CALL FPMult
	B_CALL OP1ToOP2
	B_CALL PopRealO1
	B_CALL FPSub
	jp Parse_Token_ExitGood
Parse_Constant:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	ld a,(OP1)
	cp StrngObj
	jp nz,Parse_Error_Argument
	B_CALL ChkFindSym
	jp c,Parse_Error_Undefined
	ld hl,Parse_Constant_Table
Parse_Constant_CheckLoop:
	push de
	ld c,(hl)
	inc hl
	ld b,(hl)
	dec hl
	ld a,b
	or c
	jp z,Parse_Error_Argument
	ld b,(hl)
	inc b
	inc b
	push hl
	push bc
	call CompareStrings
	pop bc
	jr nz,Parse_Constant_CheckLoop2_Failed
	pop de
	;Found match...
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	;ld a,h
	;or l
	;jr z,Parse_Constant_SaveTheManatees
	rst 20h
	pop de
	or 1
	ret
Parse_Constant_CheckLoop2_Failed:
	pop hl
	ld c,b
	ld b,0
	add hl,bc
	inc hl
	inc hl
	pop de
	jr Parse_Constant_CheckLoop
;Parse_Constant_SaveTheManatees:
	;*cough* :)
;	call GetRealSettings
;	bit 7,(hl)
;	jr z,Parse_Constant_SaveTheManatees_Skip
;	ld hl,Manatee_Hook
;	in a,(6)
;	B_CALL SetCursorHook
;	xor a
;	ld (manateebyte),a
;Parse_Constant_SaveTheManatees_Skip:
;	B_CALL OP1Set0
;	pop de
;	xor a
;	inc a
;	ret
Parse_Constant_Table:
	DW 2
	DB "NA"
	DW Parse_Constant_Na
	DW 1
	DB "K"
	DW Parse_Constant_k
	DW 2
	DB "CC"
	DW Parse_Constant_Cc
	DW 2
	DB "EC"
	DW Parse_Constant_ec
	DW 2
	DB "RC"
	DW Parse_Constant_Rc
	DW 2
	DB "GC"
	DW Parse_Constant_Gc
	DW 1
	DB "G"
	DW Parse_Constant_g
	DW 2
	DB "ME"
	DW Parse_Constant_Me
	DW 2
	DB "MP"
	DW Parse_Constant_Mp
	DW 2
	DB "MN"
	DW Parse_Constant_Mn
	DW 1
	DB "H"
	DW Parse_Constant_h
	DW 1
	DB "C"
	DW Parse_Constant_c
	DW 1
	DB "U"
	DW Parse_Constant_u
	;DW 25
	;DB "I",tSpace,"SHALL",tSpace,"SAVE",tSpace,"THE",tSpace,"MANATEES"
	;DW 0
	DW 0FFFFh
Parse_Constant_Na:
	DB 00h,97h,60h,22h,13h,67h,00h,00h,00h
Parse_Constant_k:
	DB 00h,69h,13h,80h,65h,80h,00h,00h,00h
Parse_Constant_Cc:
	DB 00h,89h,89h,87h,55h,17h,87h,37h,00h
Parse_Constant_ec:
	DB 00h,6Dh,16h,02h,17h,73h,30h,00h,00h
Parse_Constant_Rc:
	DB 00h,80h,83h,14h,51h,00h,00h,00h,00h
Parse_Constant_Gc:
	DB 00h,75h,66h,72h,59h,00h,00h,00h,00h
Parse_Constant_g:
	DB 00h,80h,98h,06h,65h,00h,00h,00h,00h
Parse_Constant_Me:
	DB 00h,61h,91h,09h,38h,97h,00h,00h,00h
Parse_Constant_Mp:
	DB 00h,65h,16h,72h,62h,31h,00h,00h,00h
Parse_Constant_Mn:
	DB 00h,65h,16h,74h,92h,86h,00h,00h,00h
Parse_Constant_h:
	DB 00h,5Eh,66h,26h,07h,55h,00h,00h,00h
Parse_Constant_c:
	DB 00h,88h,29h,97h,92h,45h,80h,00h,00h
Parse_Constant_u:
	DB 00h,65h,16h,60h,54h,02h,00h,00h,00h
Parse_Subfactorial:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	B_CALL CkPosInt
	jp nz,Parse_Error_DataType
	;OP1 is N
	ld de,nvariable
	B_CALL MovFrOP1
	B_CALL OP1Set0
	ld de,kvariable
	B_CALL MovFrOP1
	ld de,summation
	B_CALL MovFrOP1
Parse_Subfactorial_Loop:
	;Loop is set up...
	ld hl,Parse_Negative1
	rst 20h
	ld hl,kvariable
	B_CALL Mov9ToOP2
	B_CALL YToX
	;OP1 now has numerator of summation fraction
	B_CALL OP1ToOP5
	;OP5 is numerator
	ld hl,kvariable
	rst 20h
	B_CALL Factorial
	;OP1 is denominator
	B_CALL OP1ToOP2
	B_CALL OP5ToOP1
	B_CALL FPDiv
	;summation evaluation complete
	;OP1 is answer...
	ld hl,summation
	B_CALL Mov9ToOP2
	B_CALL FPAdd
	ld de,summation
	B_CALL MovFrOP1
	ld hl,kvariable
	rst 20h
	;OP1 = K
	B_CALL Plus1
	ld de,kvariable
	B_CALL MovFrOP1
	ld hl,nvariable
	B_CALL Mov9ToOP2
	B_CALL CpOP1OP2
	jr z,Parse_Subfactorial_Loop
	jr c,Parse_Subfactorial_Loop
	B_CALL OP2ToOP1
	B_CALL Factorial
	ld hl,summation
	B_CALL Mov9ToOP2
	B_CALL FPMult
	jp Parse_Token_ExitGood
Parse_Play:
	dec a
	jp nz,Parse_Error_Argument
	;Well let's initialize variables now
	in a,(2)
	rla
	jr nc,Parse_Play_NotSE
	xor a
	out (20h),a
Parse_Play_NotSE:
	ld a,120
	ld (bpm),a
	ld a,2
	ld (octave),a
	xor a
	ld (style),a
	ld a,4
	ld (notelen),a
	call Play_CalculateDurations
	B_CALL PopRealO1
	ld a,(OP1)
	cp StrngObj
	jp nz,Parse_Error_Argument
	B_CALL ChkFindSym
	jp c,Parse_Error_Argument
	ld a,b
	or a
	jp nz,Parse_Error_Argument
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld a,b
	or c
	jp z,Parse_Error_Argument
	inc hl
	ld (playptr),hl
	dec hl
	add hl,bc
	ld (playend),hl
Play_Loop:
	call Play_FetchByte
	jp c,Play_Break
	cp tSpace
	jr z,Play_Loop
	cp tL
	jp z,Play_SetNoteLen
	cp tM
	jp z,Play_SetStyle
	cp tP
	jp z,Play_Pause
	cp tT
	jp z,Play_SetTempo
	cp tO
	jp z,Play_SetOctave
	cp tGT
	jp z,Play_RaiseOctave
	cp tLT
	jp z,Play_LowerOctave
	cp tN
	jp z,Play_SpecificNote
	cp tX
	jp z,Play_SilentLink
	cp tA
	jp c,Parse_Error_Syntax
	cp tG+1
	jp nc,Parse_Error_Syntax
	;Now play A-G
	sub tA		;0 for A
	ld hl,Play_NotesTable
	ld d,0
	ld e,a
	add hl,de
	ld c,(hl)		;the note position
	ld a,(octave)
	add a,a
	add a,a
	ld b,a
	add a,a
	add a,b
	add a,c		;(octave) * 12 + note
	inc a
	ld (notetoplay),a		;note is 1-84
	;We have to unfortunately check for things like sharps, flats, dotted notes
Play_Note_CheckSpecial:
	call Play_FetchByte
	jr c,Play_Note_Periods
	cp tSub
	jr z,Play_Note_Flat
	cp tAdd
	jp nz,Play_Note_Sharp_Back
	ld a,(notetoplay)
	inc a
	add a,(256-85)
	sbc a,(256-85)
	ld (notetoplay),a
	jr Play_Note_CheckSpecial
Play_Note_Flat:
	ld a,(notetoplay)
	sub 1
	adc a,0
	ld (notetoplay),a
	jr Play_Note_CheckSpecial
Play_Note_Periods:
	;Now we must check for dotted notes
	;First calculate the note's normal duration
	ld a,(style)
	or a
	jr z,Play_Note_NormalStyle
	dec a
	jr z,Play_Note_LegatoStyle
	ld hl,(staccato)
	ld a,(staccato+2)
	jr Play_Note_StyleLoaded
Play_Note_NormalStyle:
	ld hl,(normal)
	ld a,(normal+2)
	jr Play_Note_StyleLoaded
Play_Note_LegatoStyle:
	ld hl,(legato)
	ld a,(legato+2)
Play_Note_StyleLoaded:
	ld b,a
	call Play_FetchByte
	jr c,Play_Note_SetupLength
	push bc
	push hl
	cp t0
	jp c,Play_Note_NoDigitAfter
	cp t9+1
	jp nc,Play_Note_NoDigitAfter
	sub t0
	call Play_CheckNoteLength_NotFirst
	pop hl
	pop bc
	jr Play_Note_Length_Loop
Play_Note_SetupLength:
	ld a,(notelen)
Play_Note_Length_Loop:
	srl a
	jr z,Play_Note_DurationReady
	srl b
	rr h
	rr l		;Divide duration by 2
	jr Play_Note_Length_Loop
Play_Note_DurationReady:
	;Now check for dots
	ld c,b
	ld d,h
	ld e,l		;CDE is the duration to add half of...BHL will be the total duration
Play_Note_EatDots:
	call Play_FetchByte
	jr c,Play_Note_Ready
	cp tDecPt
	jp nz,Play_Note_DotsBack
	srl c
	rr d
	rr e
	add hl,de
	ld a,b
	adc a,c
	ld b,a
	jr Play_Note_EatDots
Play_Note_Ready:
	;Now just check if it's a pause
	ld a,(notetoplay)
	or a
	jr z,Play_Note_Pause
	push bc
	push hl
	ld hl,Frequencies-2
	add a,a
	ld d,0
	ld e,a
	add hl,de
	ld c,(hl)
	inc hl
	ld b,(hl)
	;BC is the period
	pop hl
	pop de
	;DHL is the duration
	ld (noteduration),hl
	ld a,d
	ld (noteduration+2),a
	call Sound_Routine
Play_Note_PauseAfter:
	;Now have the filling pause
	ld hl,(noteduration)
	ld a,(noteduration+2)
	ld b,a
	ld a,(style)
	dec a
	jp z,Play_Loop
	dec a
	jr z,Play_Note_Staccato
	srl b
	rr h
	rr l
Play_Note_Staccato:
	;Staccato pause
	srl b
	rr h
	rr l
	srl b
	rr h
	rr l
	call Play_PauseRoutine
	jp Play_Loop	
Play_Note_Pause:
	call Play_PauseRoutine
	ld a,(notelen2)
	ld (notelen),a
	jr Play_Note_PauseAfter
Play_PauseRoutine:
	;BHL is the duration
	;We need a 136 clock cycle loop
	srl b
	rr h
	rr l
	inc b
	inc hl
	di
	exx
	ld bc,0
	ld de,1
	ld hl,1
	exx
	ld e,8
	ld a,e
Play_Note_Pause_Extra:
	and e		;4
	jr z,Pause_Break		;7
Play_Note_Pause_Loop:
	exx	;4
	;Let's make some Fibonnaci numbers! (outputted in CHL, of course)
	push hl		;11
	add hl,de	;11
	ld a,c		;4
	push af		;11
	adc a,b		;4
	ld c,a		;4
	pop af		;10
	pop de		;10
	ld b,a		;4
	exx	;4
	ld a,1	;7
	nop		;4
	nop		;4
	dec hl	;6
	ld a,h	;4
	or l		;4
	in a,(4)		;11
	jr nz,Play_Note_Pause_Extra	;12 taken, 7 not
	dec b		;4
	jr nz,Play_Note_Pause_Loop	;12
	ret
Pause_Break:
	pop hl
	jp Play_Break
Play_Note_Sharp_Back:
	call Play_BackByte
	jp Play_Note_Periods
Play_Note_DotsBack:
	call Play_BackByte
	jp Play_Note_Ready
Play_NotesTable:
	DB 9	;A
	DB 11	;B
	DB 0	;C
	DB 2	;D
	DB 4	;E
	DB 5	;F
	DB 7	;G
Play_SpecificNote:
	call Play_FetchByte
	jp c,Parse_Error_Syntax
	call Play_Check0To9
	sub t0
	ld c,a
	call Play_FetchByte
	jr c,Play_SpecificNote_Check
	cp t0
	jr c,Play_SpecificNote_Back
	cp t9+1
	jr nc,Play_SpecificNote_Back
	sub t0
	ld d,a
	ld a,c
	add a,a
	ld b,a
	add a,a
	add a,a
	add a,b
	add a,d
	ld c,a
Play_SpecificNote_Check:
	ld a,c		;the note number
	cp 85
	jp nc,Parse_Error_Syntax
	ld (notetoplay),a		;now check for periods...but not sharps/flats
	jp Play_Note_Periods
Play_SpecificNote_Back:
	call Play_BackByte
	jr Play_SpecificNote_Check
Play_RaiseOctave:
	ld a,(octave)
	cp 6
	jp z,Play_Loop
	inc a
	ld (octave),a
	jp Play_Loop
Play_LowerOctave:
	ld a,(octave)
	dec a
	jp m,Play_Loop
	ld (octave),a
	jp Play_Loop
Play_SetOctave:
	call Play_FetchByte
	cp t0
	jp c,Parse_Error_Syntax
	cp t6+1
	jp nc,Parse_Error_Syntax
	sub t0
	ld (octave),a
	jp Play_Loop
Play_SetTempo:
	ld bc,3*256+0
Play_SetTempo_ReadLoop:
	call Play_FetchByte
	jp c,Play_SetTempo_NotDigit
	cp t0
	jp c,Play_SetTempo_Back
	cp t9+1
	jp nc,Play_SetTempo_Back
	sub t0
	ld d,a
	ld a,c
	add a,a
	ld c,a
	add a,a
	add a,a
	add a,c
	add a,d
	ld c,a
	djnz Play_SetTempo_ReadLoop
Play_SetTempo_NotDigit:
	ld a,c
	cp 32
	jp c,Parse_Error_Syntax
	ld (bpm),a
	call Play_CalculateDurations
	jp Play_Loop
Play_SetTempo_Back:
	call Play_BackByte
	jr Play_SetTempo_NotDigit
Play_SetStyle:
	call Play_FetchByte
	jp c,Parse_Error_Syntax
	ld c,0
	cp tN
	jr z,Play_Style_Set
	inc c
	cp tL
	jr z,Play_Style_Set
	inc c
	cp tS
	jr z,Play_Style_Set
	jp Parse_Error_Syntax
Play_Style_Set:
	ld a,c
	ld (style),a		;0 for normal, 1 for legato, 2 for staccato
	jp Play_Loop
Play_Note_NoDigitAfter:
	call Play_BackByte
	pop hl
	pop bc
	jp Play_Note_SetupLength
Play_Pause:
	call Play_CheckNoteLength		;make sure it's a valid pause
	ld c,a
	ld a,(notelen)
	ld (notelen2),a
	ld a,c
	ld (notelen),a
	xor a
	ld (notetoplay),a
	jp Play_Note_Periods
Play_SetNoteLen:
	call Play_CheckNoteLength
	ld (notelen),a
	jp Play_Loop
Play_CheckNoteLength:
	call Play_FetchByte
	jp c,Parse_Error_Syntax		;The L was at the end with no note length defined!
	call Play_Check0To9
	sub t0
Play_CheckNoteLength_NotFirst:
	ld c,a
	call Play_FetchByte
	jp c,Play_SetNoteLen_Other
	cp t0
	jr c,Play_SetNoteLen_Back
	cp t9+1
	jr nc,Play_SetNoteLen_Back
	sub t0
	ld d,a
	ld a,c
	add a,a
	ld b,a
	add a,a
	add a,a
	add a,b	;A = A *10
	add a,d
	ld c,a
Play_SetNoteLen_Other:
	ld a,c
	ld hl,Play_NoteLengths
	ld bc,7
	cpir
	jp nz,Parse_Error_Syntax
	ret
Play_SetNoteLen_Back:
	call Play_BackByte
	ld a,c
	jr Play_SetNoteLen_Other
Play_NoteLengths:
	DB 1,2,4,8,16,32,64
Play_Check0To9:
	cp t0
	jp c,Parse_Error_Syntax
	cp t9+1
	jp nc,Parse_Error_Syntax
	ret
Play_BackByte:
	push hl
	ld hl,(playptr)
	dec hl
	ld (playptr),hl
	pop hl
	ret
Play_FetchByte:
	;Byte in A....or C flag if at the end 
	push de
	push hl
	ld de,(playptr)
	ld a,(de)
	ld hl,(playend)
	or a
	sbc hl,de
	jr c,Play_FetchByte_Exit
	inc de
	ld (playptr),de
Play_FetchByte_Exit:
	pop hl
	pop de
	ret
Play_Divide32:
	ld c,a
	xor a
	ld b,32
Play_CalculateDurations_Loop:
	add hl,hl
	rl e
	rl d
	rla
	jr c,Play_CalculateDurations_Fit
	cp c
	jr c,Play_CalculateDurations_NoFit
Play_CalculateDurations_Fit:
	sub c
	inc l
Play_CalculateDurations_NoFit:
	djnz Play_CalculateDurations_Loop
	ret
Play_CalculateDurations:
	;Given a quarter-note tempo in (bpm), calculate the length of a whole note in the three
	;possible styles.
	ld hl,9748h
	ld de,0150h
	ld a,(bpm)
	call Play_Divide32
	;DEHL...well actually EHL, is the length of the whole note
	ld (legato),hl
	ld a,e
	ld (legato+2),a
	push af
	push hl
	;Now we must calculate 7/8 of this for normal
	ld b,a
	ld d,h
	ld e,l
	add hl,hl
	adc a,a
	add hl,hl
	adc a,a
	add hl,hl
	adc a,a
	or a
	sbc hl,de
	sbc a,b
	srl a
	rr h
	rr l
	srl a
	rr h
	rr l
	srl a
	rr h
	rr l
	ld (normal),hl
	ld (normal+2),a
	;Now we must calculate 3/4 of legato for staccato
	pop hl
	pop af
	ld d,h
	ld e,l
	ld b,a
	add hl,hl
	adc a,a
	add hl,de
	adc a,b
	srl a
	rr h
	rr l
	srl a
	rr h
	rr l
	ld (staccato),hl
	ld (staccato+2),a
	ret
Play_SilentLink:
	ld hl,Silentlink
	in a,(6)
	B_CALL SetLinkHook
	ld c,1
	jp Play_BreakNoLink
Silentlink:
	add a,e
	in a,(2)
	and 80h
	ret z
	in a,(9)
	ld a,80h
	out (8),a
	xor a
	ret
Sound_Routine:
	;91.91176471 kHz
	di
	dec bc
	ld (appbackupscreen),bc		;period
	ld a,h
	or l
	jr nz,Sound_Routine_Ok
	inc hl
Sound_Routine_Ok:
	ex af,af'
	ld a,0D0h
	ex af,af'
	inc d
	ld e,8
	ld a,e
Sound_Routine_ExtraDelay:
	and e		;4
	jr z,Play_Break2	;7
Sound_Routine_Loop:
	cpd	;16
	jp pe,Sound_Routine_SkipInvert	;10
	ex af,af'	;4
	xor 3		;7
	out (0),a	;11
	ex af,af'	;4
	ld bc,(appbackupscreen)	;20
	ld a,r	;9
	ld a,r	;9
	nop		;4
Sound_Routine_SkipInvert:
	ld a,h	;4
	or l		;4
	in a,(4)		;11
	jr nz,Sound_Routine_ExtraDelay	;12 taken, 7 not
	dec d		;4
	jr nz,Sound_Routine_Loop	;12
	ld a,0D0h
	out (0),a
	ret
Play_Break2:
	ld c,0
	jr Play_RealBreak
Play_Break:
	ld c,1
Play_RealBreak:
	ld a,0D0h
	out (0),a
Play_BreakNoLink:
	ei
	ld a,c
	B_CALL SetXXOP1
	in a,(2)
	and 80h
	rlca
	jp nc,Parse_Token_ExitGood
	out (20h),a
	jp Parse_Token_ExitGood
Frequencies:
	DW 1405	; 32.7
	DW 1326	; 34.65
	DW 1252	; 36.71
	DW 1182	; 38.89
	DW 1115	; 41.2
	DW 1053	; 43.65
	DW 994	; 46.25
	DW 938	; 49
	DW 885	; 51.91
	DW 836	; 55
	DW 789	; 58.27
	DW 744	; 61.74
	DW 703	; 65.41
	DW 663	; 69.3
	DW 626	; 73.42
	DW 591	; 77.78
	DW 558	; 82.41
	DW 526	; 87.31
	DW 497	; 92.5
	DW 469	; 98
	DW 443	; 103.83
	DW 418	; 110
	DW 394	; 116.54
	DW 372	; 123.47
	DW 351	; 130.81
	DW 332	; 138.59
	DW 313	; 146.83
	DW 295	; 155.56
	DW 279	; 164.81
	DW 263	; 174.61
	DW 248	; 185
	DW 234	; 196
	DW 221	; 207.65
	DW 209	; 220
	DW 197	; 233.08
	DW 186	; 246.94
	DW 176	; 261.63
	DW 166	; 277.18
	DW 156	; 293.66
	DW 148	; 311.13
	DW 139	; 329.63
	DW 132	; 349.23
	DW 124	; 369.99
	DW 117	; 392
	DW 111	; 415.3
	DW 104	; 440
	DW 99	; 466.16
	DW 93	; 493.88
	DW 88	; 523.25
	DW 83	; 554.37
	DW 78	; 587.33
	DW 74	; 622.25
	DW 70	; 659.26
	DW 66	; 698.46
	DW 62	; 739.99
	DW 59	; 783.99
	DW 55	; 830.61
	DW 52	; 880
	DW 49	; 932.33
	DW 47	; 987.77
	DW 44	; 1046.5
	DW 41	; 1108.73
	DW 39	; 1174.66
	DW 37	; 1244.51
	DW 35	; 1318.51
	DW 33	; 1396.91
	DW 31	; 1479.98
	DW 29	; 1567.98
	DW 28	; 1661.22
	DW 26	; 1760
	DW 25	; 1864.66
	DW 23	; 1975.53
	DW 22	; 2093
	DW 21	; 2217.46
	DW 20	; 2349.32
	DW 18	; 2489.02
	DW 17	; 2637.02
	DW 16	; 2793.83
	DW 16	; 2959.96
	DW 15	; 3135.96
	DW 14	; 3322.44
	DW 13	; 3520
	DW 12	; 3729.31
	DW 12	; 3951.07
Parse_RestoreMemory:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	in a,(2)
	and 80h
	jp z,Parse_Error_Range
	ld a,1
	out (20h),a
	di
	;Now check for corruption.
	ld a,86h
	out (7),a
	ld hl,cxMain_BackupRAM_Checksum
	ld de,8000h
	ld b,16
	call CompareStrings
	ld a,81h
	out (7),a
	jp nz,Parse_Error_DataType
	ld a,86h
	out (7),a
	ld hl,(8050h)
	push hl
	ld a,87h
	out (7),a
	ld hl,8000h
	ld bc,4000h
	ld ix,0
Parse_RestoreMemory_Checksum:
	ld e,(hl)
	ld d,0
	add ix,de
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,Parse_RestoreMemory_Checksum
	push ix
	pop de
	pop hl
	ld a,81h
	out (7),a
	or a
	sbc hl,de
	jp nz,Parse_Error_DataType
	;we are go
	di
	ld de,8681h
	call CopyRAMPageD_To_E
	ld sp,(savesscreen)
	ld a,87h
	out (7),a
	ld hl,8000h
	ld bc,4000h
	ld de,0C000h
	ldir
	ld a,81h
	out (7),a
	;All done...with the RAM part.
	;ld hl,appbackupscreen
	;call CopyLayer
	ei
	ret
Parse_Asm:
	dec a
	jp nz,Parse_Error_Argument
	B_CALL PopRealO1
	ld a,(OP1)
	cp StrngObj
	jp nz,Parse_Error_Syntax
	rst rFINDSYM
	jp c,Parse_Error_Argument
	ld a,b
	or a
	jp nz,Parse_Error_Argument
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	push hl
	ld hl,767*2
	sbc hl,bc
	pop hl
	jp c,Parse_Error_Syntax
	srl b
	rr c
	jp c,Parse_Error_Syntax
	ld de,appbackupscreen
Parse_Asm_Loop:
	push bc
	ld a,(hl)
	call Parse_Asm_Fetch
	add a,a
	add a,a
	add a,a
	add a,a
	ld b,a
	inc hl
	ld a,(hl)
	call Parse_Asm_Fetch
	or b
	ld (de),a
	inc de
	pop bc
	cpi
	jp pe,Parse_Asm_Loop
	ld a,0C9h
	ld (de),a
	call appbackupscreen
	B_CALL OP1Set1
	jp Parse_Token_ExitGood
Parse_Asm_Fetch:
	cp tF+1
	jp nc,Parse_Error_Syntax
	sub t0
	jp c,Parse_Error_Syntax
	cp 10
	ret c
	sub 7
	jp c,Parse_Error_Syntax
	ret
Parse_Negative1:
	DB 80h,80h,10h,00h,00h,00h,00h,00h,00h
Parse_Token_ExitGood:
	xor a
	inc a
	ret
Parse_NotReal:
	xor a
	ret
Parser_JumpTable:
	DW Parse_Sprite
	DW Parse_RomanDecimal
	DW Parse_RadicalSimplify
	DW Parse_FactorNumber
	DW Parse_Gamma
	DW Parse_Base
	DW Parse_LinkGet
	DW Parse_LinkSend
	DW Parse_Modulo
	DW Parse_Constant
	DW Parse_Subfactorial
	DW Parse_Play
	DW Parse_RestoreMemory
	DW Parse_Asm
Parse_Error_Range:
Parse_Error_Domain:
	B_JUMP ErrDomain
Parse_Error_DataType:
	B_JUMP ErrDataType
Parse_Error_Break:
	B_JUMP ErrBreak
Parse_Error_Syntax:
	B_JUMP ErrSyntax
Parse_Error_Argument:
	B_JUMP ErrArgument
Parse_Error_Undefined:
	B_JUMP ErrUndefined
;---------= Large Sprite =---------
; Draws a Large sprite.
;Input:	ix->sprite
;	a=x
;	l=y
;	b=height	(in pixels)
;	c=width		(in bytes, e.g. 2 would be 16)
;Output: nothing
; All registers are destroyed except bc', de', hl'
;
largeSprite:
	di
	ex af,af'
	ld a,c
	push af
	ex af,af'
	ld e,l
	ld h,0
	ld d,h
	add hl,de
	add hl,de
	add hl,hl
	add hl,hl
	ld e,a
	and 07h
	ld c,a
	srl e
	srl e
	srl e
	add hl,de
	ld de,plotsscreen
	add hl,de
lsl0:
 push hl
lsl1:
	ld d,(ix)
	ld e,0
	ld a,c
	or a
	jr z,lsl3
lsl2:
	srl d
	rr e
	dec a
	jr nz,lsl2
lsl3:
	ld a,(hl)
	xor d
	ld (hl),a
	inc hl
	ld a,(hl)
	xor e
	ld (hl),a
	inc ix
	ex af,af'
	dec a
	push af
	ex af,af'
	pop af
	jr nz,lsl1
	pop hl
	pop af
	push af
	ex af,af'
	ld de,0Ch
	add hl,de
	djnz lsl0
	pop af
	ei
	ret
	;---------= Large Sprite =---------
; Draws a Large sprite.
;Input:	ix->sprite
;	a=x
;	l=y
;	b=height	(in pixels)
;	c=width		(in bytes, e.g. 2 would be 16)
;Output: nothing
; All registers are destroyed except bc', de', hl'
;
largeSpriteOR:
	di
	ex af,af'
	ld a,c
	push af
	ex af,af'
	ld e,l
	ld h,0
	ld d,h
	add hl,de
	add hl,de
	add hl,hl
	add hl,hl
	ld e,a
	and 07h
	ld c,a
	srl e
	srl e
	srl e
	add hl,de
	ld de,plotsscreen
	add hl,de
lsl0OR:
 push hl
lsl1OR:
	ld d,(ix)
	ld e,0
	ld a,c
	or a
	jr z,lsl3OR
lsl2OR:
	srl d
	rr e
	dec a
	jr nz,lsl2OR
lsl3OR:
	ld a,(hl)
	or d
	ld (hl),a
	inc hl
	ld a,(hl)
	or e
	ld (hl),a
	inc ix
	ex af,af'
	dec a
	push af
	ex af,af'
	pop af
	jr nz,lsl1OR
	pop hl
	pop af
	push af
	ex af,af'
	ld de,0Ch
	add hl,de
	djnz lsl0OR
	pop af
	ei
	ret
Parser_Symbolic_Token:
	push af
	ld hl,SymbolicApp
	rst 20h
	B_CALL FindApp
	jr c,Parser_Symbolic_Token_NotFound
	ld b,a
	pop af
	ld c,a
	push bc
	ld hl,Parser_Symbolic_Jumper
	ld de,9872h
	ld bc,Parser_Symbolic_Jumper_End-Parser_Symbolic_Jumper
	ldir
	pop bc
	jp 9872h
Parser_Symbolic_Token_NotFound:
	pop af
	jp Parse_Error_Undefined
Parser_Symbolic_Jumper:
	;B is Symbolic's ROM page
	;C is token value
	ld a,b
	out (6),a
	ld hl,4082h
	ld d,0
	ld e,c
	sla e
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	;HL is where to jump to...
	in a,(2)
	and 80h
	jr nz,Parser_Symbolic_Jumper_SE
	jp (hl)
Parser_Symbolic_Jumper_SE:
	xor a
	out (20h),a
	jp (hl)
Parser_Symbolic_Jumper_End:
Parse_Log:
	ld a,l
	or a
	ret z
	dec a
	ret z
	dec a
	jr z,Parse_Log_Base
	jp Parse_Error_Argument
Parse_Log_Base:
	B_CALL LogX
	B_CALL OP1ToOP6
	B_CALL PopRealO1
	B_CALL LogX
	B_CALL OP6ToOP2
	B_CALL FPDiv
	or 1
	ret
