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
Hook_Key:
	;Key hook for _getkey
	add a,e
	ld e,a
	cp kMode
	call z,Hook_Key_Mode
	cp kAppsMenu
	call z,Hook_Key_Apps
Hook_Key_Apps_ReturnPoint:
	ld a,e
	cp 91h
	call z,Hook_Key_Prgm_Second
	cp k5
	call z,Hook_Key_Custom_Second
	cp kPrgm
	jp nz,Hook_Key_Other
	ld a,(menucurrent)
	cp 03h
	jp z,Hook_Key_Prgm
	cp 2Ah
	jp z,Hook_Key_Prgm
	ld a,kPrgm
	ret
Hook_Key_Apps:
	in a,(4)
	bit 3,a
	jr nz,Hook_Key_Apps_AppsReturn
	B_CALL ZIfRclHandler
	jr z,Hook_Key_Apps_AppsReturn
	;On+Apps pressed. Now check for MirageOS
	ld hl,MirageOS
	rst 20h
	B_CALL FindApp
	jr c,Hook_Key_Apps_AppsReturn
	;Run MirageOS
	ld hl,MirageOS+1
	ld de,progToEdit
	ld bc,8
	ldir
	B_CALL CloseEditEqu
	B_CALL ExecuteApp
MirageOS:
	DB AppObj,"MirageOS"
Hook_Key_Apps_AppsReturn:
	call GetRealSettings
	inc hl
	bit 2,(hl)
	jp z,Hook_Key_Apps_Return
	set 3,(iy+asm_Flag1)
	bit 3,(hl)
	jr z,$F
	res 3,(iy+asm_Flag1)
$$:
 INCLUDE "appsmenu.asm"
Hook_Key_Apps_Return:
	ld a,kAppsMenu
	ld e,a
	ret
 INCLUDE "vcalc.asm"
Hook_Key_Other:
	cp kLog
	jp z,Hook_Key_Base
	cp kMath
	jp z,Hook_Key_Math
	cp kVars
	jp z,Hook_Key_Vars
	cp 0Dh
	jp z,Hook_Key_Entry
	cp k0
	jp z,Hook_Key_TokenShow
	cp kDel
	jp z,Hook_Key_MemoryDel
	cp 0ACh
	jp z,Hook_Key_CheckForModeScreen
	ld c,a
	ld a,(menucurrent)
	cp 0Ch	;Memory menu
	jp z,Hook_Key_MemoryMenu
	cp 45h
	jr z,Hook_Key_Catalog
Hook_Key_CRet:
	ld a,c
Hook_Key_MemoryReturnOK:
	or a
	ret
Hook_Key_Catalog_AlphaCheck:
	ld e,c
	ld hl,Hook_Key_Catalog_Alphas
	ld bc,8
	cpir
	ld a,e
	jr nz,Hook_Key_MemoryReturnOK
	ld c,e
	ld de,7
	add hl,de
	ld a,(hl)
	jr Hook_Key_Catalog_AlphaNumbersGO
Hook_Key_Catalog_Alphas:
	DB kCapO,kCapP,kCapT,kCapU,kCapV,kCapY,kCapZ,kTheta
	DB 6,7,3,4,5,0,1,2
Hook_Key_Catalog:
	ld a,c
	cp k1
	jr c,Hook_Key_Catalog_AlphaCheck
	cp k9
	jr nc,Hook_Key_Catalog_AlphaCheck
	sub k1
Hook_Key_Catalog_AlphaNumbersGO:
	ld d,a
	in a,(4)
	bit 3,a
	jr nz,Hook_Key_CRet
	ld a,(localLanguage)
	cp LANG_ENGLISH
	jr nz,Hook_Key_CRet
	ld a,d
	push af
	;1-8 pressed with On
	call Custom_Lookup
	jp c,Parse_Error_Undefined
	ld a,b
	or a
	jp z,Hook_Key_Catalog_Unarchived
	call EditBuffer1
	B_CALL Arc_Unarc
	call EditBuffer2
	call Custom_Lookup
Hook_Key_Catalog_Unarchived:
	ex de,hl
	inc hl
	inc hl
	pop af
	add a,a
	ld d,0
	ld e,a
	add hl,de
	;HL is where to insert
	push hl
	ld hl,CatalogKeyLoader
	ld de,ramCode
	ld bc,CatalogKeyLoader_End-CatalogKeyLoader
	ldir
	call ramCode
	pop hl
	ld a,e
	cp 0FCh
	jr z,Catalog_Insert_Two
	cp 0FDh
	jr z,Catalog_Insert_Two
	cp 0FEh
	jr z,Catalog_Insert_Two
	cp 0FFh
	jr z,Catalog_Insert_TwoDec
	ld (hl),d
	xor a
	ret
Catalog_Insert_TwoDec:
	dec e
Catalog_Insert_Two:
	ld (hl),e
	inc hl
	ld (hl),d
	xor a
	ret
CatalogKeyLoader:
	in a,(6)
	push af
	;B_CALL 5056h appears to be the B_CALL to insert the current catalog selection
	;We use this to determine which page the catalog table is on.
	ld b,7Bh
	in a,(2)
	rla
	jr c,$F
	ld b,1Bh
$$:
	ld a,b
	out (6),a
	ld a,(5058h)
	out (6),a
	ld hl,(CatalogCurrent)
	ld e,(hl)
	inc hl
	ld d,(hl)
	pop af
	out (6),a
	ret
CatalogKeyLoader_End:
Hook_Key_MemoryDel:
	ld a,(cxCurApp)
	cp cxErase
	jr nz,Hook_Key_MemoryDel_Del
	call GetRealSettings
	bit 2,(hl)
	jr z,Hook_Key_MemoryDel_Del
	xor a
	ret
Hook_Key_MemoryDel_Del:
	ld a,kDel
	or a
	ret
Hook_Key_MemoryMenu:
	ld a,c
	cp kEnter
	jr z,Hook_Key_MemoryMenu_Enter
	cp k7
	jp nz,Hook_Key_MemoryReturnOK
	call GetRealSettings
	bit 2,(hl)
	jr z,Hook_Key_MemoryMenu_7No
	xor a
	ret
Hook_Key_MemoryMenu_7No:
	ld a,k7
	or a
	ret
Hook_Key_MemoryMenu_Enter:
	call GetRealSettings
	bit 2,(hl)
	jr z,Hook_Key_MemoryMenu_EnterNo
	ld a,(85E0h)	;Check menu number
	cp 6
	jr nz,Hook_Key_MemoryMenu_EnterNo
	xor a
	ret
Hook_Key_MemoryMenu_EnterNo:
	ld a,kEnter
	or a
	ret
Hook_Key_CheckForModeScreen:
	ld a,(cxCurApp)
	cp cxMode
	jr nz,Hook_Key_CheckForModeScreenNo
	call GetRealSettings
	bit 2,(hl)
	jr z,Hook_Key_CheckForModeScreenNo
	xor a
	ret
Hook_Key_CheckForModeScreenNo:
	ld a,0ACh
	or a
	ret
Hook_Key_Prgm_Second:
	ld a,(tokenHookPtr+3)
	cp 3
	jr nc,Hook_Key_Prgm_Second_Real
	dec a
	jp nz,Hook_Key_Prgm_Second_No
	pop af
	ld a,2
	ld (tokenHookPtr+3),a
	ld a,kReal
	ld (keyExtend),a
	ld a,0FCh
	or a
	ret
Hook_Key_Prgm_Second_Real:
	;(rawkeyhookptr+3)
	cp 4
	jr z,Hook_Key_Prgm_Second_Real_Second
	cp 5
	jr z,Hook_Key_Prgm_Second_Real_Third
	cp 6
	jr z,Hook_Key_Prgm_Second_Real_Fourth
	cp 7
	jr z,Hook_Key_Prgm_Second_Real_Fifth
	ld a,4
	ld (tokenHookPtr+3),a
	ld a,12h
	ld (kbdscancode),a
	ld a,kReal
	ld (keyExtend),a
	ld a,0FCh
	set indicOnly,(iy+indicFlags)
	or a
	ret
DivideBy10:
	ld c,-1
Div10loop
	inc c
	sub 10
	jr nc,Div10loop
	add a,10
	ret
Hook_Key_Prgm_Second_Real_Second:
	ld a,5
	ld (tokenHookPtr+3),a
	ld a,12h
	ld (kbdscancode),a
	ld a,(rawKeyHookPtr+3)
	call DivideBy10
	ld a,c
	add a,k0
	or a
	ret
Hook_Key_Prgm_Second_Real_Third:
	ld a,12h
	ld (kbdscancode),a
	ld a,6
	ld (tokenHookPtr+3),a
	ld a,(rawKeyHookPtr+3)
	call DivideBy10
	add a,k0
	or a
	ret
Hook_Key_Prgm_Second_Real_Fourth:
	ld a,7
	ld (tokenHookPtr+3),a
	ld a,12h
	ld (kbdscancode),a
	ld a,kComma
	or a
	ret
Hook_Key_Prgm_Second_Real_Fifth:
	res indicOnly,(iy+indicFlags)
	xor a
	ld (tokenHookPtr+3),a
	ret
Hook_Key_Prgm_Second_No:
	ld a,91h
	or a
	ret
Hook_Key_Prgm:
	; (tokenhookptr+3) tells us 1 if a token was inserted, 2 if second time.
	; (rawkeyhookptr+3) tells us which token (last byte) to insert
	res 1,(iy+asm_Flag1)
	set 0,(iy+asm_Flag1)
	B_CALL RunIndicOff
	call Menu
Symbolic_JumpPoint:
	ld (rawkeyhookptr+3),a
	ld a,3
	ld (tokenHookPtr+3),a
	ld a,12h
	ld (kbdscancode),a
	ld a,kClear
	or a
	res cmdVirgin,(iy+cmdFlags)
	set indicOnly,(iy+indicFlags)
	ret
Menu:
	res indicrun,(iy+indicflags)
	res apptextsave,(iy+appflags)
	;If 3 columns columns go 0-2
	;If 5 rows rows would go 0-4
	xor a
	ld (cur_column),a
Menu_1:
	ld hl,FunctionsMenu
	xor a
	ld (menutop),a
	ld (curitem),a
	inc a
	ld (itemrow),a
	ld a,(hl)	;Get columns
	dec a
	ld (endcolumn),a
Menu_Loop_Refresh:
	call Menu_Display
Menu_Loop:
	B_CALL GetKey
	ld ix,Menu_Loop
	cp kAppsMenu
	jr z,Menu_Apps
	cp kDown
	jp z,Menu_Down
	cp kUp
	jp z,Menu_Up
	cp kRight
	jp z,Menu_Right
	cp kLeft
	jp z,Menu_Left
	cp kEnter
	jp z,Menu_Enter
	cp kAdd
	jr z,Menu_Help
	cp k1
	call nc,Menu_Number_Maybe
	ld hl,Menu_SpecialKeys
	ld c,(hl)
	inc hl
	ld b,0
	cpir
	jr nz,Menu_Loop
	pop hl
	or a
	ret
Menu_Number_Maybe:
	cp k9+1
	ret nc
	;We have 0-9
	sub k1
	ld b,a
	ld a,(enditem)
	cp b
	ld a,0	;for the return
	ret c
	push bc
	ld hl,FunctionsMenu+1
	ld a,(cur_column)
	add a,a
	ld b,0
	ld c,a
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	pop af
	ld c,(hl)
	cp c
	ret nc
	inc hl
	inc a
	ld b,a
Menu_Number_Search:
	push bc
	xor a
	ld bc,-1
	cpir
	pop bc
	djnz Menu_Number_Search
	ld a,(hl)
	pop hl
	ret
Menu_Apps:
	ld hl,Hook_Key_Apps_ReturnPoint
	ex (sp),hl
	jp Hook_Key_Apps
Menu_Help:
	ld hl,FunctionsMenu+1
	ld a,(cur_column)
	add a,a
	ld b,0
	ld c,a
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	inc hl
	;HL now points to start of menu data
	ld a,(curitem)
	inc a
	ld b,a
Menu_Help_Search:
	push bc
	xor a
	ld bc,-1
	cpir
	pop bc
	djnz Menu_Help_Search
	ld a,(hl)
	sub 20
	add a,a
	ld hl,Menu_Help_Table
	ld d,0
	ld e,a
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	;HL points to help stuff
	push hl
	B_CALL ClrLCDFull
	ld de,0
	ld (currow),de
	pop hl
	call PutScreen
Menu_Help_Pause:
	ei
	halt
	B_CALL GetCSC
	or a
	jr z,Menu_Help_Pause
	B_CALL ClrLCDFull
	call Menu_Display
	jp Menu_Loop
Menu_Help_Table:
	DW Menu_Help_Sprite
	DW Menu_Help_Roman
	DW Menu_Help_Simp
	DW Menu_Help_Factor
	DW Menu_Help_Gamma
	DW Menu_Help_Base
	DW Menu_Help_LinkGet
	DW Menu_Help_LinkSend
	DW Menu_Help_Modulo
	DW Menu_Help_Constant
	DW Menu_Help_Subfactorial
	DW Menu_Help_Play
	DW Menu_Help_RestoreMem
	DW Menu_Help_ExecAsm
Menu_Help_Sprite:
	DB "sprite(pic #,spr"
	DB "ite X,sprite Y,s"
	DB "prite width,spri"
	DB "te height,dest X"
	DB ",dest Y"
	DB 1,1
	DB "Press any key",0
Menu_Help_Roman:
	DB "Rom",5,"Dec(string)",1
	DB 1
	DB "string is Roman",1
	DB "Numerals. Ex:",1
	DB "Rom",5,"Dec(",lquote,"MCMLX",lquote,")",1
	DB 1
	DB "Press any key",0
Menu_Help_Simp:
	DB "simp",10h,"(number)",1
	DB 1
	DB "Simplifies the r"
	DB "adical of number"
	DB "(a positive inte"
	DB "ger.)",1,1
	DB "Press any key",0
Menu_Help_Factor:
	DB "factor(number)",1
	DB 1
	DB "Returns a list o"
	DB "f factors for nu"
	DB "mber (positive i"
	DB "nteger)",1,1
	DB "Press any key",0
Menu_Help_Gamma:
	DB "gamma(number)",1
	DB 1
	DB "number is nonneg"
	DB "ative. Returns",1
	DB "gamma of number.",1,1
	DB "Press any key",0
Menu_Help_Base:
	DB "baseInput(",34,"n",34,",b,"
	DB 0C1h,"new b]",1
	DB "Inputs integer",1
	DB "n, where n is in"
	DB "base b. Optional"
	DB "ly converts to",1
	DB "new b.",1
	DB "Press any key",0
Menu_Help_LinkGet:
	DB "linkGet(n)",1
	DB 1
	DB "Gets byte from l"
	DB "ink port. n=0 or"
	DB "(n=1 to wait for"
	DB "ever for byte).",1
	DB "-1 if failed.",1
	DB "Press any key",0
Menu_Help_LinkSend:
	DB "linkSend(byte)",1
	DB 1
	DB "Sends byte (0-25"
	DB "5) through link "
	DB "port. Returns 0 "
	DB "on work, -1 on  "
	DB "failure",1
	DB "Press any key",0
Menu_Help_Modulo:
	DB "mod(n1,n2)",1
	DB 1
	DB "Returns the",1
	DB "remainder of",1
	DB "n1/n2.",1,1
	DB 1
	DB "Press any key",0
Menu_Help_Constant:
	DB "const(string)",1
	DB 1
	DB "Returns a",1
	DB "constant like",1
	DB "NA,K,CC,EC,RC,",1
	DB "GC,G,ME,MP,MN,",1
	DB "H,C,U",1
	DB "Press any key",0
Menu_Help_Subfactorial:
	DB "!n(number)",1
	DB 1
	DB "Returns the",1
	DB "derangements of "
	DB "a list with",1
	DB "number elements.",1
	DB "Press any key",0
Menu_Help_Play:
	DB "play(string)",1
	DB 1
	DB "Please refer",1
	DB "to the manual",1
	DB "for help with",1
	DB "this.",1
	DB "Press any key",0
Menu_Help_RestoreMem:
	DB "Please refer",1
	DB "to the manual",1
	DB "for help.",0
Menu_Help_ExecAsm:
	DB "ExecAsm(string)",1
	DB 1
	DB "string is a",1
	DB "series of hex",1
	DB "opcodes.",1
	DB 1,"Press any key",0
PutScreen:
	ld a,(hl)
	inc hl
	or a
	ret z
	cp 1
	jr z,PutScreen_NewLine
	B_CALL PutC
	jr PutScreen
PutScreen_NewLine:
	xor a
	ld (curcol),a
	ld a,(currow)
	inc a
	ld (currow),a
	jr PutScreen
Menu_Enter:
	ld hl,FunctionsMenu+1
	ld a,(cur_column)
	add a,a
	ld b,0
	ld c,a
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	inc hl
	;HL now points to start of menu data
	ld a,(curitem)
	inc a
	ld b,a
Menu_Enter_Search:
	push bc
	xor a
	ld bc,-1
	cpir
	pop bc
	djnz Menu_Enter_Search
	ld a,(hl)
	ret
Menu_Left:
	ld a,(cur_column)
	or a
	jr z,Menu_Left_Right
	dec a
	ld (cur_column),a
	jr Menu_Right_Go
Menu_Left_Right:
	ld a,(endcolumn)
	ld (cur_column),a
	jr Menu_Right_Go
Menu_Right:
	ld a,(endcolumn)
	ld b,a
	ld a,(cur_column)
	cp b
	jr z,Menu_Right_Left
	inc a
	ld (cur_column),a
Menu_Right_Go:
	jp Menu_1
Menu_Right_Left:
	xor a
	ld (cur_column),a
	jr Menu_Right_Go
Menu_Up:
	call Entries_MoveUp
	jp Menu_Loop_Refresh
Menu_Down:
	call Entries_MoveDown
	jp Menu_Loop_Refresh
Menu_Display:
	ld de,0
	ld (currow),de
	ld hl,FunctionsMenu
	ld a,(hl)
	add a,a
	inc a
	ld b,0
	ld c,a
	add hl,bc
	ld a,(FunctionsMenu)
	ld b,a
	ld c,0
Menu_Display_Top_Loop:
	push bc
	ld a,(cur_column)
	cp c
	call z,SetInverse
	call putstr
	call ResInverse
	ld a,20h
	B_CALL PutC
	pop bc
	inc c
	djnz Menu_Display_Top_Loop
	;Now display rows (1:2:3: et cetera)
	ld hl,FunctionsMenu+1
	ld a,(cur_column)
	add a,a
	ld b,0
	ld c,a
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld a,(hl)
	dec a
	ld (enditem),a
	inc hl
	set 0,(iy+asm_Flag1)
	res 1,(iy+asm_Flag1)		;if APPS menu...it's not!
	jp SuperMenu
SetInverse:
	set textInverse,(iy+textFlags)
	ret
ResInverse:
	res textInverse,(iy+textFlags)
	ret
Menu_SpecialKeys:
	DB 29
	DB kClear,kAppsMenu,kPrgm,kZoom,kDraw,kSPlot,kStat,kMath
	DB kTest,kChar,kVars,kMem,kMatrix,kDist,kAngle,kList,kCalc
	DB kCatalog,kgraph,kMode,kWindow,kYequ,kTable,kTblSet
	DB	kStatP,kFormat,kTrace,kQuit
FunctionsMenu:
	;Format:
	;Number of columns
	DB 3
	DW Menu_Prgm
	DW Menu_Math
	DW Menu_Misc
	DB "PRGM",0
	DB "MATH",0
	DB "MISC",0
Menu_Prgm:
	DB 3
Token_SpriteToken:
	DB "sprite(",0,20
Token_PlayToken:
	DB "play(",0,31
Token_ExecAsmToken:
	DB "ExecAsm(",0,33
Menu_Math:
	DB 7
Token_SubfactorialToken:
	DB "!n(",0,30
Token_BaseToken:
	DB "baseInput(",0,25
Token_ConstantToken:
	DB "const(",0,29
Token_FactorToken:
	DB "factor(",0,23
Token_GammaToken:
	DB "gamma(",0,24
Token_ModuloToken:
	DB "mod(",0,28
Token_RootToken:
	DB "simp",10h,"(",0,22
Menu_Misc:
	DB 4
Token_LinkGetToken:
	DB "linkGet(",0,26
Token_LinkSendToken:
	DB "linkSend(",0,27
Token_RestoreMemToken:
	DB "RestoreMem(",0,32
Token_RomanToken:
	DB "Rom",5,"Dec(",0,21
Hook_Key_TokenShow_2:
	ld hl,(edittail)
	ld a,0BBh
	cp (hl)
	jp nz,Hook_Key_TokenShow_Exit
	inc hl
	ld a,26h
	cp (hl)
	jp nz,Hook_Key_TokenShow_Exit
	jr Hook_Key_TokenShow_3
Hook_Key_TokenShow:
	ld a,(tokenHookPtr+3)
	or a
	jp nz,Hook_Key_TokenShow_No
	in a,(4)
	bit 3,a
	jp nz,Hook_Key_TokenShow_No
	ld a,(iy+appFlags)
	push af
	ld hl,appbackupscreen
	call SaveDisplay
	res appTextSave,(iy+appFlags)
	set textInverse,(iy+textFlags)
	ld hl,(editcursor)
	ld a,0BBh
	cp (hl)
	jr nz,Hook_Key_TokenShow_2
	inc hl
	ld a,026h
	cp (hl)
	jr nz,Hook_Key_TokenShow_2
Hook_Key_TokenShow_3:
	inc hl
	ld a,(hl)
	sub 30h
	ld b,a
	inc hl
	ld a,(hl)
	sub 30h
	ld c,a
	ld a,b
	add a,a
	add a,a
	add a,b
	add a,a
	add a,c
	cp last_token+1
	jr nc,Hook_Key_TokenShow_Exit
	ld d,0
	ld e,a
	sla e
	ld hl,Hook_Token_Table
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	ld de,(currow)
	push de
	ld de,7
	ld (currow),de
	call putstr
	res textInverse,(iy+textFlags)
	di
Hook_Key_TokenShow_Loop:
	ld a,0FFh
	out (1),a
	neg
	ld a,0EFh
	out (1),a
	neg
	in a,(1)
	cp 0FEh
	jr nz,Hook_Key_TokenShow_Restore
	in a,(4)
	bit 3,a
	jr z,Hook_Key_TokenShow_Loop
Hook_Key_TokenShow_Restore:
	ei
	ld hl,appbackupscreen
	call CopyLayer
	pop de
	ld (currow),de
Hook_Key_TokenShow_Exit:
	res textInverse,(iy+textFlags)
	pop af
	ld (iy+appFlags),a
	xor a
	ret
Hook_Key_TokenShow_No:
	ld a,k0
	or a
	ret
Hook_Key_Entry:
	ld a,(cxCurApp)
	cp 40h
	jp nz,Hook_Key_Entry_Keep
	ld a,(menucurrent)
	or a
	jp nz,Hook_Key_Entry_Keep
	call GetRealSettings
	bit 3,(hl)
	jp z,Hook_Key_Entry_Keep
	;Now check if Rcl mode active
	ld hl,textshadow+70h
	ld de,Hook_Key_Entry_Rcl
	ld b,4
	call CompareStrings
	jp z,Hook_Key_Entry_Keep
	;We are good...
	ld hl,appbackupscreen
	B_CALL SaveDisp
	res indicrun,(iy+indicflags)
	res apptextsave,(iy+appflags)
	res appautoscroll,(iy+appflags)
	B_CALL CursorOff
	ld hl,Entries_Program
	rst 20h
	B_CALL ChkFindSym
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld a,b
	or c
	jp z,Hook_Key_Entry_Cancel
	dec hl
	inc bc
	inc bc
	ld de,entrieslist
	push hl
	ld hl,01F4h
	or a
	sbc hl,bc
	pop hl
	jr nc,$F
	ld bc,01F4h
	ld a,0F4h
	ld (de),a
	inc de
	ld a,1
	ld (de),a
	inc de
	inc hl
	inc hl
$$:
	ldir
	ld a,(numLastEntries)
	inc a
	ld (numentries),a
	;Copy entries over
	ld a,(numLastEntries)
	or a
	jr z,Entries_SkipFirstCopy
	ld b,a
	ld hl,lastEntryStk
	;DE is where to copy
Entries_FirstCopy:
	push bc
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc bc
	inc bc
	dec hl
	ldir
	pop bc
	djnz Entries_FirstCopy
Entries_SkipFirstCopy:
	ld hl,(currow)
	ld (originalcur),hl
	res appTextSave,(iy+appFlags)
	B_CALL ClrLCDFull
	ld a,(numentries)
	ld b,a
	dec a
	ld (enditem),a
	xor a
	ld (curitem),a
	ld (menutop),a
	inc a
	ld (itemrow),a
Entries_Loop_Refresh:
	call Entries_DisplayMenu
Entries_Loop:
	set 7,(iy+28h)
	B_CALL GetKey
	res 7,(iy+28h)
	ld ix,Entries_Loop
	cp kOff
	jp z,Entries_PowerOff
	cp kDown
	jp z,Entries_MoveDown1
	cp kUp
	jp z,Entries_MoveUp1
	cp kEnter
	jr z,Entries_Enter
	cp kClear
	jp z,Entries_Exit
	cp 40h
	jp z,Entries_Exit
	;cp k0
	;jr z,Entries_0
	cp k0
	jr c,Entries_Loop
	cp k9+1
	jr nc,Entries_NotNumeric
	cp k0
	jr nz,$F
	ld a,k1+9
$$:
	sub k1
	ld b,a
	ld a,(enditem)
	cp b
	jr c,Entries_Loop
	ld a,b
	ld (curitem),a
	jr Entries_Enter
Entries_NotNumeric:
	cp 9Ah
	jr c,Entries_Loop
	cp 0B3h+1
	jr nc,Entries_Loop
	sub 9Ah-10
	ld b,a
	ld a,(enditem)
	cp b
	jr c,Entries_Loop
	ld a,b
	ld (curitem),a
	jr Entries_Enter
;Entries_0:
;	sub k0-9
;	ld (curitem),a
Entries_Enter:
	ld hl,appbackupscreen
	ld b,64
	B_CALL RestoreDisp
	ld de,(originalcur)
	ld (currow),de
Entries_Enter_MoveBack:
	ld hl,(editTop)
	ld de,(editCursor)
	or a
	sbc hl,de
	jp z,Entries_Enter_Ready
	B_CALL CursorLeft
	jr Entries_Enter_MoveBack
Entries_Enter_Ready:
	set apptextsave,(iy+appflags)
	set appautoscroll,(iy+appflags)
	ld hl,(editTop)
	ld (editCursor),hl
	ld hl,(editBtm)
	ld (editTail),hl
	;now insert
	ld a,(curitem)
	call GetEntry
	;HL is entry
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
Entries_Enter_Insert_Loop:
	push bc
	ld a,(hl)
	B_CALL IsA2ByteTok
	jr z,Entries_Enter_Insert_Two
	ld d,0
	ld e,(hl)
	push hl
	push de
	B_CALL BufReplace
	pop de
	B_CALL PutTokString
	pop hl
	inc hl
	pop bc
	dec bc
	ld a,b
	or c
	jr nz,Entries_Enter_Insert_Loop
	jr Entries_Enter_Display
Entries_Enter_Insert_Two:
	ld d,(hl)
	inc hl
	ld e,(hl)
	push hl
	push de
	B_CALL BufReplace
	pop de
	B_CALL PutTokString
	pop hl
	inc hl
	pop bc
	dec bc
	dec bc
	ld a,b
	or c
	jr nz,Entries_Enter_Insert_Loop
Entries_Enter_Display:
	res cmdVirgin,(iy+cmdFlags)
	res AnsScroll,(iy+ansFlags)
	B_CALL DispEOW
	jp Hook_Key_Entry_Cancel
Entries_PowerOff:
	call Entries_Exit
	B_CALL PowerOff
Entries_Exit:
	ld hl,appbackupscreen
	ld b,64
	B_CALL RestoreDisp
	ld de,(originalcur)
	ld (currow),de
	jp Hook_Key_Entry_Cancel
Entries_ClearCurrent:
	ld a,(itemrow)
	ld (currow),a
	xor a
	ld (curcol),a
	ld a,(curitem)
	add a,a
	ld e,a
	ld d,0
	ld hl,Entries_List
	add hl,de
	ld a,(hl)
	bit 1,(iy+asm_Flag1)		;APPS menu?
	jr z,$F
	ld a,(menutop)
	ld b,a
	ld a,(currow)
	add a,b
	cp 10+1
	ld a,(hl)
	jr c,$F
	ld a,' '
$$:
	B_CALL PutC
	inc hl
	ld a,(itemrow)
	cp 7
	jr nz,$F
	ld a,(curitem)
	ld b,a
	ld a,(enditem)
	cp b
	jr z,$F
	ld a,ldownarrow
	jr Entries_ClearCurrent_1
$$:
	ld a,(itemrow)
	dec a
	jr nz,$F
	ld a,(menutop)
	or a
	jr z,$F
	ld a,luparrow
	jr Entries_ClearCurrent_1
$$:
	ld a,(hl)
Entries_ClearCurrent_1:
	B_CALL PutC
	res textInverse,(iy+textFlags)
	ret
Entries_SetCurrent:
	set textInverse,(iy+textFlags)
	jr Entries_ClearCurrent
Entries_MoveUp1:
	ld hl,Entries_Loop_Refresh
	push hl
Entries_MoveUp:
	call Entries_ClearCurrent
Entries_MoveUp_Special:
	ld a,(curitem)
	or a
	jr z,Entries_MoveUp_Bottom
	dec a
	ld (curitem),a
	ld a,(itemrow)
	dec a
	ld (itemrow),a
	jr z,Entries_MoveUp_2
	bit 2,(iy+asm_Flag1)
	jr z,$F
	ret
$$:
	pop hl
	push ix
	jr Entries_SetCurrent
Entries_MoveUp_2:
	;We're at the top of the current screen
	ld hl,itemrow
	inc (hl)
	ld hl,menutop
	dec (hl)
	ret
Entries_MoveUp_Bottom:
	bit 2,(iy+asm_Flag1)
	ret nz
	ld a,(enditem)
	ld (curitem),a
	sub 6
	ld (menutop),a
	ld a,(enditem)
	ld e,7
	cp e
	jr nc,Entries_MoveUp_BottomSkip
	xor a
	ld (menutop),a
Entries_MoveUp_BottomSkip:
	ld a,e
	ld (itemrow),a
	ld a,(enditem)
	cp e
	ret nc
	inc a
	ld (itemrow),a
	ret
Entries_MoveDown1:
	ld hl,Entries_Loop_Refresh
	push hl
Entries_MoveDown:
	res 2,(iy+asm_Flag1)
	call Entries_ClearCurrent
Entries_MoveDown_Special:
	ld a,(enditem)
	ld b,a
	ld a,(curitem)
	cp b
	jr z,Entries_MoveDown_Top
	inc a
	ld (curitem),a
	ld a,(itemrow)
	inc a
	ld (itemrow),a
	cp 7+1
	jr nc,Entries_MoveDown_2
	bit 2,(iy+asm_Flag1)
	jr z,$F
	ret
$$:
	pop hl
	push ix
	jp Entries_SetCurrent
Entries_MoveDown_2:
	;We're at the bottom of the current screen
	dec a
	ld (itemrow),a
	ld hl,menutop
	inc (hl)
	ret
Entries_MoveDown_Top:
	bit 2,(iy+asm_Flag1)
	ret nz
	xor a
	ld (menutop),a
	ld (curitem),a
	inc a
	ld (itemrow),a
	ret
Entries_DisplayMenu:
	ld hl,0
	ld (currow),hl
	ld hl,Entries_Title
	set textInverse,(iy+textFlags)
	call putstr
	res textInverse,(iy+textFlags)
	res 0,(iy+asm_Flag1)			;whether we're using this for entries or Prgm+Prgm
	res 1,(iy+asm_Flag1)			;if for APPS menu
	ld a,(menutop)
	call GetEntry
SuperMenu:
	ld bc,0*256+1
	ld (currow),bc
	;HL points to first menu item
	push hl
	ld a,(menutop)
	ld b,a
	ld a,(enditem)
	inc a
	sub b
	ld b,a
	cp 7
	jr c,Entries_DisplayMenu_Ready
	ld b,7
Entries_DisplayMenu_Ready:
	ld hl,Entries_List
	ld a,(menutop)
	add a,a
	ld d,0
	ld e,a
	add hl,de
	ex de,hl
	pop hl
Entries_DisplayMenu_Loop:
	push bc
	push hl
	ex de,hl
	ld a,(itemrow)
	ld b,a
	ld a,(currow)
	cp b
	jr nz,Entries_DisplayMenu_NoHighlight
	set textInverse,(iy+textFlags)
Entries_DisplayMenu_NoHighlight:
	bit 1,(iy+asm_Flag1)
	ld a,(hl)
	jr z,$F
	ld a,(menutop)
	ld b,a
	ld a,(currow)
	add a,b
	cp 10+1
	ld a,(hl)
	jr c,$F
	ld a,' '
$$:
	B_CALL PutC
	inc hl
	ld c,':'
	ld a,(menutop)
	or a
	jr z,Entries_DisplayMenu_Loop_CheckBottom
	ld a,(currow)
	dec a
	jr nz,Entries_DisplayMenu_Loop_CheckBottom
	ld c,LupArrow
	jr Entries_DisplayMenu_Loop_ColonOK
Entries_DisplayMenu_Loop_CheckBottom:
	ld a,(currow)
	cp 7
	jr nz,Entries_DisplayMenu_Loop_ColonOK
	ld a,(enditem)
	ld b,a
	ld a,(menutop)
	add a,6
	cp b
	jr nc,Entries_DisplayMenu_Loop_ColonOK
	ld c,LdownArrow
Entries_DisplayMenu_Loop_ColonOK:
	ld a,c
	B_CALL PutC
	res textInverse,(iy+textFlags)
	inc hl
	ex de,hl
	pop hl
	push de
	bit 0,(iy+asm_Flag1)
	jr nz,Entries_DisplayMenu_Reuse
	bit 1,(iy+asm_Flag1)
	jr nz,Entries_DisplayMenu_APPS
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	call Entries_DisplayEntry
Entries_DisplayMenu_Reuse_Done:
	pop de
	xor a
	ld (curcol),a
	ld a,(currow)
	inc a
	ld (currow),a
	pop bc
	djnz Entries_DisplayMenu_Loop
	ld a,(currow)
Entries_DisplayMenu_ClearBottom:
	cp 8
	ret z
	ld (currow),a
	push af
	xor a
	ld (curcol),a
	B_CALL EraseEOL
	pop af
	inc a
	jr Entries_DisplayMenu_ClearBottom
	ret
Entries_DisplayMenu_Reuse:
	call putstr
	inc hl
	B_CALL EraseEOL
	jr Entries_DisplayMenu_Reuse_Done
Entries_APPS_Finance:
	DB "Finance",Lellipsis,0
Entries_DisplayMenu_APPS:
	ld a,(currow)
	dec a
	ld b,a
	ld a,(menutop)
	add a,b
	bit 3,(iy+asm_Flag1)
	jr z,$F
	dec a
	cp 0FFh
	jr nz,$F
	ld hl,Entries_APPS_Finance
	call putstr
	jr Entries_DisplayMenu_Reuse_Done
$$:
	ld c,a
	di
	ld a,2
	out (5),a
	ld hl,apppointers+4000h
 	sla c
	ld b,0
	add hl,bc
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	set 6,h
	ld bc,8
	ld de,OP3
	ldir
	xor a
	out (5),a
	ld b,8
	ld hl,OP3
Entries_DisplayMenu_APPS_Loop:
	ld a,(hl)
	or a
	jr nz,$F
	ld a,' '
$$:
	B_CALL PutC
	inc hl
	djnz Entries_DisplayMenu_APPS_Loop
	ld a,' '
	B_CALL PutC
	jp Entries_DisplayMenu_Reuse_Done
Entries_DisplayEntry:
	;Inputs - BC length, HL entry
	ld d,h
	ld e,l
	add hl,bc
	push hl
	ex de,hl
Entries_DisplayEntry_Loop:
	or a
	sbc hl,de
	jr z,Entries_DisplayEntry_Quit
	add hl,de
	push de
	push hl
	B_CALL Get_Tok_Strng
	ld hl,OP3
Entries_DisplayEntry_SubLoop:
	ld a,(hl)
	or a
	inc hl
	jr z,Entries_DisplayEntry_1
	B_CALL PutMap
	ld a,(curcol)
	inc a
	ld (curcol),a
	cp 16
	jr nz,Entries_DisplayEntry_SubLoop
	dec a
	ld (curcol),a
	ld a,Lellipsis
	B_CALL PutMap
	jr Entries_DisplayEntry_Quit_2
Entries_DisplayEntry_1:
	pop hl
	pop de
	ld a,(hl)
	inc hl
	B_CALL IsA2ByteTok
	jr nz,Entries_DisplayEntry_Loop
	inc hl
	jr Entries_DisplayEntry_Loop
Entries_DisplayEntry_Quit_2:
	pop hl
	pop de
	pop hl
	ret
Entries_DisplayEntry_Quit:
	pop hl
	B_CALL EraseEOL
	ret
GetEntry:
	ld b,a
	ld hl,entrieslist
	or a
	ret z
GetEntryLoop:
	push bc
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	add hl,bc
	pop bc
	djnz GetEntryLoop
	ret
Entries_Title:
	DB "ENTRIES",0
Entries_List:
	DB "1:2:3:4:5:6:7:8:9:0:A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z:"
	DB "a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:"
Entries_Program:
	DB 5,"#",0
Hook_Key_Entry_Rcl:
	DB "Rcl "
Hook_Key_Entry_Cancel:
	set apptextsave,(iy+appflags)
	set appautoscroll,(iy+appflags)
	B_CALL CursorOn
	xor a
	ret
Hook_Key_Entry_Keep:
	ld a,0Dh
	or a
	ret
SymbolicApp:
	DB AppObj,"Symbolic"
Hook_Key_Math:
	ld a,(menucurrent)
	cp 2Fh
	jr z,Hook_Key_Math_OK
	cp 08h
	jr z,Hook_Key_Math_OK
Hook_Key_Math_Exit_Good:
	ld a,kMath
	or a
	ret
Hook_Key_Math_OK:
	ld hl,SymbolicApp
	rst 20h
	B_CALL FindApp
	jr c,Hook_Key_Math_Exit_Good
	;Symbolic exists, lets go
	push af
	ld hl,SymbolicMenu_Loader
	ld de,appbackupscreen
	ld bc,SymbolicMenu_Loader_End-SymbolicMenu_Loader
	ldir
	pop bc
	call appbackupscreen
	;B contains return code
	ld a,b
	cp 0FFh
	jr z,SymbolicMenu_Good
	or a
	ret
SymbolicMenu_Good:
	ld a,e
	sub 0CFh
	jp Symbolic_JumpPoint
SymbolicMenu_Loader:
	in a,(6)
	push af
	ld a,b
	out (6),a
	ld hl,(40ACh)
	ld a,h
	or l
	jr z,SymbolicMenu_Loader_Point
	ld de,SymbolicMenu_Loader_Point-SymbolicMenu_Loader+appbackupscreen
	push de
	jp (hl)
SymbolicMenu_Loader_Point:
	ld b,a
	pop af
	out (6),a
	ret
SymbolicMenu_Loader_End:
Hook_Key_Vars:
	ld a,(menucurrent)
	cp 0Bh
	jr z,Hook_Key_Vars_OK
	ld a,kVars
	or a
	ret
Hook_Key_Vars_OK:
	res indicrun,(iy+indicflags)
	res apptextsave,(iy+appflags)
	B_CALL ClrLCDFull
	xor a
	ld (cur_row),a
Custom_Main_Loop_Refresh:
	call Custom_Display
Custom_Main_Loop:
	set 7,(iy+28h)
	B_CALL GetKey
	res 7,(iy+28h)
	cp kOff
	jr z,CustomMenu_PowerOff
	cp kAppsMenu
	jr z,CustomMenu_AppsMenu
	cp kEnter
	jr z,CustomMenu_Enter
	cp kDown
	jp z,CustomMenu_MoveDown
	cp kUp
	jp z,CustomMenu_MoveUp
	cp kDel
	jr z,CustomMenu_Clear
	cp k1
	call nc,CustomMenu_Numeric
	ld hl,Menu_SpecialKeys
	ld c,(hl)
	inc hl
	ld b,0
	cpir
	jr nz,Custom_Main_Loop
	or a
	ret
CustomMenu_PowerOff:
	B_CALL PowerOff
CustomMenu_AppsMenu:
	ld hl,Hook_Key_Apps_ReturnPoint
	push hl
	jp Hook_Key_Apps
CustomMenu_Clear:
	call Custom_Lookup
	ld a,b
	or a
	jr z,CustomMenu_Clear_OK
	call EditBuffer1
	B_CALL Arc_Unarc
	call EditBuffer2
	call Custom_Lookup
CustomMenu_Clear_OK:
	ex de,hl
	inc hl
	inc hl
	ld a,(cur_row)
	add a,a
	ld d,0
	ld e,a
	add hl,de
	ld (hl),0
	inc hl
	ld (hl),0
	jp Hook_Key_Vars_OK
CustomMenu_Numeric:
	cp k8+1
	ret nc
	sub k1
	ld (cur_row),a
CustomMenu_Enter:
	ld a,1
	ld (tokenHookPtr+3),a
	call Custom_Lookup
	ld a,b
	or a
	jr z,CustomMenu_Enter_Go
	ex de,hl
	ld bc,768
	ld de,savesscreen
	B_CALL FlashToRam
	ld de,savesscreen+16
CustomMenu_Enter_Go:
	ex de,hl
	inc hl
	inc hl
	;HL is start of data
	ld a,(cur_row)
	add a,a
	ld d,0
	ld e,a
	add hl,de
	ld a,(hl)
	or a
	jr z,CustomMenu_Enter_Empty
	ld (ramCode),a
	inc hl
	ld a,(hl)
	ld (ramCode+1),a
	ld a,sk5
	ld (kbdscancode),a
	ld a,kClear
	or a
	res cmdVirgin,(iy+cmdFlags)
	ret
CustomMenu_Enter_Empty:
	xor a
	ld (tokenHookPtr+3),a
	ld a,kClear
	or a
	ret
CustomMenu_MoveUp:
	call Custom_Display_CalculatePrefix
	call Custom_Display_ShowPrefix
	ld a,(cur_row)
	dec a
CustomMenu_Move_Jump:
	and 7
	ld (cur_row),a
	set textInverse,(iy+textFlags)
	call Custom_Display_CalculatePrefix
	call Custom_Display_ShowPrefix
	jp Custom_Main_Loop
CustomMenu_MoveDown:
	call Custom_Display_CalculatePrefix
	call Custom_Display_ShowPrefix
	ld a,(cur_row)
	inc a
	jr CustomMenu_Move_Jump
Custom_Display_Loop_None:
	inc hl
	inc hl
	jr Custom_Display_Loop_JumpIn
Custom_Display_CalculatePrefix:
	ld a,(cur_row)
	ld hl,Entries_List
	ld d,0
	add a,a
	ld e,a
	add hl,de
	ret
Custom_Display:
	ld hl,0
	ld (currow),hl
	call Custom_Lookup
	call c,CreateCustomAppVar
	ld a,b
	or a
	jp z,CustomAppVarGood
	ex de,hl
	ld bc,768
	ld de,savesscreen
	B_CALL FlashToRam
	ld de,savesscreen+16
CustomAppVarGood:
	ex de,hl
	inc hl
	inc hl
	ld b,8
	;Now display menu
	ld de,Entries_List
Custom_Display_Loop:
	push bc
	push hl
	ex de,hl
	ld a,(cur_row)
	ld b,a
	ld a,(currow)
	cp b
	jr nz,Custom_Display_SkipInverse
	set textInverse,(iy+textFlags)
Custom_Display_SkipInverse:
	call Custom_Display_ShowPrefix2
	inc hl
	ex de,hl
	pop hl
	ld a,(hl)
	or a
	jr z,Custom_Display_Loop_None
	push de
	ld d,0
	ld e,a
	cp 0FCh
	jr c,Custom_Display_Loop_One
	ld d,e
	inc hl
	ld e,(hl)
	dec hl
Custom_Display_Loop_One:
	inc hl
	inc hl
	push hl
	B_CALL KeyToString
	B_CALL PutPSB
	pop hl
	pop de
Custom_Display_Loop_JumpIn:
	pop bc
	xor a
	ld (curcol),a
	ld a,(currow)
	inc a
	ld (currow),a
	djnz Custom_Display_Loop
	ret
Hook_Key_Custom_Second:
	ld a,(tokenHookPtr+3)
	dec a
	jr nz,Custom_Key_Second_No
	ld (tokenHookPtr+3),a
	ld a,(ramCode)
	cp 0FCh
	jr nc,Custom_Key_Second_Two
	pop bc
	or a
	ret
Custom_Key_Second_Two:
	ld a,(ramCode+1)
	ld (keyExtend),a
	ld a,(ramCode)
	cp 0FFh
	call z,Custom_Key_Second_Dec
	cp 0FDh
	call z,Custom_Key_Second_Dec
	pop bc
	or a
	ret
Custom_Key_Second_Dec:
	dec a
	ret
Custom_Key_Second_No:
	ld a,k5
	or a
	ret
Custom_Display_ShowPrefix:
	xor a
	ld (curcol),a
	ld a,(cur_row)
	ld (currow),a
Custom_Display_ShowPrefix2:
	ld a,(hl)
	B_CALL PutC
	inc hl
	ld a,(hl)
	B_CALL PutC
	res textInverse,(iy+textFlags)
	ret
Custom_Lookup:
	ld hl,CatalogAppVar
	rst 20h
	B_CALL ChkFindSym
	ret
EditBuffer1:
	ld de,(iMathPtr1)
	push de
	B_CALL CloseEditEqu
	pop hl
	ex (sp),hl
	jp (hl)
EditBuffer2:
	pop hl
	ex (sp),hl
	ex de,hl
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
	jr nc,$F
	ex de,hl
$$:
	ld (editBtm),hl
	ld (editTail),hl
	ret
CreateCustomAppVar:
	call EditBuffer1
	ld hl,16
	B_CALL CreateAppVar
	call EditBuffer2
	jp Custom_Lookup
Hook_Key_Base:
	in a,(4)
	bit 3,a
	jp nz,Hook_Key_Base_Ret
	ld a,(menucurrent)
	or a
	jp nz,Hook_Key_Base_Ret
	ld a,(cxcurapp)
	cp 40h
	jp nz,Hook_Key_Base_Ret
	call GetRealSettings
	bit 6,(hl)
	jp z,Hook_Key_Base_Ret
	ld hl,Appvar
	rst 20h
	B_CALL ChkFindSym
	ld a,b
	or a
	jr z,$F
	call EditBuffer1
	B_CALL Arc_Unarc
	call EditBuffer2
$$:
	ld hl,savesscreen
	call SaveDisplay
	ei
	ld a,(iy+appFlags)
	push af
	res appTextSave,(iy+appFlags)
	ld hl,(currow)
	push hl
	ld hl,6
	ld (currow),hl
	ld hl,Hook_Key_Base_Text
	call putstr
	;Get two digit base
	ld bc,2*256+0
Hook_Key_Base_Loop:
	add a,a
	ld c,a
	add a,a
	add a,a
	add a,c
	ld c,a
Hook_Key_Base_Loop2:
	B_CALL GetCSC
	or a
	jr z,Hook_Key_Base_Loop2
	ld hl,Hook_Key_Base_Table
	push bc
	ld bc,10
	cpir
	pop bc
	jr nz,Hook_Key_Base_SpeedyExit
	ld de,Hook_Key_Base_Table
	scf
	sbc hl,de
	ld a,l
	push af
	push bc
	add a,'0'
	B_CALL PutC
	pop bc
	pop af
	add a,c
	djnz Hook_Key_Base_Loop
	cp 37
	jr nc,Hook_Key_Base_SpeedyExit
	cp 2
	jr c,Hook_Key_Base_SpeedyExit
	push af
	call GetRealSettings_Unarchive
	inc hl
	inc hl
	pop af
	ld (hl),a
	call FormA
	set textInverse,(iy+textFlags)
	ld hl,14*256
	ld (currow),hl
	ld hl,OP3
	call putstr
	ld b,120
	call Pause
	ld a,(tempkey)
	ld (kbdscancode),a
	res textInverse,(iy+textFlags)
Hook_Key_Base_SpeedyExit:
	pop hl
	ld (currow),hl
	pop af
	ld (iy+appFlags),a
	ld hl,savesscreen
	call CopyLayer
	ei
Hook_Key_Base_RetNo:
	xor a
	ret
Hook_Key_Base_Table:
	DB sk0,sk1,sk2,sk3,sk4,sk5,sk6,sk7,sk8,sk9
	DB 0,1,2,3,4,5,6,7,8,9
Hook_Key_Base_Ret:
	ld a,kLog
	or a
	ret
Hook_Key_Base_Text:
	DB "ENTER BASE      "
	DB "(02-36): ",0
