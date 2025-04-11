; Omnicalc v1.26.1
; (C) 2002-2005 Michael Vincent.
;  Modified in 2025 by Benjamin Allred (to remap up arrow key to [Entry])
;
;	This program is free software; you can redistribute it and/or modify
;	it under the terms of the Omnicalc license included with this source code.
;
;	This program is distributed in the hope that it will be useful,
;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;

; Omnicalc v1.2.1
; Michael Vincent - michael@detachedsolutions.com
;
; Copyright (C) 2001-2005, by Michael Vincent. All rights reserved.
; Developed for Detached Solutions - www.detachedsolutions.com
; Project started 6-06-01.
; Rewrite started 12-21-01.
; Fork started 2025-04-01
;
 NOLIST
 INCLUDE "ti83plus.inc"
 INCLUDE "michael.inc"
 LIST
 INCLUDE "equates.inc"
 GLOBALS ON
 INCLUDE "header.inc"
TotalAppStart:
	ld hl,Appvar
	rst 20h
	B_CALL ChkFindSym
	jr nc,StartApp
	ld hl,14
	B_CALL CreateAppVar
	ex de,hl
	inc hl
	inc hl
	ld b,14
CreateAppVar_Loop:
	ld (hl),0
	inc hl
	djnz CreateAppVar_Loop
	jr StartApp2
StartApp:
StartApp2:
	ld hl,CatalogAppVar
	rst 20h
	B_CALL ChkFindSym
	jr nc,StartApp3
	ld hl,16
	B_CALL CreateAppVar
	ld b,16
	ex de,hl
	inc hl
	inc hl
StartApp_Clear:
	ld (hl),0
	inc hl
	djnz StartApp_Clear
StartApp3:
	call GetRealSettings
	bit 0,(hl)
	call nz,InstallHooks2
StartApp_Install:
	B_CALL ClrLCDFull
	ld hl,Splash_Title
	ld de,appbackupscreen+2
	ld bc,200
	call RLE_Decompress
	ld hl,80*256+20
	ld (appbackupscreen),hl
	ld hl,appbackupscreen
	ld de,1*256+7
	B_CALL DisplayImage
	ld hl,Splash_DS
	ld de,appbackupscreen
	ld bc,57
	ldir
	ld hl,appbackupscreen
	ld de,49*256+5
	B_CALL DisplayImage
	ld hl,57*256+9
	ld (pencol),hl
	ld hl,Intro_Web
	call vputstr
	ld hl,30*256+29
	ld (pencol),hl
	ld hl,Intro_Version
	call vputstr
	xor a
	ld (counter),a
Splash_Loop1:
	ei
	halt
	halt
	B_CALL GetCSC
	or a
	jr nz,MainMenu
	ld a,(counter)
	inc a
	ld (counter),a
	jr nz,Splash_Loop1
	xor a
	ld (counter),a
	ld (counter2),a
Splash_Loop:
	ei
	halt
	B_CALL GetCSC
	or a
	jr nz,MainMenu
	ld a,(counter)
	add a,2
	ld (counter),a
	or a
	jr nz,Splash_Loop
	ld (counter),a
	ld hl,30*256
	ld de,37*256+95
	B_CALL ClearRect
	ld a,(counter2)
	ld b,a
	ld hl,Splash_Moving_Strings
	call getString
	ld a,30
	ld (penrow),a
	call Vputs_Centered
	ld a,(counter2)
	inc a
	ld (counter2),a
	cp 10
	jr nz,Splash_Loop
	ld b,200
	call Pause
MainMenu:
	res bufferonly,(iy+plotflag3)
	call GetRealSettings
	bit 0,(hl)
	call nz,InstallHooks2
MainMenu_Point:
	B_CALL ClrScrnFull
	B_CALL GrBufClr
	ld hl,0
	ld de,7*256+95
	B_CALL FillRect
	set textInverse,(iy+textFlags)
	ld hl,30
	ld (pencol),hl
	ld hl,Title_MainMenu
	call vputstr
	xor a
	ld (currentmenu),a
	res textInverse,(iy+textFlags)
	ld hl,57*256
	ld (pencol),hl
	ld hl,MainScreenInstalled
	call vputstr
	call GetRealSettings
	ld a,(hl)
	ld hl,YesNo
	and 1
	add a,a
	add a,a
	ld d,0
	ld e,a
	add hl,de
	call vputstr
	call Main_DisplayMenu
	call MainMenu_Select
MainMenuLoop:
	B_CALL GetKey
	cp 3
	call z,MenuMoveUp
	cp 4
	call z,MenuMoveDown
	cp 5
	jr z,MainMenuSelected
	cp 9
	jr z,exitapp
	cp 40h
	jr z,exitapp
	cp 8Fh
	jp z,InstallRemove
	cp 90h
	jp z,Adjust
	cp 91h
	jp z,FontSets
	cp 92h
	jr nz,MainMenuLoop
exitapp:
	B_CALL SetTblGraphDraw
	B_JUMP JForceCmdNoChar
MainMenuSelected:
	ld a,(currentmenu)
	or a
	jr z,InstallRemove
	dec a
	jp z,Adjust
	dec a
	jp z,FontSets
	dec a
	jr exitapp
InstallRemove:
	B_CALL GrBufClr
	B_CALL ClrScrnFull
	B_CALL GetBaseVer
	ld h,a
	ld l,b
	ld de,010Dh
	or a
	sbc hl,de
	jr c,OldBaseVersion
	call GetRealSettings
	bit 0,(hl)
	jp nz,Settings_Uninstall
	call DisplayMessageBox
	bit GetKeyHook,(iy+Hooks1)
	jp nz,Install_Hooks_Already
	bit TokenHook,(iy+Hooks2)
	jr nz,Install_Hooks_Already
	bit ParserHook,(iy+Hooks3)
	jr nz,Install_Hooks_Already
	bit cxMainHook,(iy+Hooks1)
	jr nz,Install_Hooks_Already
	bit GetcscHook,(iy+Hooks1)
	jr nz,Install_Hooks_Already
	bit FontHook,(iy+Hooks2)
	jr nz,Install_Hooks_Already
	;bit CharacterHook,(iy+Hooks2)
	;jr nz,Install_Hooks_Already
	ld hl,14*256+17
	ld (pencol),hl
	ld hl,Settings_Installed
	call vputstr
	ld de,30*256+27
	ld (pencol),de
	call vputstr
	call InstallHooks
	call GetRealSettings_Unarchive
	set 0,(hl)
	res onInterrupt,(iy+onFlags)
	B_CALL GetKey
	cp 40h
	jp z,exitapp
	jp MainMenu
OldBaseVersion:
	call DisplayMessageBox
	ld hl,14*256+17
	ld (pencol),hl
	ld hl,OldBaseVersion_Text
	call vputstr
	ld de,20*256+17
	ld (pencol),de
	call vputstr
	ld de,30*256+27
	ld (pencol),de
	call vputstr
	B_CALL GetKey
	jp exitapp
Install_Hooks_Already:
	call DrawYesNo
	ld hl,14*256+17
	ld (pencol),hl	
	ld hl,Settings_Already
	call vputstr
	ld de,21*256+17
	ld (pencol),de
	call vputstr
	ld de,57*256+4
	ld (pencol),de
	call vputstr
	ld a,25
	ld (pencol),a
	call vputstr
Settings_Overwriteloop:
	B_CALL GetKey
	cp 40h
	jp z,exitapp
	cp 48h
	jp z,MainMenu
	cp 49h
	jr nz,Settings_Overwriteloop
	call DisableHooks
	call GetRealSettings_Unarchive
	res 0,(hl)
	jp InstallRemove
DrawYesNo:
	ld bc,0
	ld de,5
	B_CALL DarkLine
	ld bc,19*256
	ld de,19*256+5
	B_CALL DarkLine
	ld bc,38*256
	ld de,38*256+5
	B_CALL DarkLine
	ld bc,2*256+7
	ld de,17*256+7
	B_CALL DarkLine
	ld bc,21*256+7
	ld de,36*256+7
	B_CALL DarkLine
	ld d,2
	ld bc,1*256+6
	B_CALL ipoint
	ld bc,18*256+6
	B_CALL ipoint
	ld bc,20*256+6
	B_CALL ipoint
	ld bc,37*256+6
	B_CALL ipoint
	ret
Settings_Uninstall:
	call DisableHooks
	call DisplayMessageBox
	ld hl,14*256+17
	ld (pencol),hl
	ld hl,Settings_Uninstalled
	call vputstr
	ld de,21*256+17
	ld (pencol),de
	call vputstr
	ld de,30*256+27
	ld (pencol),de
	call vputstr
	call GetRealSettings_Unarchive
	res 0,(hl)
MessageBox_Pause:
	res onInterrupt,(iy+onFlags)
	B_CALL GetKey
	cp 40h
	jp z,exitapp
	jp MainMenu
Settings_Uninstalled:
	DB "Omnicalc",0
	DB "uninstalled",0
	DB "Press any key",0
Settings_Installed:
	DB "Omnicalc  installed",0
	DB "Press any key",0
Settings_Already:
	DB "Hooks already exist",0
	DB "Overwrite?",0
	DB "YES",0
	DB "NO",0
OldBaseVersion_Text:
	DB "ROM 1.13 or higher",0
	DB "is required.",0
	DB "Press any key",0
MainScreenInstalled:
	DB "Installed?",6,0
YesNo:
	DB "No",0,0
	DB "Yes",0
DisableHooks:
	res GetcscHook,(iy+Hooks1)		;RawKeyHook
	res ParserHook,(iy+Hooks3)		;Parser hook
	res cxMainHook,(iy+Hooks1)		;cxMain hook (aka Homescreen hook)
	res GetKeyHook,(iy+Hooks1)		;GetKeyHook
	res FontHook,(iy+Hooks2)		;Font hook
	res TokenHook,(iy+Hooks2)		;Token hook
	;res CharacterHook,(iy+Hooks2)	;Character hook
	ret
DisplayMessageBox:
	ld bc,16*256+50
	ld de,80*256+50
	B_CALL DarkLine
	ld bc,15*256+25
	ld de,81*256+25
	B_CALL DarkLine
	ld bc,16*256+24
	ld de,82*256+24
	B_CALL DarkLine
	ld de,81*256+49
	ld bc,81*256+25
	B_CALL DarkLine
	inc b
	inc d
	B_CALL DarkLine
	ld bc,15*256+49
	ld de,15*256+26
	B_CALL DarkLine
	ret
GetRealSettings_Routine:
	ld hl,Appvar
	rst 20h
	B_CALL ChkFindSym ; find the app var with 20h?
	ld a,b
	ret
GetRealSettings_Unarchive:
	call GetRealSettings_Routine
	jr c,GetRealSettings_NoExist
	or a
	jr z,GetRealSettings_Point
	B_CALL arc_unarc
GetRealSettings:
	call GetRealSettings_Routine
	jr c,GetRealSettings_NoExist
	or a
	jr nz,GetRealSettings_Archived
GetRealSettings_Point:
	ex de,hl
	inc hl
	inc hl
	ret
GetRealSettings_NoExist:
	ld hl,14
	B_CALL CreateAppVar
	ex de,hl
	inc hl
	inc hl
	ld b,14
GetRealSettings_NoExistLoop:
	ld (hl),0
	inc hl
	djnz GetRealSettings_NoExistLoop
	ret
GetRealSettings_Archived:
	ex de,hl
	ld bc,20
	add hl,bc
	bit 7,h
	jr z,$F
	res 7,h
	set 6,h
$$:
	ld de,tempSwapArea
	push de
	ld bc,14
	B_CALL FlashToRam
	pop hl
	ret
InstallHooks:
	;set our non appvar byte
	xor a
	ld (rawKeyHookPtr+3),a
	;and our recursive parser thing
	ld (parserHookPtr+3),a
	;and our temporary base thing
	ld (getKeyHookPtr+3),a
InstallHooks2:
	xor a
	ld (tokenHookPtr+3),a
	;First is _getkey hook.
	ld hl,Hook_Key
	in a,(6)
	B_CALL SetGetKeyHook
	;Parser hook
	ld hl,Hook_Parser
	in a,(6)
	B_CALL SetParserHook
	;Getcsc hook
	ld hl,csc_Hook
	in a,(6)
	B_CALL SetGetcscHook
	;cxMain hook
	ld hl,cxMain_Hook
	in a,(6)
	B_CALL SetcxMainHook
	;Character hook
	;ld hl,Character_Hook
	;in a,(6)
	;B_CALL SetCharacterHook
;InstallHooks_SkipcxMain:
	pop hl
	push hl
	ld de,StartApp_Install
	or a
	sbc hl,de
	jr z,$F
	add hl,de
	ld de,MainMenu_Point
	or a
	sbc hl,de
	jr nz,InstallHooks_Unarchive
$$:
	call GetRealSettings
InstallHooks_Point:
	bit 7,(hl)
	jr z,InstallHooks_NoLower
	set lwrCaseActive,(iy+appLwrCaseFlag)
	jr InstallHooks_SkipLowercase
InstallHooks_NoLower:
	res lwrCaseActive,(iy+appLwrCaseFlag)
InstallHooks_SkipLowercase:
	inc hl
	inc hl
	ld (hl),10
	inc hl
	bit 0,(hl)
	ret z
	;Font hook
	ld hl,Font_Hook
	in a,(6)
	B_CALL SetFontHook
	ret
InstallHooks_Unarchive:
	call GetRealSettings_Unarchive
	jr InstallHooks_Point
Main_DisplayMenu:
	ld hl,Menu_Main
	ld b,4
	ld de,10*256
	ld (pencol),de
DisplayMenuLoop:
	push bc
	call vputstr
	xor a
	ld (pencol),a
	ld a,(penrow)
	add a,8
	ld (penrow),a
	pop bc
	djnz DisplayMenuLoop
	ret
MainMenu_Select:
	set textInverse,(iy+textFlags)
	ld h,1
	jr $F
MainMenu_Unselect:
	ld h,0
$$:
	push hl
	xor a
	ld (pencol),a
	ld a,(currentmenu)
	add a,a
	add a,a
	add a,a
	add a,10
	ld (penrow),a
	ld a,' '
	B_CALL VPutMap
	ld a,(currentmenu)
	add a,'0'+1
	B_CALL VPutMap
	ld a,':'
	B_CALL VPutMap
	res textInverse,(iy+textFlags)
	pop hl
MainMenu_Line:
	ld a,(penrow)
	add a,6
	ld b,a
	ld a,63
	sub b
	ld c,a
	ld e,a
	ld b,0
	ld d,6
	B_CALL ILine
	ret
MenuMoveDown:
	ld a,(currentmenu)
	cp 3
	ret z
	call MainMenu_Unselect
	ld a,(currentmenu)
	inc a
Menu_Intercalary:
	ld (currentmenu),a
	jr MainMenu_Select
MenuMoveUp:
	ld a,(currentmenu)
	or a
	ret z
	call MainMenu_Unselect
	ld a,(currentmenu)
	dec a
	jr Menu_Intercalary
Title_MainMenu:
	DB "MAIN  MENU",0
Menu_Main:
	DB " 1:",6,"Install/Uninstall",0
	DB " 2:",6,"General  Settings",0
	DB " 3:",6,"Font  Sets",0
	DB " 4:",6,"Quit",0
Intro_Web:
	DB "detachedsolutions.com",0
Appvar:
	DB AppVarObj,"Omnicalc"
CatalogAppVar:
	DB AppVarObj,"custom",0
Splash_Title:
	DB 91h,00h,08h,0Ch,91h,00h,09h,1Ch,91h,00
	DB 09h,1Ch,00h,07h,0E0h,91h,00h,06h,0Ch,00
	DB 1Fh,0F0h,91h,00h,03h,06h,00h,00h,0Ch,00
	DB 3Ch,78h,91h,00h,03h,06h,00h,00h,0Ch,00
	DB 70h,1Ch,91h,00h,06h,0Ch,00h,60h,1Ch,40
	DB 0h,30h,0Eh,00h,3Eh,0Ch,00h,0C0h,0Ch,0FE
	DB 7Ch,77h,0Eh,0Fh,0BFh,0Ch,1Fh,0C0h,0Ch,0FF
	DB 0FEh,7Fh,8Eh,3Fh,83h,0Ch,7Fh,0C0h,0Ch,0E3
	DB 8Eh,71h,0CEh,70h,03h,0Ch,60h,0C0h,0Ch,0C3
	DB 06h,61h,0CCh,60h,03h,8Ch,0C0h,0C0h,1Ch,0C3
	DB 06h,60h,0CCh,40h,1Fh,8Ch,0C0h,0C0h,18h,0C3
	DB 06h,60h,0CCh,40h,3Bh,0Ch,0C0h,0E0h,38h,0C3
	DB 0Eh,61h,8Ch,60h,63h,0Ch,0C0h,0F0h,70h,0C3
	DB 0Ch,61h,8Eh,70h,67h,1Ch,0E0h,7Fh,0E0h,0C3
	DB 1Ch,61h,8Eh,3Fh,0FFh,1Ch,7Fh,1Fh,80h,0C3
	DB 1Ch,63h,8Eh,1Fh,0BBh,9Ch,3Fh,91h,00h,03
	DB 1Ch,03h,80h,00h,03h,80h,91h,00h,08h,03
	DB 00h,00h
Splash_DS:
	DB 5,86
	DB 231,188,199,107,220,28,204,107,222,103,28
	DB 214,25,172,107,26,49,172,105,140,214,176
	DB 215,25,236,123,154,25,172,105,140,214,152
	DB 214,25,172,107,26,13,172,105,140,214,140
	DB 231,153,167,107,220,56,207,49,158,102,184
putstr:
	ld a,(hl)
	inc hl
	or a
	ret z
	B_CALL PutC
	jr putstr
vputstr:
	push af
	push de
	push ix
vputstr10:
	ld a,(hl)
	inc hl
	or a
	jr z,vputstr20
	B_CALL vputmap
	jr nc,vputstr10
vputstr20:
	pop ix
	pop de
	pop af
	ret
Pause:
	push af
Pause_Loop:
	B_CALL GetCSC
	ld (tempkey),a
	or a
	jr nz,Pause_Exit
	ei
	halt
	djnz Pause_Loop
Pause_Exit:
	pop af
	ret
getString:
	ld a,b
	or a
	ret z
getStringLoop:
	push bc
	xor a
	ld bc,-1
	cpir
	pop bc
	djnz getStringLoop
	ret
Vputs_Centered:
	ld c,0
	push hl
Vputs_Centered_Loop:
	push hl
	ld a,(hl)
	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	push bc
	B_CALL SFont_Len
	ld l,b
	pop bc
	ld a,c
	add a,l
	ld c,a
	pop hl
	inc hl
	ld a,(hl)
	or a
	jr nz,Vputs_Centered_Loop
	pop hl
	ld a,48
	sra c
	sub c
	ld (pencol),a
	ld de,(pencol)
	push de	;save cursor location
	push hl
	;HL now is start of string, and ready to display!
	call Display_EvenOffset_String
	ld b,35
	call Pause
	pop hl
	pop de
	ld (pencol),de
	push hl
	ld a,(hl)
	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	B_CALL SFont_Len
	ld a,(pencol)
	add a,b
	ld (pencol),a
	pop hl
	inc hl
Display_EvenOffset_String:
	ld a,(hl)
	or a
	ret z
	B_CALL VPutMap
	inc hl
	ld a,(hl)
	or a
	ret z
	push hl
	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	B_CALL SFont_Len
	ld a,(pencol)
	add a,b
	ld (pencol),a
	pop hl
	inc hl
	jr Display_EvenOffset_String
Splash_Moving_Strings:
	;10 strings
	DB "Programmed  By:",0
	DB "Michael  Vincent",0
	DB "E-mail:",0
	DB "me@michaelv.org",0
	DB "  ",0
	DB "Special  Thanks:",0
	DB "Kirk  Meyer",0
	DB "Dan  Englender",0
	DB "Brandon  Sterner",0
	DB "Jason  Malinowski",0
	INCLUDE "settings.asm"
	INCLUDE "hooks.asm"
	INCLUDE "compression.asm"
Intro_Version:
	DB "Version 1.26.1",0
CompareStrings:
	;HL, DE strings for B bytes
	;There is an undocumented B_CALL but this is ~2000 cycles faster
	ld a,(de)
	cp (hl)
	ret nz
	inc hl
	inc de
	djnz CompareStrings
	xor a
	ret
	;HL is a RLE compressed image
	;BC is its length
RLE_Loop_Inc:
	inc hl
RLE_Decompress:
RLE_Loop:
	ld a,(hl)
	cp 91h
	jr z,RLE_Display_Run
	ldi
	ret po
	jr RLE_Loop
RLE_Display_Run:
	inc hl
	inc hl
	ld a,(hl)
	dec hl
RLE_Loop2:
	ldi
	dec a
	jr z,RLE_Loop_Inc
	dec hl
	jr RLE_Loop2
