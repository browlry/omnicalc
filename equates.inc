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
last_token EQU 33

NoTable_Entries EQU 15
NoTable2_Entries EQU 18

settingstemp EQU appbackupscreen	;byte
currentI EQU appbackupscreen
currentCo EQU appbackupscreen+2
currentNum EQU appbackupscreen+4
currentNum2 EQU appbackupscreen
factorsCount EQU appbackupscreen+20
primeptr EQU appbackupscreen+22
factorsList EQU appbackupscreen+24
tempop1 EQU appbackupscreen+30
currentmenu EQU appbackupscreen
counter EQU savesscreen+2
counter2 EQU savesscreen+4
selection EQU savesscreen+6
keypress EQU savesscreen+7
NewKeyPress EQU savesscreen+8
KeyValid EQU savesscreen+9
FontChar EQU savesscreen+10
hlsave EQU savesscreen+11
AdjustCurrent EQU savesscreen+12
offset EQU savesscreen+14	;4 bytes
temp EQU savesscreen+18
settingbyte EQU savesscreen+20
settingdisp EQU savesscreen+22
settingplace EQU savesscreen+23
oldhl EQU savesscreen+25
settingnum EQU savesscreen+27
string1 EQU 9872h
string2 EQU 9882h
value EQU 9892h
conv1 EQU 98A2h

;Token menu equates
cur_column EQU appbackupscreen
endcolumn EQU cur_column+2
cur_row EQU endcolumn+2

;Parsing equates for sprite
spriteheight EQU appbackupscreen	;byte
spritewidth EQU spriteheight+1	;byte
spritey EQU spritewidth+1		;byte
spritex EQU spritey+1			;byte
dataskip EQU spritex+1		;byte
spriteacross EQU dataskip+1	;byte
spritedx EQU spriteacross+1		;byte
spritedy EQU spritedx+1		;byte
Sprite_Data EQU savesscreen	;0<x<768

;Entry equates
originalcur EQU OP5	;word
curitem EQU OP5+2			;byte
enditem EQU OP5+3			;byte
numentries EQU OP5+4		;byte
itemrow EQU OP5+5			;byte
menutop EQU OP5+6			;byte
temphl EQU OP5+7			;word
entrieslist EQU savesscreen	;unlimited
tempflags EQU OP6

;Font set screen equates
numsets EQU appbackupscreen		;byte
fontsets EQU numsets+1	;no defined length, table of words
setsend EQU OP6		;word, end of sets
setpointer EQU OP6+2	;word, current set
curchar EQU OP5		;byte

;Parsing equates for subfactorial
summation EQU appbackupscreen
kvariable EQU appbackupscreen+16
nvariable EQU appbackupscreen+32

;Parenthesis Assistant Equates
bufferpos EQU appbackupscreen	;word
tempkey EQU 9900h
parens EQU asm_Flag2
evilQuotes EQU 0

;Base displayer equates
newbase EQU appbackupscreen
oldnum EQU newbase+2
strlen EQU oldnum+4
tempstring EQU strlen+1
baseflags EQU asm_Flag1
appendbase EQU 0

;Base input equates
multtable EQU appbackupscreen+20
numbase EQU multtable+250
maxnum EQU numbase+1
partialans EQU maxnum+2
partialans2 EQU partialans+2
partialans3 EQU partialans2+2
convertbase EQU 1

;play() equates
bpm EQU appbackupscreen+2		;the tempo in bpm
octave EQU bpm+1					;octave (0-6)
normal EQU octave+1				;24-bits, the length of a whole note at current tempo in normal style
staccato EQU normal+3			;24-bits, for staccato
legato EQU staccato+3			;24-bits, for legato

playptr EQU legato+3			;string pointer
playend EQU playptr+2		;the last byte of the string
notelen EQU playend+2		;byte - value of the note (1,2,4,8,16,32,64)
style EQU notelen+1			;byte - the style to play in

notetoplay EQU style+1		;byte - the note to play (from 0 to 84)
noteduration EQU notetoplay+1		;24-bits - the duration of the note to play
notelen2 EQU noteduration+3		;byte - backup of (notelen) for pauses

;Quick apps menu equates
numapps EQU 0BEFFh
backup_sp EQU numapps-2	;word

appcode EQU 0BF00h
appnames EQU 0B000h
apppointers EQU 0AC00h

;Save the manatees
manateebyte EQU 0FEFFh
