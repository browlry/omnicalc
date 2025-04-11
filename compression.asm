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
Decompress:
	;What's on the stack
	;The address of (size) in the program
	;Then the ROM page that was previously swapped in
	;Lastly, the return to MirageOS or other assorted shell of your choice
	ld hl,Decompress_Code
	ld de,ramcode
	ld bc,Decompress_Code_End-Decompress_Code
	ldir
	jp ramcode
Decompress_Code:
	im 1
	in a,(20h)
	ld (appbackupscreen+4),a
	in a,(2)
	rla
	jr nc,$F
	ld a,1
	out (20h),a
$$:
	pop hl
	pop af
	ld (appbackupscreen),a
	push hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	B_CALL MemChk
	pop de
	or a
	sbc hl,de
	jr c,Decompress_Return_Special
	pop hl
	push hl
	ld bc,1Ch
	sbc hl,bc
	ld (appbackupscreen+2),hl
	ex de,hl
	push hl
	B_CALL InsertMem
	pop bc
	pop hl
	add hl,bc
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec hl
	push de
	;ld de,1Ch
	;or a
	;sbc hl,de
	ld bc,0Bh	;27h-1Ch
	add hl,bc
	ld de,(appbackupscreen+2)
	push de
	call Decompress_LZSS
	;Stack now has:
	;The address of the first byte to delete
	;The number of bytes to deletemem when done
	;MirageOS return address
	in a,(6)
	ld (appbackupscreen+1),a
	ld e,7Bh
	in a,(2)
	rla
	ld a,e
	jr c,$F
	and 1Fh
$$:
	out (6),a
	ld hl,(44F5h)	;paged call return
	ld a,(appbackupscreen)
	out (6),a
	push af
	ld a,(appbackupscreen+1)
	ld e,a
	ld ix,Decompress_Return
	push ix
	push de
	push hl
	;Stack now has:
	;Needed stuff to return to Decompress_Return
	;MirageOS ROM page
	;Address of first byte to delete
	;Number of bytes to delete
	;MirageOS return address
	in a,(2)
	rla
	jr nc,$F
	ld a,(appbackupscreen+4)
	out (20h),a
$$:
	ld hl,(appbackupscreen+2)
	jp (hl)
Decompress_Return_Special:
	add hl,de
	call Decompress_Return_MemoryErr
	pop hl
	ld a,(appbackupscreen)
	out (6),a
	ret
Decompress_Code_End:
Decompress_Return_MemoryErr:
	push hl
	push de
	B_CALL ClrLCDFull
	B_CALL HomeUp
	ld hl,Decompress_Return_MemoryErr_1
	call putstr
	ld de,2
	ld (currow),de
	call putstr
	pop hl
	B_CALL DispHL
	ld hl,3
	ld (currow),hl
	ld hl,Decompress_Return_MemoryErr_2
	call putstr
	pop hl
	B_CALL DispHL
$$:
	ld a,0FFh
	out (1),a
	neg
	xor a
	out (1),a
	neg
	in a,(1)
	inc a
	jr z,$B
	ret
Decompress_Return_MemoryErr_1:
	DB "Insufficent RAM",0
	DB "Required: ",0
Decompress_Return_MemoryErr_2:
	DB "Available: ",0
Decompress_Return:
	ld hl,Decompress_Return_Code
	ld de,ramcode
	ld bc,Decompress_Return_Code_End-Decompress_Return_Code
	ldir
	jp ramcode
Decompress_Return_Code:
	pop af
	out (6),a
	pop hl
	pop de
	B_CALL DelMem
	ret
Decompress_Return_Code_End:
;
; Lite86 II Decompression Routine
; written by Kirk Meyer
;
; NOTE: This must be called as a function
;       unless you modify the "ret z" line.
;
; Inputs:
;   HL -> the compressed data
;   DE -> where to store the output
;
; Outputs:
;   AF, BC, DE, HL trashed
;
Decompress_LZSS:
	ld a,(hl)
	inc hl
	scf
	adc a,a
 	DB 6
DecompressLoop:
	add a,a
	jr z,Decompress_LZSS
	jr c,DecompressChunk
	ldi
	jr DecompressLoop
DecompressChunk:
	ld b,a
	ld c,1Fh
	ld a,(hl)
	cp c
	ret z
	push bc 
	inc hl
	ld b,(hl)
	inc hl
	push hl
	ld l,b
	ld b,a
	or c
	rlca
	rlca
	rlca
	ld h,a
	ld a,b
	and c
	add a,3
	ld c,a
	ld b,0
	add hl,de
	ldir
 	pop hl
 	pop af
	jr DecompressLoop	
