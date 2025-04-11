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
;Hook_Token:
;	add a,e
;	push hl
;	push de
;	ld hl,3F6h
;	or a
;	sbc hl,de
	;jr z,Hook_Token_Real
;	pop de
;	pop hl
;	scf
;	ret
Hook_Token_Table:
	DW Hook_Token_Symbolic_csc
	DW Hook_Token_Symbolic_csc1
	DW Hook_Token_Symbolic_sec
	DW Hook_Token_Symbolic_sec1
	DW Hook_Token_Symbolic_cot
	DW Hook_Token_Symbolic_cot1
	DW Hook_Token_Symbolic_csch
	DW Hook_Token_Symbolic_csch1
	DW Hook_Token_Symbolic_sech
	DW Hook_Token_Symbolic_sech1
	DW Hook_Token_Symbolic_coth
	DW Hook_Token_Symbolic_coth1
	DW Hook_Token_Symbolic_d
	DW Hook_Token_Symbolic_simp
	DW Hook_Token_Symbolic_sign
	DW Hook_Token_Symbolic_version
	DW Hook_Token_Symbolic_arclength
	DW Hook_Token_Symbolic_let
	DW Hook_Token_Symbolic_numstr
	DW Hook_Token_Symbolic_pretty
	DW Token_SpriteToken
	DW Token_RomanToken
	DW Token_RootToken
	DW Token_FactorToken
	DW Token_GammaToken
	DW Token_BaseToken
	DW Token_LinkGetToken
	DW Token_LinkSendToken
	DW Token_ModuloToken
	DW Token_ConstantToken
	DW Token_SubfactorialToken
	DW Token_PlayToken
	DW Token_RestoreMemToken
	DW Token_ExecAsmToken
Hook_Token_Symbolic_csc:
	DB "csc(",0
Hook_Token_Symbolic_csc1:
	DB "csc",linverse,"(",0
Hook_Token_Symbolic_sec:
	DB "sec(",0
Hook_Token_Symbolic_sec1:
	DB "sec",linverse,"(",0
Hook_Token_Symbolic_cot:
	DB "cot(",0
Hook_Token_Symbolic_cot1:
	DB "cot",linverse,"(",0
Hook_Token_Symbolic_csch:
	DB "csch(",0
Hook_Token_Symbolic_csch1:
	DB "csch",linverse,"(",0
Hook_Token_Symbolic_sech:
	DB "sech(",0
Hook_Token_Symbolic_sech1:
	DB "sech",linverse,"(",0
Hook_Token_Symbolic_coth:
	DB "coth(",0
Hook_Token_Symbolic_coth1:
	DB "coth",linverse,"(",0
Hook_Token_Symbolic_d:
	DB "d(",0
Hook_Token_Symbolic_simp:
	DB "simp(",0
Hook_Token_Symbolic_sign:
	DB "sign(",0
Hook_Token_Symbolic_version:
	DB "version(",0
Hook_Token_Symbolic_arclength:
	DB "arclength(",0
Hook_Token_Symbolic_let:
	DB "let(",0
Hook_Token_Symbolic_numstr:
	DB "numStr(",0
Hook_Token_Symbolic_pretty:
	DB "pretty(",0
