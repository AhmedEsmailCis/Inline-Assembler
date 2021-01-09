
INCLUDE Irvine32.inc
.data
    RegisterNumber EQu 8
	RegisterValueArray Dword RegisterNumber dup(1)   ;eax,ebx,ecx,edx,esi,edi,esp,ebp

    Message1 byte "                                    To End The Program Enter 0 .",0
	Message2 byte "Enter The Code Lines .",0
	
	maxNumber equ 60
	BigLine byte maxNumber dup(?),0
	SmallLine byte maxNumber dup(?),0
	LineWithoutInst byte maxNumber dup(?),0
	Linelength Dword 0

	instructionName byte 3 dup(?),0
	In00 byte "cal",0
	In1 byte "mov",0
	In2 byte "add",0
	In3 byte "sub",0
	In4 byte "mul",0
	In5 byte "div",0
	In6 byte "inc",0
	STR1 byte "ldumpregs",0
	STR2 byte "lwriteint",0
	InstructionId byte ? ;from 1 to 7 >> 1-movzx,2-mov,3-add,4-sub,5-mul,6-div,7-inc

	OperandName byte maxNumber dup(?),0
	OperandLength byte 0
	R32_1 byte "eax",0
	R32_2 byte "ebx",0
	R32_3 byte "ecx",0
	R32_4 byte "edx",0
	R32_5 byte "esi",0
	R32_6 byte "edi",0
	R32_7 byte "esp",0
	R32_8 byte "ebp",0

	R16_1 byte "ax",0
	R16_2 byte "bx",0
	R16_3 byte "cx",0
	R16_4 byte "dx",0
	R16_5 byte "si",0
	R16_6 byte "di",0
	R16_7 byte "sp",0
	R16_8 byte "bp",0

	R8_1 byte "al",0
	R8_2 byte "ah",0
	R8_3 byte "bl",0
	R8_4 byte "bh",0
	R8_5 byte "cl",0
	R8_6 byte "ch",0
	R8_7 byte "dl",0
	R8_8 byte "dh",0
	DestinationType dword 0  ; 3-dword ,2-word ,1-byte
	DestinationDisplacement dword 0  

	SourceType dword 0       ; 4-immediate,3-dword ,2-word ,1-byte 
	SourceDisplacement dword 0
	ImmediateVar dword 0
    Multiplier dword 10
	Multiplier1 dword 16
	Multiplier2 dword 2

	Error1 byte "error A2008 : syntax error  .",0
	Error2 byte "error A2206 : missing operator in expression .",0
	Error3 byte "error A2006 undefined sumbol : invalid register 32 .",0
	Error4 byte "error A2006 undefined sumbol : invalid register 16 .",0
	Error5 byte "error A2006 undefined sumbol : invalid register 8 .",0
	Error6 byte "error A2006 undefined sumbol :  The Source Is Wrong  ..",0
	Error7 byte "error A2008 syntax error : this instruction must take  two operand ..please edit this Line .",0
	Error8 byte "error A2008 syntax error : this instruction must take only one operand ..please edit this Line .",0
	Error9 byte "error A2022 :Mov instruction operands must be the same size . .",0
	Error10 byte "error A2022 :Add instruction operands must be the same size . .",0
	Error11 byte "error A2022 :Sub instruction operands must be the same size . .",0
	Error12 byte "error A2070 : invalid instruction operands .",0
	Error13 byte "error A2006 undefined sumbol : Edit to (call writeint) OR (call dumpregs) ..",0
	Error14 byte "error A2022 : instruction operands must be the same size . .",0
	Error15 byte "Error : Divisor is zero ..",0

	Warning byte "Warning: The result of this operation won’t fit in the specified destination, would you still like to continue? (y/n)",0

	ShowReg1 byte "The Value in Register Eax is : ",0
	ShowReg2 byte "The Value in Register Ebx is : ",0
	ShowReg3 byte "The Value in Register Ecx is : ",0
	ShowReg4 byte "The Value in Register Edx is : ",0
	ShowReg5 byte "The Value in Register Esi is : ",0
	ShowReg6 byte "The Value in Register Edi is : ",0
	ShowReg7 byte "The Value in Register Esp is : ",0
	ShowReg8 byte "The Value in Register Ebp is : ",0
;-----------------------------------------------------------------------------------------------------------------------------------------;
.code
main PROC
    mov edx,offset Message1
	call writestring
	call crlf
    mov edx,offset Message2
	call writestring
	call crlf

    line1: 
					call ReadTheLinesFromTheUser    ; store in BigLine with length eax

    				mov bl , BigLine[0]
 					cmp bl ,'0'                      ; Close The Program And Show The Results if user enter 0
 					je finish00  

					mov  SmallLine[0] , 0    
					call RemoveTheSpacesAndComm     ; store in SmallLine with length Linelength
                    
					cmp SmallLine[0] , 0  
					je finish0                      ; Handle a single line comment .

					call Convert_T0_Small           ; Convert to small letter and store in SmallLine .

					call ReadTheInstrction 

					mov esi, offset instructionName
					mov edi, offset In00
					mov ecx, lengthof instructionName
					repe cmpsb
					jne line2
					call CALLInSTRUCTION
					jmp finish0
			 line2:
					call DefineTheInstruction       ; add InstructionId 1-movzx,2-mov,3-add,4-sub,5-mul,6-div,7-inc

					call RemoveTheInstruction       ; store in LineWithoutInst with length Linelength


					call DefineTheDestination       
	    
							mov eax,Linelength        ; destination and source length
							movzx ebx,OperandLength   ; destination length
							sub eax,ebx
							cmp eax , 0               ; mean a line has one Operand
							jne l3
							call InstructionWithSingleOperand
							jmp finish0
					l3:
							call DefineTheSource
							call InstructionWithTwoOperand
					finish0:
 							call ResetTheData
   jmp line1
     finish00:
 	         call ShowTheRegisters
			 call dumpregs
	         exit
	
main ENDP
;-----------------------------------------------------------------------------------------------------------------------------------------;
	;-----------------------------------------------------------------------------------
	; Doing: Read line from the user Contain operation and spaces and comment if exist 
	; return: string in BigLine and size in eax register
	;-----------------------------------------------------------------------------------
ReadTheLinesFromTheUser PROC
    mov edx,offset BigLine
	mov ecx,maxNumber
	call readstring 
  ret
ReadTheLinesFromTheUser ENDP

	;-----------------------------------------------------------------------------------
	; determine line immportant parts and delete The rest
	; works on bigline and size which in eax
	; return exactly operation parts in smallline and his length in linelingth
	;-----------------------------------------------------------------------------------
RemoveTheSpacesAndComm PROC
		mov ecx,eax
		mov esi,offset BigLine
		mov edi,offset SmallLine
		mov dl , ';'
	    mov bl,' '
		loop1:
				 mov al,[esi]
				 cmp al , dl
				 je finish1
				 cmp al,bl
				 je l2
				 mov [edi],al
				 inc esi
				 inc edi
				 inc Linelength
				 jmp cont
			 l2:
				 inc esi
			 cont:
		loop loop1
	finish1:
  ret
RemoveTheSpacesAndComm ENDP
	;-----------------------------------------------------------------------------------
	; get the instruction from smallline
	; works on smallline and instractionname strings
	; returns instruction name in instructionName
	; all instructions will be with size 3.
	;-----------------------------------------------------------------------------------
ReadTheInstrction PROC
    mov esi,offset SmallLine
	mov edi ,offset instructionName
	mov ecx,3
	loop2:
		   mov al,[esi]
		   mov [edi],al
		   inc esi
		   inc edi
	loop loop2
  ret
ReadTheInstrction ENDP
	;-----------------------------------------------------------------------------------
	; find what is the instruction the user need to use it.
	; work on instruction name and his length and stored strings.
	; returns number of instruction in InstructionId.
	;-----------------------------------------------------------------------------------
DefineTheInstruction PROC
            
            cld    ;clear the direction flag
  			mov esi, offset instructionName
			mov edi, offset In1
			mov ecx, lengthof instructionName
			repe cmpsb
			jne s1
			                     ; In Case Of Movzx. 
			mov bl , "z"                    
			mov dl, SmallLine[3]
			cmp bl ,dl
			jne s0
			mov bl , "x"
			mov dl, SmallLine[4]
			cmp bl ,dl
			jne ERR1
			mov InstructionId ,1
			jmp finish2
		s0:
			mov InstructionId ,2
			jmp finish2
		s1:
			mov esi, offset instructionName
			mov edi, offset In2
			mov ecx, lengthof instructionName
			repe cmpsb
			jne s2
			mov InstructionId ,3
			jmp finish2
		s2:
			mov esi, offset instructionName
			mov edi, offset In3
			mov ecx, lengthof instructionName
			repe cmpsb
			jne s3
			mov InstructionId ,4
			jmp finish2
		s3:
			mov esi, offset instructionName
			mov edi, offset In4
			mov ecx, lengthof instructionName
			repe cmpsb
			jne s4
			mov InstructionId ,5
			jmp finish2
		s4:
			mov esi, offset instructionName
			mov edi, offset In5
			mov ecx, lengthof instructionName
			repe cmpsb
			jne s5
			mov InstructionId ,6
			jmp finish2
		s5:
			mov esi, offset instructionName
			mov edi, offset In6
			mov ecx, lengthof instructionName
			repe cmpsb
			jne ERR1
			mov InstructionId ,7
			jmp finish2
			                                 ; in case of Syntax or any error
		ERR1:
			 mov edx ,offset Error1
			 call writestring
			 call crlf
			 exit
		finish2:
  ret
DefineTheInstruction ENDP
	;-----------------------------------------------------------------------------------------------------
	; doing remove the instruction from smallLine string and store the result in linewithoutinstruction
	; works on smallline string 
	; returns LineWithoutInst string contain only distination and source and comma between them.
	;-----------------------------------------------------------------------------------------------------
RemoveTheInstruction PROC
		mov esi ,offset SmallLine
		mov edi ,offset LineWithoutInst
		mov bl,InstructionId
		cmp bl ,1
		je s6
		add esi ,3
		sub Linelength,3
		jmp s7
		                      ;in case of movzx instruction
	s6:
		add esi ,5 
		sub Linelength,5
	s7:
		mov ecx, Linelength
		loop3:
			 mov al ,[esi]
			 mov [edi],al
			 inc esi
			 inc edi
		loop loop3
  ret
RemoveTheInstruction ENDP
	;----------------------------------------------------------------------------------------------------------------------------
	; put the destination in operand name and with size in operand length untill comma
	; determine which destination register the user need to use it if 8 or 16 or 32
	; works on line without instruction and his length
	; returns two variable distinationdisplacement which contain location of distination register in register array "we define it"
	; and distinatiotype which contain the number describe the register if 1 byte 2 word 3 dword.
	;----------------------------------------------------------------------------------------------------------------------------
DefineTheDestination PROC
			mov esi ,offset LineWithoutInst
			mov edi ,offset OperandName
			mov ecx , Linelength
			mov al,','
			loop4:
					mov bl ,[esi]
					cmp bl,al
					je finish3
					mov [edi],bl
					inc OperandLength
					inc esi
					inc edi
			loop loop4
		finish3:
			cmp OperandLength,3
			ja ERR2
			je s8
			cmp OperandLength,2
			jb ERR2
			je s9
		ERR2:
			mov edx, offset Error2
			call writestring
			call crlf
			exit
		s8:                                ;ebx returns from the procedures 
		    mov DestinationType,3
		    call DefineRegister32
			mov DestinationDisplacement ,ebx
			jmp finish4
		s9:
		   mov al,OperandName[1]
		   mov bl,"l"
		   cmp al ,bl
		   je s10
		   mov bl,"h"
		   cmp al ,bl
		   je s10
		   mov DestinationType,2
		   call DefineRegister16
		   mov DestinationDisplacement ,ebx
		   jmp finish4
	   s10:
	       mov DestinationType,1
		   call DefineRegister8
		   mov DestinationDisplacement ,ebx
		finish4:
  ret
DefineTheDestination ENDP
	;-----------------------------------------------------------------------------------
	; works on operand name and operand length and stored strings .
	; determine which 32 register will use it 
	; retunes the register location in our registerstring in ebx register 
	;-----------------------------------------------------------------------------------
DefineRegister32 PROC
           cld    ;clear the direction flag
           mov esi, offset OperandName
		   mov edi, offset R32_1
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s11
		   mov ebx ,0
		   jmp finish5
	   s11:
	       mov esi, offset OperandName
		   mov edi, offset R32_2
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s12
		   mov ebx , 4
		   jmp finish5
	   s12:
	       mov esi, offset OperandName
		   mov edi, offset R32_3
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s13
		   mov ebx ,8
		   jmp finish5
	   s13:
	       mov esi, offset OperandName
		   mov edi, offset R32_4
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s14
		   mov ebx ,12
		   jmp finish5
	   s14:
	       mov esi, offset OperandName
		   mov edi, offset R32_5
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s15
		   mov ebx ,16
		   jmp finish5
	   s15:
	       mov esi, offset OperandName
		   mov edi, offset R32_6
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s16
		   mov ebx ,20
		   jmp finish5
	   s16:
	       mov esi, offset OperandName
		   mov edi, offset R32_7
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s17
		   mov ebx ,24
		   jmp finish5
	   s17:
	       mov esi, offset OperandName
		   mov edi, offset R32_8
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne ERR3
		   mov ebx ,28
		   jmp finish5
	  ERR3:
			mov edx ,offset Error3
			call writestring
			call crlf
			exit
	  finish5:
  ret
DefineRegister32 ENDP
	;-----------------------------------------------------------------------------------
	; works on operand name and operand length and stored strings .
	; determine which162 register will use it 
	; retunes the register location in our registerstring in ebx register 
	;-----------------------------------------------------------------------------------
DefineRegister16 PROC
           cld    ;clear the direction flag
           mov esi, offset OperandName
		   mov edi, offset R16_1
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s18
		   mov ebx ,0
		   jmp finish6
	   s18:
	       mov esi, offset OperandName
		   mov edi, offset R16_2
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s19
		   mov ebx ,4
		   jmp finish6
	   s19:
	       mov esi, offset OperandName
		   mov edi, offset R16_3
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s20
		   mov ebx ,8
		   jmp finish6
	   s20:
	       mov esi, offset OperandName
		   mov edi, offset R16_4
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s21
		   mov ebx ,12
		   jmp finish6
	   s21:
	       mov esi, offset OperandName
		   mov edi, offset R16_5
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s22
		   mov ebx ,16
		   jmp finish6
	   s22:
	       mov esi, offset OperandName
		   mov edi, offset R16_6
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s23
		   mov ebx ,20
		   jmp finish6
	   s23:
	       mov esi, offset OperandName
		   mov edi, offset R16_7
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s24
		   mov ebx ,24
		   jmp finish6
	   s24:
	       mov esi, offset OperandName
		   mov edi, offset R16_8
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne ERR4
		   mov ebx ,28
		   jmp finish6
	  ERR4:
			mov edx ,offset Error4
			call writestring
			call crlf
			exit
	  finish6:
  ret
DefineRegister16 ENDP
	;-----------------------------------------------------------------------------------
	; works on operand name and operand length and stored strings .
	; determine which 8 register will use it 
	; retunes the register location in our registerstring in ebx register 
	;-----------------------------------------------------------------------------------
DefineRegister8 PROC
           cld    ;clear the direction flag
           mov esi, offset OperandName
		   mov edi, offset R8_1
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s25
		   mov ebx ,0
		   jmp finish7
	   s25:
	       mov esi, offset OperandName
		   mov edi, offset R8_2
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s26
		   mov ebx ,1
		   jmp finish7
	   s26:
	       mov esi, offset OperandName
		   mov edi, offset R8_3
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s27
		   mov ebx ,4
		   jmp finish7
	   s27:
	       mov esi, offset OperandName
		   mov edi, offset R8_4
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s28
		   mov ebx ,5
		   jmp finish7
	   s28:
	       mov esi, offset OperandName
		   mov edi, offset R8_5
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s29
		   mov ebx ,8
		   jmp finish7
	   s29:
	       mov esi, offset OperandName
		   mov edi, offset R8_6
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s30
		   mov ebx ,9
		   jmp finish7
	   s30:
	       mov esi, offset OperandName
		   mov edi, offset R8_7
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne s31
		   mov ebx ,12
		   jmp finish7
	   s31:
	       mov esi, offset OperandName
		   mov edi, offset R8_8
		   movzx ecx, OperandLength
		   repe cmpsb
		   jne ERR5
		   mov ebx ,13
		   jmp finish7
	  ERR5:
			mov edx ,offset Error5
			call writestring
			call crlf
			exit
	  finish7:
  ret
DefineRegister8 ENDP
	;--------------------------------------------------------------------------------------------------------------------------------
	; put the source in operand name and with size in operand length to end of string
	; determine which source register the user need to use it if 8 or 16 or 32 or immediate
	; works on line without instruction and his length
	; returns two variable sourcedisplacement which contain location of source register in register array "we define it" or imm value
	; and sourcetype which contain the number describe the register size if 1 byte 2 word 3 dword  4 immediate.
	;---------------------------------------------------------------------------------------------------------------------------------
DefineTheSource PROC
  
			  mov esi ,offset LineWithoutInst
			  movzx ebx,OperandLength
			  inc ebx
			  add esi,ebx
			  sub Linelength,ebx
			  mov OperandLength,0
			  mov edi ,offset OperandName
			  mov ecx , Linelength
			  loop5:
					 mov bl,[esi]
					 mov [edi],bl
					 inc edi
					 inc esi
					 inc OperandLength
			 loop loop5

			 cmp OperandLength,3
			 ja immediate
			 jne source1
			 cmp OperandName[0],'e'
			 jne immediate
			 mov SourceType,3
			 call DefineRegister32
			 mov SourceDisplacement ,ebx
			 jmp finish8
	source1:
			 cmp OperandLength,2
			 jb immediate
			 mov al,OperandName[1]
			 cmp al ,'l'
			 je reg8
			 cmp al ,'h'
			 je reg8
			 cmp al,'x'
			 je reg16
			 cmp al,'i'
			 je reg16
			 cmp al,'p'
			 je reg16
			 jmp immediate
		reg8:
		     cmp OperandName[0],'0'
			 ja A
			 jmp B
			A:
			 cmp OperandName[0],'9'
			 jb immediate
           B:
			 mov SourceType,1
			 call DefineRegister8
			 mov SourceDisplacement ,ebx
			 jmp finish8
	   reg16:
			 mov SourceType,2
			 call DefineRegister16
			 mov SourceDisplacement ,ebx
			 jmp finish8
  immediate:
			 mov SourceType,4

			 call CheckImmediate

			 cmp DestinationType,1
			 jne E1
			 cmp ImmediateVar , 0ffh
			 ja ERRR
			 jmp finish8
		  E1:
			 cmp DestinationType,2
			 jne E2
			 cmp ImmediateVar , 0ffffh
			 ja ERRR
			 jmp finish8
           E2:
			 cmp ImmediateVar , 0ffffffffh 
			 ja ERRR
			 jmp finish8
		 ERRR:
		      mov edx ,offset Error14
			  call writestring
			  call crlf
			  exit
	 finish8:

  ret
DefineTheSource ENDP
	;---------------------------------------------------------------------------------------------------
	; works when the source be immediate if decimal or hexa decimal or binary
	; determine the source decimal or hexa decimal or binary and put it in Immediatevar and return it
	;---------------------------------------------------------------------------------------------------
CheckImmediate PROC 
			    	mov esi,offset OperandName
			    	movzx ecx , OperandLength
					mov al,'h'
					add esi,ecx
					sub esi,1
					cmp al,byte ptr[esi]
					je hex
					mov al,'b'
					cmp al,byte ptr[esi]
					je bin
					jne integers
		       bin: 
			        call binary_number
			        jmp finish10
			   hex:
			        call hex_number
			        jmp finish10
		  integers:
		            call int_number
		  finish10:
   ret
CheckImmediate ENDP
	;-----------------------------------------------------------------------------------
	; works if immediate number is Hexa decimal
	; convert it from string to decimal number
	; and return it in immediatevar
	;-----------------------------------------------------------------------------------
hex_number proc
				 mov ImmediateVar,0;
				 mov esi,offset OperandName
				 dec ecx

				 mov dl,byte ptr [esi]
				 cmp dl,'a'
				 jb cv0
				 jmp ERR6
              cv0:
			     cmp dl,0
				 jne loop6
				 inc esi
				 dec ecx
		   loop6:
						mov bl,byte ptr[esi]
						cmp bl,'0'
						jb cv1
						cmp bl,'9'
						ja cv1
						sub bl,48
						jmp skip6
				cv1:
						cmp bl,'a'
						jne cv_1
						mov bl,10
						jmp skip6
				cv_1:
						cmp bl,'b'
						jne cv_2
						mov bl,11
						jmp skip6
				cv_2:
						cmp bl,'c'
						jne cv_3
						mov bl,12
						jmp skip6
				cv_3:
						cmp bl,'d'
						jne cv_4
						mov bl,13
						jmp skip6
				cv_4:
						cmp bl,'e'
						jne cv_5
						mov bl,14
						jmp skip6
				cv_5:   
						cmp bl,'f'
						jne ERR6
						mov bl,15
						jmp skip6
				skip6:
						mov eax,1
						push ecx
									loop7:
											mul Multiplier1
									loop loop7
						mov edx,0
						div Multiplier1
						movzx edi,bl
						mul edi
						add ImmediateVar,eax
						mov eax,ImmediateVar
						inc esi
						pop ecx
		   loop loop6
				             jmp skip14
						ERR6:
							 mov edx,offset Error6
							 call writestring
							 call crlf
							 exit
					  skip14:

			 ret
hex_number ENDP
	;-----------------------------------------------------------------------------------
	; works if immediate number is  binary
	; convert it from string to number
	; and return it in immediatevar
	;-----------------------------------------------------------------------------------
binary_number proc
            mov ImmediateVar,0
			mov esi,offset OperandName
			dec ecx
			loop8:
						mov bl,byte ptr[esi]
						cmp bl,'0'
						je cb1
						cmp bl,'1'
						je cb1
						jmp ERR6
				   cb1:
					    sub bl,48
					    mov eax,1
					    push ecx
									  loop9:
											 mul Multiplier2
									  loop loop9
						 mov edx,0
						 div Multiplier2
						 movzx edi,bl
						 mul edi
						 add ImmediateVar,eax
						 inc esi
						 pop ecx
			loop loop8
			             jmp skip13
			 ERR6:
					 mov edx,offset Error6
					 call writestring
					 call crlf
					 exit
			skip13:

			ret
binary_number ENDP
	;-----------------------------------------------------------------------------------
	; works if immediate number is decimal
	; convert it from string to decimal number
	; and return it in immediatevar
	;-----------------------------------------------------------------------------------
int_number proc
                   mov esi,offset OperandName
		       	   movzx ecx , OperandLength
		   loop10:
				    mov edx,0
					mov eax ,ImmediateVar
					mul Multiplier
					mov ImmediateVar,eax

					mov al,[esi]
					cmp al,'0'
					jb ERR6
					cmp al,'9'
					ja ERR6
					sub al,48
					movzx ebx,al
					add ImmediateVar,ebx
				    inc esi
	  loop loop10
	                jmp skip15
	          ERR6:
					 mov edx,offset Error6
					 call writestring
					 call crlf
					 exit
	        skip15:
	ret
int_number ENDP
	;-----------------------------------------------------------------------------------
	; works if we have only one operand
	; compare instructionid and decide which proc will use
	; in case of 5 multiblication 6 division 7 increment 
	;-----------------------------------------------------------------------------------
InstructionWithSingleOperand PROC
				   cmp InstructionId ,5
				   jne s41
				   call MulOperation
				   jmp finish11
			   s41:
				   cmp InstructionId ,6
				   jne s42
				   call DivOperation
				   jmp finish11
			   s42:
				   cmp InstructionId ,7
				   jne ERR7
				   call IncOperation
				   jmp finish11
			  ERR7:
							 mov edx,offset Error7
							 call writestring
							 call crlf
							 exit
			finish11:

  ret
InstructionWithSingleOperand ENDP
	;-----------------------------------------------------------------------------------
	; works if we have only two operand
	; compare instructionid and decide which proc will use
	; in case of 1 movzx 2 mov 3 addition 4 subtraction
	;-----------------------------------------------------------------------------------
InstructionWithTwoOperand PROC
		           cmp InstructionId ,1
				   jne s43
				   call MovzxOperation
				   jmp finish12
			   s43:
				   cmp InstructionId ,2
				   jne s44
				   call MovOperation
				   jmp finish12
			   s44:
				   cmp InstructionId ,3
				   jne s45
				   call AddOperation
				   jmp finish12
			   s45:
				   cmp InstructionId ,4
				   jne ERR8
				   call SubOperation
				   jmp finish12
			  ERR8:
					mov edx,offset Error8
				    call writestring
					call crlf
				    exit
		 finish12:

  ret
InstructionWithTwoOperand ENDP
	;-----------------------------------------------------------------------------------
	; works on RegisterValueArray string and DestinationType and DestinationDisplacement
	; doing the increment operation 
	; return the result in destination operand
	; using DestinationType and DestinationDisplacement
	;-----------------------------------------------------------------------------------
IncOperation PROC
			mov esi,offset RegisterValueArray 
			mov edx,DestinationType
            mov ebx, DestinationDisplacement

			cmp edx,1
	        jne s46
			mov eax,0
			mov al,byte ptr[esi + ebx]
			add al,1
			jc W1
			    mov al,byte ptr[esi + ebx]
			    inc al
				mov byte ptr[esi+ebx],al
				jmp finish13
			W1:
			    call Show_Warning
				cmp al,'n'
				je N1
				cmp al,'y'
				je Y1
				jmp finish13
			  N1: 
				movzx eax ,byte ptr[esi + ebx]
				call writeint
				call crlf 
				jmp finish13
	          Y1:
				mov al,byte ptr[esi + ebx]
			    inc al
				mov byte ptr[esi+ebx],al
				movzx eax ,byte ptr[esi+ebx]
				call writeint
				call crlf
				jmp finish13
			;-------------------------------------------------------- 
			
		s46:
			cmp edx,2
			jne s47
			mov eax,0
			mov ax,word ptr[esi + ebx]
			add ax,1
			jc W2
			    mov ax,word ptr[esi + ebx]
			    inc ax
				mov word ptr[esi+ebx],ax
				jmp finish13
			W2:
			    call Show_Warning
				cmp al,'n'
				je N2
				cmp al,'y'
				je Y2
				jmp finish13
			  N2: 
				movzx eax ,word ptr[esi + ebx]
				call writeint
				call crlf 
				jmp finish13
	          Y2:
				mov ax,word ptr[esi + ebx]
			    inc ax
				mov word ptr[esi+ebx],ax
				movzx eax ,word ptr[esi+ebx]
				call writeint
				call crlf
				jmp finish13
			;-------------------------------------------------------- 
		s47:
			mov eax,0
			mov eax,dword ptr[esi + ebx]
			add eax,1
			jc W3
			    mov eax,dword ptr[esi + ebx]
			    inc eax
				mov dword ptr[esi+ebx],eax
				jmp finish13
			W3:
			    call Show_Warning
				cmp al,'n'
				je N3
				cmp al,'y'
				je Y3
				jmp finish13
			  N3: 
				mov eax ,dword ptr[esi + ebx]
				call writeint
				call crlf 
				jmp finish13
	          Y3:
				mov eax,dword ptr[esi + ebx]
			    inc eax
				mov dword ptr[esi+ebx],eax
				mov eax ,dword ptr[esi+ebx]
				call writeint
				call crlf
				jmp finish13
			;--------------------------------------------------------
   finish13:
  
  ret
IncOperation ENDP
	;-----------------------------------------------------------------------------------
	; works on RegisterValueArray string and DestinationType and DestinationDisplacement
	; doing the multiplication operation and return the result in eax register
	; using DestinationType and DestinationDisplacement
	;-----------------------------------------------------------------------------------
MulOperation PROC
             mov esi,offset RegisterValueArray
			 mov edi,DestinationType
			 mov ecx, DestinationDisplacement
			 mov eax ,[esi]
			 mov edx ,[esi+12]

			 cmp edi,1
			 jne s48
			 mov eax ,[esi]
			 mov bl,byte ptr[esi+ecx]
			 mul bl
			 jc W1
				 mov word ptr[esi],ax
				 jmp finish14
			 W1:
			    call Show_Warning
				cmp al,'n'
				je N1
				cmp al,'y'
				je Y1
				jmp finish14
			  N1: 
				movzx eax ,byte ptr[esi]
				call writedec
				call crlf 
				jmp finish14
	          Y1:
			    mov eax ,[esi]
			    mov bl,byte ptr[esi+ecx]
			    mul bl
				mov word ptr[esi],ax
				movzx eax ,word ptr[esi]
				call writedec
				call crlf
				jmp finish14
				;------------------------------------------------
		  s48:
			 cmp edi,2
			 jne s49
			 mov eax ,[esi]
			 mov edx ,[esi+12]
			 mov bx,word ptr[esi+ecx]
			 mul bx
			 jc W2
				 mov word ptr[esi],ax
				 mov word ptr[esi+12],dx
			     jmp finish14
			 W2:
			    call Show_Warning
				cmp al,'n'
				je N2
				cmp al,'y'
				je Y2
				jmp finish14
			  N2: 
				movzx eax ,word ptr[esi]
				call writedec
				call crlf 
				jmp finish14
	          Y2:
			    mov eax ,[esi]
			    mov edx ,[esi+12]
			    mov bx,word ptr[esi+ecx]
			    mul bx
				mov word ptr[esi],ax
				mov word ptr[esi+12],dx

				movzx eax ,word ptr[esi+12]
				shl eax,16
				movzx ebx, word ptr[esi]
				add eax,ebx
				call writedec
				call crlf
				jmp finish14
			   ;-------------------------------------------------------
		  s49:
		     mov eax ,[esi]
			 mov edx ,[esi+12]
			 mov ebx,dword ptr[esi+ecx]
			 mul ebx
			 jc W3
				 mov dword ptr[esi],eax
				 mov dword ptr[esi+12],edx
				 jmp finish14
			 W3:
			    call Show_Warning
				cmp al,'n'
				je N3
				cmp al,'y'
				je Y3
				jmp finish14
			  N3: 
				mov eax ,dword ptr[esi]
				call writedec
				call crlf 
				jmp finish14
	          Y3:
			    mov eax ,[esi]
			    mov edx ,[esi+12]
			    mov ebx,dword ptr[esi+ecx]
			    mul ebx
				mov dword ptr[esi],eax
				mov dword ptr[esi+12],edx

				mov eax ,dword ptr[esi+12]
				call writedec
				mov eax,0 
				mov al ,'|'
				call writechar
				call writechar
				mov eax,0 
				mov eax ,dword ptr[esi]
				call writedec
				call crlf

			finish14:
  ret
MulOperation ENDP
	;-----------------------------------------------------------------------------------
	; works on RegisterValueArray string and DestinationType and DestinationDisplacement
	; doing the division operation 
	; return the result in eax register
	; using DestinationType and DestinationDisplacement
	;-----------------------------------------------------------------------------------
DivOperation PROC
		 mov esi,offset RegisterValueArray
		 mov edi,DestinationType
		 mov ecx, DestinationDisplacement
		 mov eax,[esi]
		 mov edx,[esi+12]

		 cmp edi,1
		 jne s50
		 mov eax,[esi]
		 mov bl,byte ptr[esi+ecx]
		 cmp bl,0
		 je ERR15
		 cmp ah,0
		 jne W1 
			 div bl
			 mov byte ptr[esi],al
			 mov byte ptr[esi+1],ah
			 jmp finish15
		 W1:
		      call Show_Warning
				cmp al,'n'
				je N1
				cmp al,'y'
				je Y1
				jmp finish15
			  N1: 
			    mov eax,0
			    mov al,'R'
				call writechar
				mov al,':'
				call writechar
				movzx eax ,byte ptr[esi]
				call writedec
				call crlf 
				jmp finish15
	          Y1:
			    mov eax,[esi]
		        mov bl,byte ptr[esi+ecx]
			    div bl
			    mov byte ptr[esi],al
			    mov byte ptr[esi+1],ah
				mov eax,0
				mov al,'S'
				call writechar
				mov al,':'
				call writechar
				movzx eax ,byte ptr[esi]
				call writedec
				call crlf

				mov eax,0
				mov al,'R'
				call writechar
				mov al,':'
				call writechar
				movzx eax ,byte ptr[esi+1]
				call writedec
				call crlf
				jmp finish15
		 ;----------------------------------------
	 s50:
		 cmp edi,2
		 jne s51
		 mov eax,[esi]
		 mov edx,[esi+12]
		 mov bx,word ptr[esi+ecx]
		 cmp bx,0
		 je ERR15
		 cmp dx,0
		 jne W2
			 div bx
			 mov word ptr[esi],ax
			 mov word ptr[esi+12],dx
			 jmp finish15
		 W2:
		     call Show_Warning
				cmp al,'n'
				je N2
				cmp al,'y'
				je Y2
				jmp finish15
			  N2: 
			    mov eax,0
			    mov al,'R'
				call writechar
				mov al,':'
				call writechar
				movzx eax ,word ptr[esi]
				call writedec
				call crlf 
				jmp finish15
	          Y2:
			    mov eax,[esi]
		        mov edx,[esi+12]
		        mov bx,word ptr[esi+ecx]
			    div bx
			    mov word ptr[esi],ax
			    mov word ptr[esi+12],dx
				mov eax,0
				mov al,'S'
				call writechar
				mov al,':'
				call writechar
				movzx eax ,word ptr[esi]
				call writedec
				call crlf

				mov eax,0
				mov al,'R'
				call writechar
				mov al,':'
				call writechar
				movzx eax ,word ptr[esi+12]
				call writedec
				call crlf
				jmp finish15
		 ;----------------------------------------
	 s51:
	     mov eax,[esi]
		 mov edx,[esi+12]
		 mov ebx,dword ptr[esi+ecx]
		 cmp bx,0
		 je ERR15
		 cmp edx ,0
		 jne W3
			 div ebx
			 mov dword ptr[esi],eax
			 mov dword ptr[esi+12],edx
			 jmp finish15
		 W3:
		     call Show_Warning
				cmp al,'n'
				je N3
				cmp al,'y'
				je Y3
				jmp finish15
			  N3: 
			    mov eax,0
			    mov al,'R'
				call writechar
				mov al,':'
				call writechar
				mov eax ,dword ptr[esi]
				call writedec
				call crlf 
				jmp finish15
	          Y3:
			    mov eax,[esi]
		        mov edx,[esi+12]
		        mov ebx,dword ptr[esi+ecx]
			    div ebx
			    mov dword ptr[esi],eax
			    mov dword ptr[esi+12],edx

				mov eax,0
				mov al,'S'
				call writechar
				mov al,':'
				call writechar
				mov eax ,dword ptr[esi]
				call writedec
				call crlf

				mov eax,0
				mov al,'R'
				call writechar
				mov al,':'
				call writechar
				mov eax ,dword ptr[esi+12]
				call writedec
				call crlf
				jmp finish15
		 ;----------------------------------------
       ERR15:
	        mov edx,offset Error15
			call writestring
			call crlf 
			exit
      finish15:

  ret
DivOperation ENDP
;-------------------------------------------------------------------------------------------------------------------------
; works on RegisterValueArray string and DestinationType and DestinationDisplacement and SourceDisplacement and SourceType
; doing the mov  operation 
; return the result in destination operand
; using DestinationType and DestinationDisplacement and SourceDisplacement and SourceType
;-------------------------------------------------------------------------------------------------------------------------
MovOperation PROC
				 mov esi , offset RegisterValueArray
				 mov ecx , DestinationDisplacement
				 mov edi , SourceDisplacement
				 mov eax , ImmediateVar

				 cmp DestinationType,1 
				 jne s53
				 cmp SourceType,1
				 jne s52
				 mov dl, byte ptr [esi+edi]
				 mov byte ptr [esi+ecx] , dl
				 jmp finish16
			s52:
				 cmp SourceType,4
				 jne ERR9
				 mov byte ptr [esi+ecx] , al
				 jmp finish16
			s53:
				 cmp DestinationType,2
				 jne s55
				 cmp SourceType,2
				 jne s54
				 mov dx, word ptr [esi+edi]
				 mov word ptr [esi+ecx] , dx
				 jmp finish16
			s54:
				 cmp SourceType,4
				 jne ERR9
				 mov word ptr [esi+ecx] , ax
				 jmp finish16
			s55:
				 cmp SourceType,3
				 jne s56
				 mov edx, dword ptr [esi+edi]
				 mov dword ptr [esi+ecx] , edx
				 jmp finish16
			s56:
				 cmp SourceType,4
				 jne ERR9
				 mov dword ptr [esi+ecx] , eax
				 jmp finish16
		   ERR9:
				 mov edx,offset Error9
				 call writestring
				 call crlf
				 exit
		  finish16:

  ret
MovOperation ENDP
	;-------------------------------------------------------------------------------------------------------------------------
	; works on RegisterValueArray string and DestinationType and DestinationDisplacement and SourceDisplacement and SourceType
	; doing the addition  operation 
	; return the result in destination operand
	; using DestinationType and DestinationDisplacement and SourceDisplacement and SourceType
	;--------------------------------------------------------------------------------------------------------------------------
AddOperation PROC
                 mov esi , offset RegisterValueArray
				 mov ecx , DestinationDisplacement
				 mov edi , SourceDisplacement
				 mov eax,0
				 mov edx,0
				 mov ebx,0
				 ;-------------------------------------------------
				 cmp DestinationType,1 
				 jne s58
				 cmp SourceType,1
				 jne s57
				 mov dl, byte ptr [esi+edi]
				 mov bl, byte ptr [esi+ecx]
				 add bl,dl
				 jc W1
					 mov dl, byte ptr [esi+edi]
					 add byte ptr [esi+ecx] , dl
					 jmp finish17
				 W1: 
						call Show_Warning
						cmp al,'n'
						je N1
						cmp al,'y'
						je Y1
						jmp finish17
					 N1: 
					    movzx eax ,byte ptr [esi+ecx]
						call writeint
						call crlf 
						jmp finish17
					 Y1:
						mov dl, byte ptr [esi+edi]
						add byte ptr [esi+ecx] , dl
						movzx eax ,byte ptr [esi+ecx]
						call writeint
						call crlf
						jmp finish17
					;--------------------------------------------------------
			s57:
				 cmp SourceType,4
				 jne ERR10
				 mov eax , ImmediateVar
				 mov bl, byte ptr [esi+ecx]
				 add bl,al
				 jc W_1
					 mov eax , ImmediateVar
					 add byte ptr [esi+ecx] , al
					 jmp finish17
				 W_1: 
						call Show_Warning
						cmp al,'n'
						je N_1
						cmp al,'y'
						je Y_1
						jmp finish17
					 N_1: 
					    movzx eax ,byte ptr [esi+ecx]
						call writeint
						call crlf 
						jmp finish17
					 Y_1:
						mov eax , ImmediateVar
					    add byte ptr [esi+ecx] , al
						movzx eax ,byte ptr [esi+ecx]
						call writeint
						call crlf
						jmp finish17
				 ;------------------------------------------------------------
			s58:
				 cmp DestinationType,2
				 jne s60
				 cmp SourceType,2
				 jne s59
				 mov dx, word ptr [esi+edi]
				 mov bx, word ptr [esi+ecx]
				 add bx,dx
				 jc W2
					 mov dx, word ptr [esi+edi]
					 add  word ptr [esi+ecx] , dx
					 jmp finish17
				 W2: 
						call Show_Warning
						cmp al,'n'
						je N2
						cmp al,'y'
						je Y2
						jmp finish17
					 N2: 
					    movzx eax ,word ptr [esi+ecx]
						call writeint
						call crlf 
						jmp finish17
					 Y2:
						mov dx, word ptr [esi+edi]
					    add  word ptr[esi+ecx] , dx
						movzx eax ,word ptr [esi+ecx]
						call writeint
						call crlf
						jmp finish17
				 ;------------------------------------------------------------
			s59:
				 cmp SourceType,4
				 jne ERR10
				 mov eax , ImmediateVar
				 mov bx, word ptr [esi+ecx]
				 add bx,ax
				 jc W_2
					 mov eax , ImmediateVar
					 add word ptr [esi+ecx] , ax
					 jmp finish17
				 W_2: 
						call Show_Warning
						cmp al,'n'
						je N_2
						cmp al,'y'
						je Y_2
						jmp finish17
					 N_2: 
					    movzx eax ,word ptr [esi+ecx]
						call writeint
						call crlf 
						jmp finish17
					 Y_2:
						mov eax , ImmediateVar
					    add word ptr [esi+ecx] , ax
						movzx eax ,word ptr [esi+ecx]
						call writeint
						call crlf
						jmp finish17
				 jmp finish17
				 ;------------------------------------------------------------
			s60:
				 cmp SourceType,3
				 jne s61
				 mov edx, dword ptr [esi+edi]
				 mov ebx, dword ptr [esi+ecx]
				 add ebx,edx
				 jc W3
					 mov edx, dword ptr [esi+edi]
					 add dword ptr [esi+ecx] , edx
					 jmp finish17
				 W3: 
						call Show_Warning
						cmp al,'n'
						je N3
						cmp al,'y'
						je Y3
						jmp finish17
					 N3: 
					    mov eax ,dword ptr [esi+ecx]
						call writeint
						call crlf 
						jmp finish17
					 Y3:
						mov edx, dword ptr [esi+edi]
					    add dword ptr [esi+ecx] , edx
						mov eax ,dword ptr [esi+ecx]
						call writeint
						call crlf
						jmp finish17
				 ;------------------------------------------------------------
			s61:
				 cmp SourceType,4
				 jne ERR10
				 mov eax , ImmediateVar
				 mov ebx, dword ptr [esi+ecx]
				 add ebx,eax
				 jc W_3
					 mov eax , ImmediateVar
					 add dword ptr [esi+ecx] , eax
					 jmp finish17
				 W_3: 
						call Show_Warning
						cmp al,'n'
						je N_3
						cmp al,'y'
						je Y_3
						jmp finish17
					 N_3: 
					    mov eax ,dword ptr [esi+ecx]
						call writeint
						call crlf 
						jmp finish17
					 Y_3:
						mov eax , ImmediateVar
					    add dword ptr [esi+ecx] , eax
						mov eax ,dword ptr [esi+ecx]
						call writeint
						call crlf
						jmp finish17
				 jmp finish17
				 ;------------------------------------------------------------
		   ERR10:
				 mov edx,offset Error10
				 call writestring
				 call crlf
				 exit
		  finish17:
  ret
AddOperation ENDP

	;--------------------------------------------------------------------------------------------------------------------------;
	; works on RegisterValueArray string and DestinationType and DestinationDisplacement and SourceDisplacement and SourceType ;
	; doing the subtraction operation                                                                                          ;
	; return the result in the destination operand                                                                             ;
	; using DestinationType and DestinationDisplacement and SourceDisplacement and SourceType                                  ;
	;------------------------------------------------------------------------------------------------------------------------- ;
SubOperation PROC
                 mov esi , offset RegisterValueArray
				 mov ecx , DestinationDisplacement
				 mov edi , SourceDisplacement
				 mov eax , ImmediateVar

				 cmp DestinationType,1 
				 jne s63
				 cmp SourceType,1
				 jne s62
				 mov dl, byte ptr [esi+edi]
				 sub byte ptr [esi+ecx] , dl
				 jmp finish18
			s62:
				 cmp SourceType,4
				 jne ERR11
				 sub byte ptr [esi+ecx] , al
				 jmp finish18
			s63:
				 cmp DestinationType,2
				 jne s65
				 cmp SourceType,2
				 jne s64
				 mov dx, word ptr [esi+edi]
				 sub word ptr [esi+ecx] , dx
				 jmp finish18
			s64:
				 cmp SourceType,4
				 jne ERR11
				 sub word ptr [esi+ecx] , ax
				 jmp finish18
			s65:
				 cmp SourceType,3
				 jne s66
				 mov edx, dword ptr [esi+edi]
				 sub dword ptr [esi+ecx] , edx
				 jmp finish18
			s66:
				 cmp SourceType,4
				 jne ERR11
				 sub dword ptr [esi+ecx] , eax
				 jmp finish18
		   ERR11:
				 mov edx,offset Error11
				 call writestring
				 call crlf
				 exit
		  finish18:
  ret
SubOperation ENDP
	;--------------------------------------------------------------------------------------------------------------------------;
	; works on RegisterValueArray string and DestinationType and DestinationDisplacement and SourceDisplacement and SourceType ;
	; doing the movzx operation                                                                                                ; 
	; destination must be bigger size from the source                                                                          ; 
	; return the result in destination operand                                                                                 ;
	; using DestinationType and DestinationDisplacement and SourceDisplacement and SourceType                                  ;
	;--------------------------------------------------------------------------------------------------------------------------;
MovzxOperation PROC
     
			  mov esi,offset RegisterValueArray
			  mov edi,DestinationDisplacement
			  mov ecx,SourceDisplacement

			  cmp DestinationType,3
			  jne s68
			  cmp SourceType,2
			  jne s67
			  movzx eax,word ptr[esi+ecx]
			  mov dword ptr[esi+edi],eax
			  jmp Finish19
		  s67:
			  cmp SourceType,1
			  jne ERR12
			  movzx eax,byte ptr[esi+ecx]
			  mov dword ptr[esi+edi],eax
			  jmp Finish19
		   s68:
			  cmp DestinationType,2
			  jne ERR12
			  cmp SourceType,1
			  jne ERR12
			  movzx ax,byte ptr[esi+ecx]
			  mov word ptr[esi+edi],ax
			  jmp Finish19
		   ERR12:
				mov edx,offset Error12
				call writestring
				call crlf
				exit
	   Finish19:
	   
  ret
MovzxOperation ENDP
	;--------------------------------------------------------------------------------------------------;
	; do this proc if the user need to see dumpregs or see what is the value in eax register "writeint";
	; works on LineWithoutInst and his length and stored etrings STR1 and STR2                         ;
	; result show the registers values or eax register                                                 ;
	;--------------------------------------------------------------------------------------------------;
CALLInSTRUCTION PROC
			 call RemoveTheInstruction

			 mov esi, offset LineWithoutInst
			 mov edi, offset STR1
			 mov ecx,  Linelength
			 repe cmpsb
			 jne s69
			     call FillRegister
			     call dumpregs
			 jmp Finish20
		 s69:
			 mov esi, offset LineWithoutInst
			 mov edi, offset STR2
			 mov ecx,  Linelength
			 repe cmpsb
			 jne ERR13
			      mov esi,offset RegisterValueArray
				  mov eax ,[esi]
				  call writeint
				  call crlf
			 jmp Finish20
		ERR13:
			 mov edx,offset Error13
			 call writestring
			 call crlf
			 exit
	 Finish20:

  ret
CALLInSTRUCTION ENDP
	;-----------------------------------------------------------------------------------;
	; convert from capital litters to small letters if exist                            ;
	; works on smallLine and line length												;
	;-----------------------------------------------------------------------------------;
Convert_T0_Small PROC
		mov esi, offset SmallLine
		mov ecx, Linelength
		loop11:
					;input validations (Limitation the char to be between a and z)
					cmp byte ptr [esi], 'A'
					jb skip_
					cmp byte ptr [esi], 'Z'
					ja skip_
					add byte ptr [esi], 00100000b
				skip_:
					inc esi
		loop loop11
    ret
Convert_T0_Small ENDP
	;-----------------------------------------------------------------------------------; 
	; reinitialize the memory date which we use it every line after using it            ;
	;-----------------------------------------------------------------------------------;
ResetTheData PROC
	mov Linelength ,0
	mov OperandLength ,0
	mov InstructionId ,0
	mov DestinationType ,0
	mov DestinationDisplacement ,0
	mov SourceType ,0
	mov SourceDisplacement ,0
	mov ImmediateVar ,0
  ret
ResetTheData ENDP
	;-----------------------------------------------------------------------------------;
	; this proc will execute if the user enter 0 and need to close the program          ;
	; show to the user all registers                                                    ;
	; using RegisterValueArray to display data and some stored strings                  ;
	;-----------------------------------------------------------------------------------;
ShowTheRegisters PROC
	   mov esi ,offset RegisterValueArray
	   
	   mov edx ,offset ShowReg1
	   call writestring
	   mov eax,[esi]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg2
	   call writestring
	   mov eax,[esi + 4]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg3
	   call writestring
	   mov eax,[esi + 8]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg4
	   call writestring
	   mov eax,[esi + 12]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg5
	   call writestring
	   mov eax,[esi + 16]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg6
	   call writestring
	   mov eax,[esi + 20]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg7
	   call writestring
	   mov eax,[esi + 24]
	   call writedec
	   call crlf

	   mov edx ,offset ShowReg8
	   call writestring
	   mov eax,[esi + 28]
	   call writedec
	   call crlf

	   call FillRegister
	   

  ret
ShowTheRegisters ENDP
	;-------------------------------------------------------;
	; fill the registers by values from array .             ;
	;-------------------------------------------------------;
FillRegister PROC
	   mov esi ,offset RegisterValueArray
	   
	   mov eax,[esi]
	   mov ebx,[esi + 4]
	   mov ecx,[esi + 8]
	   mov edx,[esi + 12]
	   mov edi,[esi + 20]
	   ;mov esp,[esi + 24]
	   mov ebp,[esi + 28]
	   mov esi,[esi + 16]
  ret
FillRegister ENDP
    ;------------------------------------------------
	; Show the Warning and Give two Choice
	; retunes : eax  that has the char value
	;------------------------------------------------
Show_Warning PROC
      mov edx,offset Warning
	  call writestring
	  call crlf 
	  call readchar
	  call writechar
	  call crlf
  ret
Show_Warning ENDP
END main