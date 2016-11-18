; General comments
; Author:  
; Date: 
; This is the Visual Studio 2012/Visual C++ Express Edition 2012 version   

; Preprocessor directives
.586		; use the 80586 set of instructions
.MODEL FLAT	; use the flat memory model (only 32 bit addresses, no segment:offset)

; External source files
INCLUDE io.h   ; header file for input/output

; Stack configuration
.STACK 4096	   ; allocate 4096 bytes for the stack

; Named memory allocation and initialization
.DATA
	
	inputSentence	BYTE "my don dont do that", 0

	searchString	BYTE "don", 0

	replaceString	BYTE "plate", 0
	
	outputSentence	BYTE 100 DUP (0)

; procedure definitions
.CODE
_MainProc PROC
	
	; while (not at null)
	;	if (current word == search word)
	;		copy replacement word into new sentence
	;	else
	;		copy current word into new sentence
	;	end if
	;	go to next word
	; end while

	push ebp				; save ebp to avoid windows32 bug

	lea ebx, replaceString
	push eax
	call stringLength
	pop ebx

	pop ebp					; restore ebp to avoid windows32 bug

	mov     eax, 0  ; exit with return code 0
	ret
_MainProc ENDP

; stringLength(stringAddr)
; returns the length of a null terminated string
stringLength PROC
	
	; set wordStart to stringAddr
	; set wordEnd to stringAddr
	; while (byte at wordEnd != 0)
	;	increment wordEnd
	; end while
	; return (wordEnd - wordStart)
	
	; ebx = wordStart
	; ecx = wordEnd

	push ebp						; save registers we're going to use
	mov ebp, esp					; copy stack index
	push ebx
	push ecx
	pushfd							; save flag register

	mov ebx, DWORD PTR [ebp + 8]	; move stringAddr into wordStart
	mov ecx, ebx					; wordEnd = wordStart
whileNotNull:						
	cmp BYTE PTR[ecx], 0			; wordEnd == NULL?
	je endWhileNotNull				; if so, end the while loop

	inc ecx							; if not, increment wordEnd
	jmp whileNotNull				; and repeat
endWhileNotNull:
	
	sub ecx, ebx					; stringLength = wordEnd - wordStart
	
	mov eax, ecx					; move stringLength into EAX for return value

	popfd							; restore flag register
	pop ecx							; restore registers we used
	pop ebx
	pop ebp
	ret
stringLength ENDP


END   ; end of source code
