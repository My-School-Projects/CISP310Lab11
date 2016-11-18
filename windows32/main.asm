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
	
	outputSentence	BYTE 100 DUP (0)	; More than 100 characters seems like unreasonable input.
										; We're not writing a novel here.

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

	push ebp						; save ebp to avoid windows32 bug

	; eax = currentWordLength
	; ebx = inputSentenceIndex
	; ecx = repeatCount
	; edx = outputSentenceIndex

	lea ebx, inputSentence			; inputSentenceIndex = first element of inputSentence
	lea edx, outputSentence			; outputSentenceIndex = first element of outputSentence

whileNotAtNull:
	cmp BYTE PTR[ebx], 0			; if (inputSentenceIndex == NULL)
	je endWhileNotAtNull			; then break the loop

	; get length of current word
	push ebx						; push the start of the current word as parameter: inputSentenceAddr
	call wordLength					; currentWordLength (EAX) = wordLength(inputSentenceAddr)
	pop ebx							; clear parameter off the stack

	; compare current word to searchString
	mov ecx, eax					; repeatCount = currentWordLength
	cld								; set scan direction: left-to-right
	mov esi, ebx					; set source index to inputSentenceIndex
	lea edi, searchString			; set destination index to first element of searchString
	
	; repe cmps means:
	; while
	;	if ECX = 0, then break loop
	;	compare [ESI] and [EDI],
	;	increment ESI and EDI
	;	decrement ECX
	;	if [ESI] and [EDI] are the same, then break loop
	; end while
	repe cmpsb						; in summary, this will compare the searchString to the current word
									; in inputSentence (starting at inputSentenceIndex).
									; if ECX == 0, that means that the loop did not break early,
									; which means that the two words are the same.
	
	cmp ecx, 0						; is ECX 0?
	je wordsAreTheSame				; if (ECX == 0), then the words are the same

	; TODO if the words are not the same, copy the current word from inputSentence to outputSentence

	jmp endWordsAreTheSame
wordsAreTheSame:
	
	; TODO if words are the same, copy replaceString into outputSentence

endWordsAreTheSame:
	add ebx, eax					; inputSentenceIndex += currentWordLength
	inc ebx							; skip over space

	jmp whileNotAtNull				; loop back
endWhileNotAtNull:
	

	pop ebp							; restore ebp to avoid windows32 bug

	mov     eax, 0  ; exit with return code 0
	ret
_MainProc ENDP

; stringLength(stringAddr)
; returns the length of a null terminated string
wordLength PROC
	
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
whileNotWordEnd:						
	cmp BYTE PTR[ecx], 0			; wordEnd == NULL?
	je endWhileNotWordEnd			; if so, end the while loop
	cmp BYTE PTR[ecx], ' '			; wordEnd == ' '?
	je endWhileNotWordEnd			; if so, end the while loop

	inc ecx							; if not, increment wordEnd
	jmp whileNotWordEnd				; and repeat
endWhileNotWordEnd:
	
	sub ecx, ebx					; stringLength = wordEnd - wordStart
	
	mov eax, ecx					; move stringLength into EAX for return value

	popfd							; restore flag register
	pop ecx							; restore registers we used
	pop ebx
	pop ebp
	ret
wordLength ENDP


END   ; end of source code
