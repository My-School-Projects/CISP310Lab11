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
	
	inputSentence	BYTE "my don dont do that", 0, 0	; this is sort of a hack,
														; but the first null is always skipped,
														; so we need an extra one to meet the exit condition
														; for the whileNotAtNull loop.

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

	; if the lengths of searchString and the current word are different, they are not equal.
	push edx						; we need an extra variable. save EDX for later
	lea edx, searchString
	push edx						; parameter: searchString
	call wordLength
	pop edx							; remove parameter
	
	mov edx, eax					; EDX = string1Length

	; get length of current word
	push ebx						; push the start of the current word as parameter: inputSentenceIndex
	call wordLength					; currentWordLength (EAX) = wordLength(inputSentenceIndex)
	pop ebx							; clear parameter off the stack
									
									; EDX = string2Length
	cmp eax, edx					; if (string1Length != string2Length), then
	pop edx							; (before we do anything, we need to restore EDX)
	jne wordsAreNotTheSame			; then the words are not the same.

	; compare current word to searchString
	mov ecx, eax					; repeatCount = currentWordLength
	cld								; set scan direction: left-to-right
	mov esi, ebx					; set source index to inputSentenceIndex
	lea edi, searchString			; set destination index to first element of searchString
	
	; repe cmpsb means:
	; while (ECX != 0)
	;	compare [ESI] and [EDI]
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

wordsAreNotTheSame:
	; if the words are not the same, copy the current word from inputSentence to outputSentence

	push eax						; parameter 3: length of word to copy
	push edx						; parameter 2: outputSentenceIndex (destination)
	push ebx						; parameter 1: inputSentenceIndex (source)

	call stringCopy					; copy current word into outputSentence

	pop ebx
	pop edx
	pop eax

	add edx, eax					; outputSentenceIndex += currentWordLength
	mov BYTE PTR[edx], ' '			; insert ' ' into outputSentence
	inc edx							; skip over space

	jmp endWordsAreTheSame
wordsAreTheSame:
	
	; if words are the same, copy replaceString into outputSentence

	; but first, we need to get the length of replaceString

	push eax						; save currentWordSize, because we need to keep the length of
									; source word, for when we increment inputSentenceIndex.

	lea ebp, replaceString			; get address of replaceString
	push ebp
	call wordLength					; EAX = wordLength(replaceString)
	pop ebp

	push eax						; parameter 3: length of word to copy
	push edx						; parameter 2: outputSentenceIndex (destination)
	
	push ebp						; parameter 1: address of replaceString (source)

	call stringCopy					; copy replaceString into outputSentence

	pop ebp							; pop parameters
	pop edx
	pop eax

	; currentWordSize (EAX) still refers to the size of replaceString.
	; increment outputSentenceIndex (EDX) by that amount (+ 1 for space)

	add edx, eax					; outputSentenceIndex += currentWordLength
	mov BYTE PTR[edx], ' '			; insert ' ' into outputSentence
	inc edx							; skip over space

	pop eax

	; currentWordSize (EAX) now refers to the size of the current word in inputSentence.
	; this is necessary because we are still going to increment inputSentenceIndex by that amount
	; (+ 1 for space)

endWordsAreTheSame:
	add ebx, eax					; inputSentenceIndex += currentWordLength
	inc ebx							; skip over space

	jmp whileNotAtNull				; loop back
endWhileNotAtNull:

	pop ebp							; restore ebp to avoid windows32 bug

	mov     eax, 0  ; exit with return code 0
	ret
_MainProc ENDP


; wordLength(stringAddr)
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


; stringCopy(sourceStringAddr, destStringAddr, lengthToCopy)
; copies a string from once place to another
stringCopy PROC
	push ebp						; save registers we're going to use
	mov ebp, esp					; copy stack pointer
	push esi
	push edi
	push ecx
	pushfd							; save flags register

	mov esi, DWORD PTR[ebp + 8]		; ESI = sourceStringAddr (1st parameter)
	mov edi, DWORD PTR[ebp + 12]	; EDI = destStringAddr (2nd parameter)
	mov ecx, DWORD PTR[ebp + 16]	; ECX = lengthToCopy (3rd parameter)

	cld								; set scan direction: left-to-right

	; rep movsb means:
	; while (ECX != 0)
	;	copy byte at [ESI] to byte at [EDI]
	;	increment ESI and EDI
	;	decrement ECX
	; end while

	rep movsb						; in short, copy lengthToCopy bytes from source string to dest. string

	popfd							; restore flags register
	pop ecx							; restore registers we used
	pop edi
	pop esi
	pop ebp
	ret
stringCopy ENDP

END   ; end of source code
