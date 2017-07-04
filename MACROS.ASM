\\\\\\\\\\\\\\\\\\\\\\\\\  
\\\\\    Macros      \\\\
\\\\\\\\\\\\\\\\\\\\\\\\\

MACRO displaystring out
	lda #<out 
	sta printloop+1
	lda #>out
	sta printloop+2
	jsr printouttext
ENDMACRO

MACRO setmode mode 
   lda #$16
   jsr oswrch
   lda #mode
   jsr oswrch
ENDMACRO

MACRO PLOT SP
	lda #<SP
	sta $70
	lda #>SP
	sta $71
	jsr GetScreenAddress
	jsr PlotSprite
ENDMACRO

MACRO ERASE SP2
	lda #<SP2
	sta $70
	lda #>SP2
	sta $71
	jsr GetScreenAddress
	jsr EraseSprite
ENDMACRO


MACRO Times8 P1, P2
   ; multiply contents of param1 and add result to
   ; contents of param1 and param1+1
   ; uses 9F as scratchpad space
   ; A corrupted, param1 corrupted
   lda #0
   sta $9F    ; clear out hi part of result 
   clc
   asl P1
   rol $9F
   asl P1
   rol $9F
   asl P1
   rol $9F
   ; ok got result of multiplication, now add to param2 contents
   lda P1
   clc
   adc P2
   sta P2
   lda $9F
   adc P2+1
   sta P2+1
ENDMACRO