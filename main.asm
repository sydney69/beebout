\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\	Beebout - a Simple Breakout game
\	Written by Simon Hooper
\	(RE)Started sometime in May/June 2015
\	Last updated 22/7/15
\
\	TO DO:
\		
\		check collision detection
\		loading screen
\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\      Acorn Constants    \\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

oswrch = $ffee
osbyte = &fff4
osword = &fff1
 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\      Other Constants    \\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Ball = 0
Bat = 1
Brick = 2

black = 0
red = 1
green = 2
yellow = 3
blue = 4
magenta = 5;pink
cyan = 6;light blue
white = 7


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\      Variables          \\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
SpriteObjectStructure = $70
SpriteGraphicStructure = $72
XOrd = $74
YOrd = $75
Width = $76
Height = $77
YScreenAddress = $78 ; and 79
SpritePixel = $7A    
XStartOffset = $7B ; remember X offset start, which needs to be the same at start of every new column
leveladdress =$7D
ScreenStartHighByte = $30
 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
INCLUDE "MACROS.ASM"
 
\\\\\\\\\\\\\\\\\\\
\\\\     Main   \\\
\\\\\\\\\\\\\\\\\\\

ORG $1900 

.start
	jsr AttractScreen
	jsr initialisegame
	.whichgame
		jsr selectgame
		lda gametype
		cmp #3
		bne whichgame
		jsr progressivegame
	.fin
	jmp start
rts
	
;______________________________________________________________________
;						subroutines
;_____________________________________________________________________



\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\			Attract Screen
\\\\\\\\	prints a nice mode 7 menu screen before starting a game
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.AttractScreen
\mode 7
	setmode 7
	;jsr CsrOff
\display text
	displaystring text1
	displaystring text2
\nice screen transition
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\				initialisegame
\\\\\\\\	set all variables to the correct starting values
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.initialisegame

	lda #0
	sta gametype		;set gametype to 0
	sta ScoreHI			;set score to 0
	sta ScoreLO
	sta ballinplay
	
	;change lives so 3 is printed and decremented when a life is lost
	lda #49		;set lives to 49 - ascii for 1
	sta lives
	
\put address of level 0 data into leveladdress	
	lda #<levels
	sta leveladdress
	lda #>levels
	sta leveladdress+1	
	

\ get random number between 0 and 7
	jsr random
	lsr a			\divide by 2
	lsr a			\divide by 4
	lsr a			\divide by 8
	lsr a			\divide by 16
	lsr a			\divide by 32
	
\	sec
\	sbc #1
	tax
	cpx #7
	bne leveljump
	rts
	
	.leveljump
	
	\add 64 to leveladdress
	lda leveladdress
	clc 
	adc #64
	sta leveladdress
	
	lda leveladdress+1
	adc #0
	sta leveladdress+1
	dex
	bne leveljump
	rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\						selectgame
\\\\\\\		loops until a game type is chosen
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
.selectgame
;get input
		lda #0
		sta gametype
		;key pressed = 'd'
		LDX #&cd
		JSR inkey
		BNE dpressed
		
		;key pressed = 'c'
		LDX #&ad
		JSR inkey
		BNE cpressed
		
		;key pressed = 'p'
		LDX #&c8
		JSR inkey
		BNE ppressed
		
		;end of key checks - jump to selectend
		jmp selectend
		
		.dpressed
		lda #1
		sta gametype
		jmp selectend
		
		.cpressed
		lda #2
		sta gametype
		jmp selectend
		
		.ppressed
		lda #3
		sta gametype
		jmp selectend
		
		.selectend
		
	rts



\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\					progressivegame
\\\\\\\		main routine for progressive game
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

INCLUDE "progressive.asm"



\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\	bodge - remove asap!
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.eraseBrickSprite
	lda #2
	jsr sound
	ERASE BrickSprite
rts
 

	
.DrawArena
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\ 					draw arena
\\\		draw the arena walls and roof and display lives,score and hiscore       
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;use the bat sprite to create the arena
	setmode 1				;use setmode macro to change mode to 1

		;top of arena
		lda #64					;set batx to 64
		sta batx
		lda #2					;set baty to 2
		sta baty
		
		.arenaloop
		PLOT BatSprite
		dec batx
		lda batx
		cmp #11
		bne arenaloop 				;if batx>11 loop
		
		;top of arena done
		
		lda #2 					;temporarily change sprite width to 2 to draw arena sides
		sta BatSpriteData
		
		lda #8					;set bat y position to 8
		sta baty
		
		.arenaloop2
		lda #66
		sta batx
		PLOT BatSprite
		lda #12
		sta batx
		PLOT BatSprite
		inc baty
		lda baty
		cmp #251
		bne arenaloop2
		
		lda #4					;return sprite width to 4 after drawing arena sides
		sta BatSpriteData
		lda #40					;return batx to 40 and baty to 232
		sta batx
		lda #232
		sta baty
		
		;display lives
		jsr displaylives
		;display score
		jsr displayscore
		jsr displayhiscore
rts

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\					drawlevel
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
.drawlevel

	lda #220
	sta bally
	lda #&ff
	sta ballyvel
	;ldy #0				;0 to 7 hold the password for the level
	;print level password for 3 seconds then erase then draw level
	
	ldy #8				;load y with #8
	
	
	;levels,y needs to be replaced with leveladdress which is calculated thusly:
	; levels + level number * 64
	
	ldx #1
	jsr setpalette		;change colour 1 to colour of brick 2
	iny
	
	ldx #2
	jsr setpalette		;change colour 2 to colour of brick 3
	iny
	
	;lda levels,y
	lda (leveladdress),y
	sta hitcount		;number of bricks in level -1 each time a brick is hit
	iny

	;
	
	lda #14				;load accumilator with #14
	sta temp			;store accumilator in temp
	sta brickx			;store accumilator in brickx
	
	lda #10				;load accumilator with #10
	sta bricky			;store accumilator in bricky
	
	
	.nextcolumn
	;lda levels,y		;a how holds value of 1st 4 bricks in column 1
	lda (leveladdress),y
	sta tempbyte
	.getnextwallbyte
	ldx #4				;count down 4 bricks
	.maskbits
	and #&03			;only 2 rightmost bits of a left
	
	
	beq	dontplotbrick	;if zero do not plot as it's black
	clc
	adc #1				;add 1 as sprites 2,3 and 4 are bricks
	sta BrickSprite		;changes bricksprite to value stored in a
	sty tempy
	stx tempx
	PLOT BrickSprite 	;plot the brick sprite
	ldy tempy
	ldx tempx
	.dontplotbrick
	lda bricky			;load a with bricky
	cmp #130			;is it == 130	
	beq finishedcolumn	;yes! column finished
	clc					;no! add 8 to bricky
	ADC #8
	sta bricky
	
	;lda levels,y
	lda (leveladdress),y
	lsr a
	lsr a
	;sta levels,y
	sta (leveladdress),y
	dex					;decrement brick counter
	bne maskbits
	lda tempbyte
	;sta levels,y
	sta (leveladdress),y
	iny
	;lda levels,y
	lda (leveladdress),y
	sta tempbyte
	jmp getnextwallbyte
	
	.finishedcolumn
	lda brickx
	clc	
	adc #4
	sta brickx
	cmp #66
	beq wallsend
	lda #10
	sta bricky
	lda tempbyte
	;sta levels,y
	sta(leveladdress),y
	;sta tempbyte
	iny
	jmp nextcolumn
	.wallsend
	lda tempbyte
	sta(leveladdress),y
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\\				setpalette
\\\\\\\\\\\\	changes the palette entries for colour 1 and 2 to 
\\\\\\\\\\\\	those defined in the level data
\\\\\\\\\\\\	a holds the physical colour number,x the logical colour number
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.setpalette
	lda #19
	jsr oswrch
	txa
	jsr oswrch
	;lda levels,y
	lda (leveladdress),y
	jsr oswrch
	lda #0
	jsr oswrch
	jsr oswrch
	jsr oswrch
rts

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\					display lives
\\\\\		prints the lives at the correct poistion at the bottom 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
.displaylives
	lda #20
	sta xpos
	lda #31 
	sta ypos
	jsr Tabxy
	lda lives
	jsr oswrch
rts

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\					display hiscore
\\\\\		prints the hiscore at the correct poistion at the bottom 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.displayhiscore
	lda #27
	sta xpos
	lda #31
	sta ypos
	jsr Tabxy
	lda HIScoreHI
	jsr BCDtoScreen
	lda HIScoreLO
	jsr BCDtoScreen
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\					display score
\\\\\		prints the score at the correct poistion at the bottom 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.displayscore
	lda #10
	sta xpos
	lda #31
	sta ypos
	jsr Tabxy
	lda ScoreHI
	jsr BCDtoScreen
	lda ScoreLO
	jsr BCDtoScreen
rts
	

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\				printouttext
\\\\\\\\	displaystring macro changes 0000 to address of string
\\\\\\\\	which is then prited to the screen using oswrch
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.printouttext
	ldy #0
	.printloop
		lda 0000,y 		;modified by printstring macro
		cmp #&ff
		beq finishedprint
		jsr oswrch
		iny
		jmp printloop
	.finishedprint
rts

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\						inkey
\\\\\\\\	;use osbyte to get keyboard input
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.inkey							
	PHA
	TYA
	PHA
	LDY #&ff
	LDA #&81
	JSR osbyte
	PLA
	TAY
	PLA
	CPX #&00
RTS


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\					checkhiscore
\\\\\\\\	if score is greater than hiscore replace hiscore 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.checkhiscore
	lda ScoreHI
	cmp HIScoreHI
	beq HIequal		;HI bytes of scores are equal so lo byte is relevant
	bpl newhi		;score-hiscore is positive or zero so new hi score
	rts				;neither of above so not a new hi score
	.HIequal
	lda ScoreLO
	cmp HIScoreLO
	bpl newhi		;score-hiscore is positive or zero so new hiscore
	rts
	.newhi
	lda ScoreHI
	sta HIScoreHI
	lda ScoreLO
	sta HIScoreLO
	;sound effect?
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\						vsync
\\\\\\\\\	wait for vsync 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.vsync
	lda #&13
	jsr osbyte
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\				increase score
\\\\\\\\\\		add points to score - will depend on bally eventually
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.increasescore
		\decrement hitcount
		ldx hitcount 
		dex
		stx hitcount
		SED             ;Set decimal mode flag
        CLC             ;Ensure carry is clear
        LDA ScoreLO       ;Add the two least significant bytes
        ADC #1
        STA ScoreLO       ;... and store the result
        LDA ScoreHI      ;Add the two most significant bytes
        ADC #0     ;... and any propagated carry bit
        STA ScoreHI    ;... and store the result
        CLD             ;Clear decimal mode
		jsr displayscore
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\				sound
\\\\\\\\\\		play the sound indexed by the value in a
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
.sound
	asl a
	asl a
	asl a
	adc #<soundbuffer
	tax
	ldy #>soundbuffer
	bcc nohibyte
	iny
.nohibyte
	lda #&07
	jsr osword
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\							tabxy
\\\\\\\		equiv of BASIC "TAB" . entry params contents of xpos ypos
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.Tabxy  ; 
	 LDA #31
	 JSR &FFEE
	 LDA xpos
	 JSR &FFEE
	 LDA ypos
	 JSR &FFEE
 RTS


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\			bcdtoscreen
\\\\\\	prints the two decimal values stored in a bcd byte
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
 .BCDtoScreen 
		pha ;Save the BCD value
        LSR A           ;Shift the four most significant bits
        LSR A           ;... into the four least significant
        LSR A
        LSR A
        ORA #&30        ;Make an ASCII digit
        JSR oswrch     ;... and print it
        PLA             ;Recover the BCD value
        AND #$0F        ;Mask out all but the bottom 4 bits
        ORA #&30        ;Make an ASCII digit
		JSR oswrch       ;... and print it
rts

.random
	LDA seed
	and #&48
	adc #&38
	asl a
	asl a
	rol seed+2
	rol seed+1
	rol seed
	lda seed
rts

.CsrOff
 lda #23
  jsr oswrch
  lda #1
  jsr oswrch
  lda #0
  ldx #7
  .CursorOffLoop
	  jsr oswrch
	  dex
  bpl CursorOffLoop
rts


INCLUDE "sprite-plot-erase.asm" 

;______________________________________________________________________
;								end of code	
;_______________________________________________________________________
 
 .xpos
	EQUB 0
 .ypos
	EQUB 0
	
.ScoreHI
	EQUB 0
.ScoreLO
	EQUB 0
	
.HIScoreHI
	EQUB 0
.HIScoreLO
	EQUB 0
	
.seed
	equb &35,&69,&e4
	
 \ball variables
.BallSprite
	EQUB Ball
.ballx
	EQUB 20
.bally
	EQUB 120 
.ballxvel
	equb 1
.ballyvel
	equb 255
.ballinplay
	EQUB 0

\Bat variables	
.BatSprite
	EQUB Bat
.batx
	EQUB 7
.baty
	EQUB 232
.movebat		
	equb 0

\Brick variables
.BrickSprite
	EQUB Brick
.brickx
	EQUB 0
.bricky
	EQUB 0
	
\other variables
.temp
	equb 0
.tempy
	equb 0
.tempx
	equb 0
.tempbyte
	equb 0
.gametype
	equb 0
.lives
	equb 3
.gameactive
	equb 0
.hitcount
	equb 0
	

 
 

\\PUT SPRITE DATA IN HERE 
.GameSprites
	equb LO(BallSpriteData-GameSprites)
	equb HI(BallSpriteData-GameSprites)
	equb LO(BatSpriteData-GameSprites)
	equb HI(BatSpriteData-GameSprites)
	equb LO(BrickSprite1Data-GameSprites)
	equb HI(BrickSprite1Data-GameSprites)
	equb LO(BrickSprite2Data-GameSprites)
	equb HI(BrickSprite2Data-GameSprites)
	equb LO(BrickSprite3Data-GameSprites)
	equb HI(BrickSprite3Data-GameSprites)
	
.BallSpriteData
	equb 1,3,&77,&77,&77
.BatSpriteData
	EQUB 4,6
	EQUB &33,&33,&33,&33,&33,&33	;1st column
	EQUB &ff,&ff,&ff,&ff,&ff,&ff	;2nd column
	EQUB &ff,&ff,&ff,&ff,&ff,&ff	;3rd column
	EQUB &ff,&ff,&ff,&ff,&ff,&ff	;4th column
.BrickSprite1Data
	EQUB 4,6
	EQUB &33,&33,&33,&33,&33,&33	;1st column
	EQUB &ff,&ff,&ff,&ff,&ff,&ff	;2nd column
	EQUB &ff,&ff,&ff,&ff,&ff,&ff	;3rd column
	EQUB &ff,&ff,&ff,&ff,&ff,&ff	;4th column
.BrickSprite2Data
	EQUB 4,6
	EQUB &03,&03,&03,&03,&03,&03	;1st column
	EQUB &0f,&0f,&0f,&0f,&0f,&0f	;2nd column
	EQUB &0f,&0f,&0f,&0f,&0f,&0f	;3rd column
	EQUB &0f,&0f,&0f,&0f,&0f,&0f	;4th column
.BrickSprite3Data
	EQUB 4,6
	EQUB &30,&30,&30,&30,&30,&30	;1st column
	EQUB &f0,&f0,&f0,&f0,&f0,&f0	;2nd column
	EQUB &f0,&f0,&f0,&f0,&f0,&f0	;3rd column
	EQUB &f0,&f0,&f0,&f0,&f0,&f0	;4th column
	
	
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\					end of sprite data							\\\\\\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.soundbuffer
	EQUW	1,-10,100,3	;arena
	EQUW	2,-10,150,3	;BAT
	EQUW	2,-10,200,3	;BRICK
	EQUW	3,-10,0,10	;LOSE LIFE
	
.text1
	;EQUS "1234567890123456789012345678901234567890"
	EQUS 13,10
	EQUS 129,141,"              BEEBOUT",13,10
	EQUS 129,141,"              BEEBOUT",13,10
	EQUS 13,10,13,10
	EQUS 130,"              CONTROLS",13,10,13,10,13,10
	EQUS 131,"    P - PLAY GAME",13,10
	EQUS 131,"    SPACE BAR - LAUNCH BALL",13,10
	EQUS 131,"    Z - MOVE BAT LEFT",13,10
	EQUS 131,"    X - MOVE BAT RIGHT",13,10,13,10
	EQUS 131,"    ESCAPE - QUIT GAME",13,10,13,10,13,10,&ff
.text2
	EQUS 132,"THERE ARE A TOTAL OF EIGHT LEVELS TO",13,10
	EQUS 132,"PLAY. YOU START ON A RANDOM LEVEL",13,10
	EQUS 132,"THEN PROGRESS THROUGH THEM IN ",13,10
	EQUS 132,"NUMERICAL ORDER",&ff
.levelcompletetext
	EQUS "LEVEL COMPLETE",&ff
	
.gameovertext
	equs "G A M E  O V E R !",&ff
	
.LookUp640
	INCBIN "LookUpTable640.bin"

	
	;each level will consist of a password,brick2 colour, brick3 colour, number of bricks, followed by 16*13 = 208 bytes of brick data
.levels
	equs "BREAKOUT"	;8
	equb green ;9
	equb blue ;10
	equb 104	;11
	equb &ff,&00,&aa,&00 ;13*4=52;+11=63bytes per level
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &ff,&00,&aa,&00
	equb &00 ; filler to make each level 64 bytes!
.level2
	equs "DIAGONAL"	;8
	equb red ;9
	equb yellow ;10
	equb 208	;11
	equb &db,&b6,&6d,&db
	equb &b6,&6d,&db,&b6
	equb &6d,&db,&b6,&6d
	equb &db,&b6,&6d,&db
	equb &b6,&6d,&db,&b6
	equb &6d,&db,&b6,&6d
	equb &db,&b6,&6d,&db
	equb &b6,&6d,&db,&b6
	equb &6d,&db,&b6,&6d
	equb &db,&b6,&6d,&db
	equb &b6,&6d,&db,&b6
	equb &6d,&db,&b6,&6d
	equb &db,&b6,&6d,&db
	equb &00 ; filler to make each level 64 bytes!
.level3
	equs "SAUCER  "	;8
	equb red ;9
	equb yellow ;10
	equb 93	;11 58+35
	EQUB &C0,&03,&00,&00
	EQUB &F0,&0F,&00,&A0
	EQUB &3C,&3F,&00,&A8
	EQUB &FC,&0F,&00,&A8
	EQUB &3F,&03,&00,&A8
	EQUB &FF,&0F,&00,&AA
	EQUB &3F,&3F,&80,&AA
	EQUB &FF,&0F,&00,&AA
	EQUB &3F,&03,&00,&A8
	EQUB &FC,&0F,&00,&A8
	EQUB &3C,&3F,&00,&A8
	EQUB &F0,&0F,&00,&A0
	EQUB &C0,&03,&00,&00
	equb &00 ; filler to make each level 64 bytes!
.level4
	equs "INVADER "	;8
	equb blue ;9
	equb green ;10
	equb 94	;11  46+48=94
	EQUB &00,&00,&00,&00
	EQUB &FC,&03,&00,&2A
	EQUB &C0,&CF,&80,&02
	EQUB &F3,&3F,&A2,&2A
	EQUB &3C,&0F,&28,&8A
	EQUB &FC,&0F,&A0,&8A
	EQUB &FC,&0F,&A0,&0A	
	EQUB &FC,&0F,&A0,&8A
	EQUB &3C,&0F,&28,&8A
	EQUB &F3,&3F,&A2,&2A
	EQUB &C0,&CF,&80,&02
	EQUB &FC,&03,&00,&2A
	EQUB &00,&00,&00,&00
	EQUB &00
.level5
	equs "ZOMBIE  "	;8
	equb blue ;9
	equb cyan ;10
	equb 106	;11
	EQUB &00,&00,&00,&00
	EQUB &00,&00,&00,&00
	EQUB &00,&5F,&15,&00
	EQUB &00,&5F,&15,&00
	EQUB &55,&FF,&AF,&AA
	EQUB &51,&FF,&AF,&AA
	EQUB &55,&FD,&AF,&AA
	EQUB &51,&FF,&AF,&AA
	EQUB &55,&FF,&AF,&AA
	EQUB &00,&5F,&15,&00
	EQUB &00,&5F,&15,&00
	EQUB &00,&00,&00,&00
	EQUB &00,&00,&00,&00
	EQUB &00
.level6
	equs "SHEEP   "	;8
	equb magenta ;9
	equb magenta ;10
	equb 115	;11   39+7+45+24
	EQUB &54,&01,&00,&00
	EQUB &55,&01,&00,&00
	EQUB &55,&01,&00,&00
	EQUB &55,&01,&00,&00
	EQUB &50,&55,&05,&00
	EQUB &50,&55,&55,&A9
	EQUB &50,&55,&55,&A9
	EQUB &50,&55,&05,&00
	EQUB &50,&55,&05,&00
	EQUB &50,&55,&05,&00
	EQUB &50,&55,&55,&A9
	EQUB &50,&55,&55,&A9
	EQUB &50,&55,&05,&00
	EQUB &00
.level7
	equs "SCOTLAND"	;8
	equb blue ;9
	equb blue ;10
	equb 169	;11
	EQUB &A5,&AA,&6A,&01
	EQUB &95,&AA,&5A,&01
	EQUB &56,&AA,&56,&02
	EQUB &5A,&A9,&95,&02
	EQUB &6A,&65,&A5,&02
	EQUB &AA,&55,&A9,&02
	EQUB &AA,&56,&AA,&02
	EQUB &AA,&55,&A9,&02
	EQUB &6A,&65,&A5,&02
	EQUB &5A,&A9,&95,&02
	EQUB &56,&AA,&56,&02
	EQUB &95,&AA,&5A,&01
	EQUB &A5,&AA,&6A,&01
	EQUB &00
.level8
	equs "BRITAIN "	;8
	equb red ;9
	equb blue ;10
	equb 208	;11
	EQUB &D9,&9F,&F6,&67
	EQUB &67,&9F,&F6,&D9
	EQUB &9F,&9D,&76,&F6
	EQUB &7F,&96,&96,&FD
	EQUB &FF,&99,&66,&FF
	EQUB &55,&95,&56,&55
	EQUB &AA,&AA,&AA,&AA
	EQUB &55,&95,&56,&55
	EQUB &FF,&99,&66,&FF
	EQUB &7F,&96,&96,&FD
	EQUB &9F,&9D,&76,&F6
	EQUB &67,&9F,&F6,&D9
	EQUB &D9,&9F,&F6,&67
	EQUB &00
.endoflevels

	


	
.end
SAVE "MAIN",start,end