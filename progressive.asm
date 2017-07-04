.progressivegame
	lda #1
	sta gameactive
	jsr DrawArena			;Draw arena - move into game mode subroutine
.newlevel	

	lda #50
	jsr pause

	jsr drawlevel						
	
	.progressivegameloop
		jsr vsync
	
		;ball in play?
		lda ballinplay
		cmp #0					;no - check for space bar to release ball
		bne inplay
			;get input
		ldx #&9d
		jsr inkey
		bne spacepressed
		jmp spacenotpressed
		.spacepressed
		lda #1
		sta ballinplay

	
		;yes - ball in play! erase,move and draw ball
		.inplay
			jsr moveballsprite
			jsr isballbelowbat
			\\ isballbelowbat sets a to 0 if above bat and 1 if below
			beq ballnotbelowbat
			jmp endofcollisiondetection
			.ballnotbelowbat
			
			
			
			jsr checkyaxis
			

			.endofcollisiondetection

	
	.spacenotpressed	
		jsr movebatsprite
	
	;get input
		jsr checkkeys
		lda gameactive
		beq gameend
		lda hitcount
		bne *+3
		jsr nextlevel
		jmp progressivegameloop
		.gameend
		jsr gameover
rts

.pause
	pha
	jsr vsync
	pla
	sec
	sbc #1
	cmp #0
	bne pause
rts

.gameover
	lda #11
	sta xpos
	lda #20
	sta ypos
	jsr Tabxy
	
	displaystring gameovertext
	
	\pause for 2 seconds = 100 vsyncs
	lda #100
	jsr pause
	
rts

.nextlevel
	\print "level complete"
	\tabxy?
	lda #13
	sta xpos
	lda #20
	sta ypos
	jsr Tabxy
	
	displaystring levelcompletetext
	
	\pause for 2 seconds = 100 vsyncs
	lda #200
	jsr pause
	
	\add 64 to leveladdress
	lda leveladdress
	clc 
	adc #64
	sta leveladdress
	
	lda leveladdress+1
	adc #0
	sta leveladdress+1
	
	cmp #>endoflevels
	bne dontlooplevels
	lda leveladdress
	cmp #<endoflevels
	bne dontlooplevels
	
	lda #<levels
	sta leveladdress
	lda #>levels
	sta leveladdress+1	


	.dontlooplevels
	
	
	
	
	setmode 1
	
	jsr DrawArena
	
	jsr drawlevel
	;lda #50
	;jsr pause
	lda #0
	sta ballinplay
	\reset ball somewhere sensible
	LDA #220
	sta bally
	lda #255
	sta ballyvel
rts

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\					movebatsprite
\\\\\\\		erase sprite, add bat movement to batx, plot sprite
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.movebatsprite
	ERASE BatSprite
	;MOVE BatSprite - batx += movebat
	lda batx
	clc
	adc movebat
	sta batx
	lda #0
	sta movebat
	PLOT BatSprite
rts

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\						moveballsprite
\\\\\\\\	erase,move and plot ball sprite
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.moveballsprite
	ERASE BallSprite		;ERASE BallSprite - obviously
	;MOVE BallSprite
	lda ballx				;add ballxvel to ballx to get new ball x position
	clc
	adc ballxvel
	sta ballx
	lda bally				;add ballyvel to bally to get new ball y position
	clc
	adc ballyvel
	sta bally
	PLOT BallSprite			;PLOT BallSprite - obviously!
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\\			isballbelowbat
\\\\\\\\\ check y pos of ball and if below bat then remove sprite,
\\\\\\\\\ reset ball position & direction, adjust lives & display,
\\\\\\\\\ set ballinplay to false. If lives have run out then check 
\\\\\\\\\ hiscore and end game
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.isballbelowbat
	lda baty
	clc
	adc #4
	cmp bally
	bne notbelowbat
	
	;reset ball position and wait for space to be pressed
	ERASE BallSprite		;ERASE BallSprite - obviously
	;decrement lives counter - if below 0 end game
	lda #20
	sta xpos
	lda #31
	sta ypos
	jsr Tabxy
	ldx lives
	inx
	stx lives
	txa
	jsr oswrch
	
	lda #3
	jsr sound
	
	lda #0
	sta ballinplay
	lda #20						;change to random value
	sta ballx
	lda #220					;change to random value
	sta bally
	lda #255 						;always 1 as ball moves down screen at upon release
	sta ballyvel
	
	ldx lives
	cpx #52
	bne stillalive
	jsr checkhiscore
	;delay?
	lda #0
	sta gameactive
	.stillalive
	\if ball is below bat set a to 1
	lda #1
	rts
	.notbelowbat
	\if ball is not below bat set a to 0
	lda #0
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\							checkxaxis
\\\\\\\\\\\							X AXIS COLLISION DETECTION								\\\\\\\\\\\\\\\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.checkxaxis
		lda ballxvel 
		cmp #1
		bne minusx  					;x is positive so ball is moving right
		
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\	check for collision at ballx+1,bally 
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		
		ldx ballx						;load ball x coord	
		inx							; increment ball x coord
		stx XOrd						;save ball x coord to XOrd
		lda bally						;load ball y coord
		sta YOrd						; save ball y coord to YOrd
		jsr ScreenStartAddress		;get memory address of ballx+1,bally result stored in &78,&79
		ldy #0
		lda (&78),Y					;load A with the value stored at the address stored at &78 + &79
		cmp #0						;is it black?
		bne hitwhite
		
		
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\ check for collision at ballx+1,bally+2
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		ldx ballx						;load ball x coord	
		inx							; increment ball x coord
		stx XOrd						;save ball x coord to XOrd
		ldx bally	;load ball y coord
		
		inx
		inx
		
		stx YOrd						; save ball y coord to YOrd
		jsr ScreenStartAddress		;get memory address of ballx+1,bally result stored in &78,&79
		ldy #0
		lda (&78),Y					;load A with the value stored at the address stored at &78 + &79
		cmp #0	
		bne hitwhite

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


		;beq	xblack					;no so hit something
		jmp xblack
		
		.hitwhite
		lda #&ff					;reverse direction of travel from heading right to heading left
		sta ballxvel
		
		;did it hit right of arena?
		lda ballx
		cmp #65
		beq hitarenax
		
		;is ball below bricks?
		lda bally
		cmp #140
		bpl xblack
		
		;did it hit a brick?
		
		;calculate x coordinate of brick column which contains balls x coord
		lda ballx
		sec
		sbc #13
		and #252
		clc
		adc #14
		sta brickx
		
		;calculate y coordinate of brick row which contains balls y coord
		jsr CalcYBrickRow
		
 		;ERASE BrickSprite
		jsr eraseBrickSprite
		jsr increasescore
		
		\decrement hitcount
		
		jmp xblack
		
		.minusx
		
		ldx ballx						;load ball x coord	
		dex							;decrement ball x coord
		stx XOrd						;save ball x coord to XOrd
		lda bally						;load ball y coord
		sta YOrd						;save ball y coord to YOrd
		jsr ScreenStartAddress		;get memory address of ballx-1,bally result stored in &78,&79
		ldy #0
		lda (&78),Y					;load A with the value stored at the address stored at &78 + &79
		cmp #0						;is it black?
		beq xblack
		lda #1						;if not black change x direction
		sta ballxvel
		;did it hit left of arena?
		lda ballx
		cmp #14
		beq hitarenax
		
		;is ball below bricks?
		lda bally
		cmp #140
		bpl xblack
		;did it hit a brick?
		
		;calculate x coordinate of brick column which contains balls x coord
		lda ballx
		sec
		sbc #15
		and #252
		clc
		adc #14
		sta brickx
		
		;calculate y coordinate of brick row which contains balls y coord
		jsr CalcYBrickRow
		
		;ERASE BrickSprite
		jsr eraseBrickSprite
		jsr increasescore
		
		\decrement hitcount
		
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


		.xblack
		rts
		.hitarenax
		lda #1
		jsr sound
		
rts

.CalcYBrickRow

		lda bally
		sec
		sbc #10
		and #248
		clc
		adc #10
		sta bricky

rts


.ballhitbat
	lda baty
	sec
	sbc #9
	cmp bally
	bpl *+4
	lda #0
	rts
	lda #1
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\\\\						checkyaxis
\\\\\\\\\\\							Y AXIS COLLISION DETECTION								\\\\\\\\\\\\\\\\
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.checkyaxis
		lda ballyvel 
		cmp #1
		bne minusy  					;y is positive so ball is moving down
				
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		
		
		;is the ball about to hit something white?
		lda bally						;load ball y coord	
		clc
		adc #3							; add 3 to ball y coord as it is 3 pixels high
		sta YOrd						;save ball y coord to YOrd
		lda ballx						;load ball x coord
		sta XOrd						; save ball x coord to XOrd
		jsr ScreenStartAddress		;get memory address of ballx,bally+3 result stored in &78,&79
		ldy #0
		lda (&78),Y					;load A with the value stored at the address stored at &78 + &79
		cmp #0						;is it black?
		beq	yblack					
		lda #&ff					;not black so reverse direction
		sta ballyvel
		;did it hit the bat?
		
		;is ball below bricks?
		lda bally
		cmp #140
		bpl hitbatsound
		
		
		;did it hit a brick?
		
		;calculate x coordinate of brick column which contains balls x coord
		lda ballx
		sec
		sbc #14
		and #252
		clc
		adc #14
		sta brickx
		
		;calculate y coordinate of brick row which contains balls y coord
		lda bally
		sec
		sbc #7
		and #248
		clc
		adc #10
		sta bricky
		
		;ERASE BrickSprite
		jsr eraseBrickSprite
		jsr increasescore
		
		\decrement hitcount
		
		jmp yblack
		
		.minusy
		
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\	check for collision at ballx,bally-1
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\		
		
		ldx bally						;load ball y coord	
		dex							;decrement ball y coord
		stx YOrd						;save ball y coord to YOrd
		lda ballx						;load ball x coord
		sta XOrd						;save ball x coord to YOrd
		jsr ScreenStartAddress		;get memory address of ballx,bally-1 result stored in &78,&79
		ldy #0
		lda (&78),Y					;load A with the value stored at the address stored at &78 + &79
		cmp #0						;is it black?
		beq yblack
		
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\		hit something
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\		
		
		lda #1						;if not black change x direction
		sta ballyvel
		
		;did it hit the top of the arena?
		lda bally 
		cmp #8
		beq hitarenay
		;did it hit a brick?
		;calculate x coordinate of brick column which contains balls x coord
		lda ballx
		sec
		sbc #14
		and #252
		clc
		adc #14
		sta brickx
		
		;calculate y coordinate of brick row which contains balls y coord
		lda YOrd
		sec
		sbc #9
		and #248
		clc
		adc #10
		sta bricky
		
		;ERASE BrickSprite
		jsr eraseBrickSprite
		jsr increasescore
		
		
		
		.yblack
		
		jsr checkxaxis
		
		rts
		.hitarenay
		lda #0
		jsr sound
		jsr checkxaxis
		rts
		.hitbatsound
		lda #1
		jsr sound
rts


\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\\\\\\\\					checkkeys
\\\\\\\\\\		check for key input and set movebat variable
\\\\\\\\\\		end game if escape is pressed
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

.checkkeys
	;key pressed = 'x', movebat = 1
		LDX #&bd
		JSR inkey
		BNE xpressed
		
		;key pressed = 'z', movebat = -1
		LDX #&9e
		JSR inkey
		BNE zpressed
		
		;key pressed = escape, end game.
		LDX #&8f
		JSR inkey
		BNE escpressed
		
		;end of key checks - jump to keyend
		jmp keyend
		
		.xpressed
		lda batx
		cmp #62
		beq keyend
		lda #1
		sta movebat
		jmp keyend
		
		.zpressed
		lda batx
		cmp #14
		beq keyend
		lda #255
		sta movebat
		jmp keyend
		
		.escpressed
		;set gameactive to 0
		lda #0
		sta gameactive
		jsr gameover
		.keyend
rts

