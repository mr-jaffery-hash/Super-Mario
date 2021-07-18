; clear the screen
[org 0x0100]
jmp start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
jumpDelay:
	push cx
	push ax
	
	mov al, 0x00	
	in al, 0x60							; read a char from keyboard port
	
	cmp al, 0x1E
	jne notDelayLeft
	call clearMario
	sub word [cs:marioLocation],2
	call detectCoinSackOne
	call detectCoinSackTwo
	call detectCoinSackThree	
	call detectCollision
	cmp word [cs:locationCheckFlag], 1
	jne leftCollisionNotDetectedInDelay
	mov word [cs:locationCheckFlag],0
	call clearMario
	add word [cs:marioLocation],2
	
leftCollisionNotDetectedInDelay:	
notDelayLeft:	
	cmp al, 0x20
	jne notDelayRight
	call clearMario
	add word [cs:marioLocation],2
	call detectCoinSackOne
	call detectCoinSackTwo
	call detectCoinSackThree
	call detectCollision
	cmp word [cs:locationCheckFlag], 1
	jne RightCollisionNotDetectedInDelay
	mov word [cs:locationCheckFlag],0
	call clearMario
	sub word [cs:marioLocation],2

RightCollisionNotDetectedInDelay:	
notDelayRight:

	mov cx, 0xffff
delayLoop:
	loop delayLoop
	
	pop ax
	pop cx
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
detectCollision:
	push ax
	push cx
	push si
	push bx
	push dx
	push di
	push bp
	
	mov dx, [cs:enemyOneLocation]
	mov di, [cs:enemyTwoLocation]
	add word dx,2
	mov word [cs:enemyOneLocation+2], dx
	add word dx,2
	mov word [cs:enemyOneLocation+4], dx
	add word dx,160
	mov word [cs:enemyOneLocation+6], dx
	sub word dx,2
	mov word [cs:enemyOneLocation+8], dx
	sub word dx,2
	mov word [cs:enemyOneLocation+10], dx
	add word dx,160
	mov word [cs:enemyOneLocation+12], dx
	add word dx,2
	mov word [cs:enemyOneLocation+14], dx
	add word dx,2
	mov word [cs:enemyOneLocation+16], dx	
	
	add word di,2
	mov word [cs:enemyTwoLocation+2], di
	add word di,2
	mov word [cs:enemyTwoLocation+4], di
	add word di,160
	mov word [cs:enemyTwoLocation+6], di
	sub word di,2
	mov word [cs:enemyTwoLocation+8], di
	sub word di,2
	mov word [cs:enemyTwoLocation+10], di
	add word di,160
    mov word [cs:enemyTwoLocation+12], di
	add word di,2
	mov word [cs:enemyTwoLocation+14], di
	add word di,2
	mov word [cs:enemyTwoLocation+16], di	
	
	mov ax, [cs:marioLocation]
	add ax, 320
	mov word [cs:marioLeftLegLocation], ax
	mov word [cs:marioRightLegLocation], ax
	add word [cs:marioRightLegLocation], 4
	mov si, 0
	mov bx, hurdleLocation
	mov cx, 12
		
detectHurdlesLoop:
	mov word ax, [cs:marioRightLegLocation]
	cmp word ax, [bx+si]
	je collisionDetected
	mov word ax, [cs:marioLeftLegLocation]
	cmp word ax, [bx+si]
	je collisionDetected	
	add si, 2
	loop detectHurdlesLoop
	
	mov si, 0
	mov di, 0
	mov bx, enemyOneLocation
	mov bp, enemyTwoLocation
	mov cx, 9

detectEnemyLoop:
	mov word ax, [cs:marioRightLegLocation]
	cmp word ax, [bx+di]
	je enemeyCollisionDetected
	cmp word ax, [bp+si]
	je enemeyCollisionDetected
	mov word ax, [cs:marioLeftLegLocation]
	cmp word ax, [bx+di]
	je enemeyCollisionDetected
	cmp word ax, [bp+si]
	je enemeyCollisionDetected
	add si, 2
	add di,2
	loop detectEnemyLoop
	
	jmp endCollisionFunction
	
enemeyCollisionDetected:
	mov byte [cs:marioLife], 0
	jmp endCollisionFunction
	
collisionDetected:
	mov word [cs:locationCheckFlag],1

endCollisionFunction:
	pop bp
	pop di
	pop dx
	pop bx
	pop si
	pop cx
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delay:
	push cx
	mov cx, 0xffff
loopTimeDelay
	loop loopTimeDelay
	pop cx
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
detectCoinSackOne:
	push cx
	cmp word [cs:coinSackOneCheck], 5
	jae endDetectCoinSackOneFunction
	mov word cx, [cs:marioLocation]
	cmp word cx, 3216
	je coinSackOneDetected
	cmp word cx, 3218
	je coinSackOneDetected
	add cx, 4
	cmp word cx, 3216
	je coinSackOneDetected
	cmp word cx, 3218
	je coinSackOneDetected
	jmp endDetectCoinSackOneFunction
coinSackOneDetected:
	add word [cs:score], 5
	add word [cs:coinSackOneCheck], 1
	call clearMario
	add word [cs:marioLocation], 160
	call drawMario
endDetectCoinSackOneFunction	
	pop cx
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
detectCoinSackTwo:
	push cx
	cmp word [cs:coinSackTwoCheck], 5
	jae endDetectCoinSackTwoFunction
	mov word cx, [cs:marioLocation]
	cmp word cx, 3330
	je coinSackTwoDetected
	cmp word cx, 3332
	je coinSackTwoDetected
	add cx, 4
	cmp word cx, 3330
	je coinSackTwoDetected
	cmp word cx, 3332
	je coinSackTwoDetected
	jmp endDetectCoinSackTwoFunction
coinSackTwoDetected:
	add word [cs:score], 5
	add word [cs:coinSackTwoCheck], 1
	call clearMario
	add word [cs:marioLocation], 160
	call drawMario
endDetectCoinSackTwoFunction	
	pop cx
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
detectCoinSackThree:
	push cx
	cmp word [cs:coinSackThreeCheck], 5
	jae endDetectCoinSackThreeFunction
	mov word cx, [cs:marioLocation]
	cmp word cx, 2960
	je coinSackThreeDetected
	cmp word cx, 2962
	je coinSackThreeDetected
	add cx, 4
	cmp word cx, 2960
	je coinSackThreeDetected
	cmp word cx, 2962
	je coinSackThreeDetected
	jmp endDetectCoinSackThreeFunction
coinSackThreeDetected:
	add word [cs:score], 5
	add word [cs:coinSackThreeCheck], 1
	call clearMario
	add word [cs:marioLocation], 160
	call drawMario
endDetectCoinSackThreeFunction	
	pop cx
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
Blue:
	push es
	push ax
	push cx
	push di

	mov ax, 0xb800
	mov es, ax ; point es to video base
	xor di, di ; point di to top left column
	mov ax, 0x3020 ; space char in normal attribute
	mov cx, 2000 ; number of screen locations
	cld ; auto increment mode
	rep stosw ; clear the whole screen

	pop di 
	pop cx
	pop ax
	pop es
	ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawBlueBorder:
	pusha
	
		mov ax, 0xb800
		mov es, ax
		
	 	mov di,0
startBorderUp:
		mov word[es:di],0x1020
		add di,2
		cmp di,160
		jne startBorderUp
		
		mov di,3840
startBorderDown:
		mov word[es:di],0x1020
		add di,2
		cmp di,4000
		jne startBorderDown
		
		mov di,318
startBorderRight:
		mov word[es:di],0x1020
		add di,160
		cmp di,3998
		jne startBorderRight
		
		mov di,160
startBorderLeft:
		mov word[es:di],0x1020
		add di,160
		cmp di,3840
		jne startBorderLeft
		
		mov di,316
		startBorderRight1:
		mov word[es:di],0x1020
		add di,160
		cmp di,3996
		jne startBorderRight1
		
		mov di,162
startBorderLeft2:
		mov word[es:di],0x1020
		add di,160
		cmp di,3842
		jne startBorderLeft2
		
		popa 
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
startScreen:
	startGameScreen:
		push ax
		push dx
		push es
		push di
		push si
		
		;push 0x1020
		call Blue
		mov ax,0xb800
		mov es,ax
	
		mov di,0
		call drawBlueBorder
			
	;center box top bar
		mov di,660
		mov si,di
		add si,120
	nextchar1a:
		mov word [es:di],0x6020
		add di,2
		cmp di,si
		jne nextchar1a
		
		mov di,820
		mov si,di
		add si,120
	
	;center box main
	nextchar1b:
		mov word [es:di],0x0020
		add di,2
		cmp di,si
		jne nextchar1b
		add di,40
		mov si,di
		add si,120
		cmp di,2000
		jle nextchar1b
		
			;print message
		mov word ax, 1174
		push ax					;location of string print
		mov ax, 00001110b		;attribute byte
		push ax 
		mov ax, screenOneMessageOne
		push ax
		mov ax, 27				;message length
		push ax
		call printstr
			;print message
		mov word ax, 1014
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, screenOneMessageTwo
		push ax
		mov ax, 22				;message length
		push ax
		call printstr
					;print message
		mov word ax, 1334
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, screenOneMessageThree
		push ax
		mov ax, 16				;message length
		push ax
		call printstr
							;print message
		mov word ax, 1494
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, screenOneMessageFour
		push ax
		mov ax, 18				;message length
		push ax
		call printstr
									;print message
		mov word ax, 1654
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, screenOneMessageFive
		push ax
		mov ax, 19				;message length
		push ax
		call printstr
											;print message
		mov word ax, 1784
		push ax					;location of string print
		mov ax, 00001110b		;attribute byte
		push ax 
		mov ax, screenOneMessageSix
		push ax
		mov ax, 56				;message length
		push ax
		call printstr
		
		pop si
		pop di
		pop es
		pop dx
		pop ax
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printstr: 
		push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov di,[bp+10] ; point di to required location
		mov si, [bp+6] ; point si to string
		mov cx, [bp+4] ; load length of string in cx
		mov ah, [bp+8] ; load attribute in ah
		cld ; auto increment mode
nextcharprint: 
		lodsb ; load next char in al
		stosw ; print char/attribute pair
		loop nextcharprint ; repeat for the whole string
	
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printNumber:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
nextdigit: 
	mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, 180 ; point di to top left column
nextpos: 
	pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
clearScreen:							 ; function to clear screen
		push ax
		push es
		push di
		
		mov ax, 0xb800 					 ; load video base in ax
		mov es, ax 						 ; point es to video base
		mov di, 0						 ; point di to top left column
		
nextchar: 
		mov word [es:di], 0x0720 		 ; clear next char on screen
		add di, 2						 ; move to next screen location
		cmp di, 4000 					 ; has the whole screen cleared
		jne nextchar 					 ; if no clear next position
		
		pop di
		pop es
		pop ax
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawHurdles:										;function to draw hurdles
		push ax
		push es
		push si
		push di
		push bx
		push cx
		
		mov ax, 0xb800 ; load video base in ax
		mov es, ax ; point es to video base
		mov al, 0x2E
		mov ah, 00101010b
		
		mov word cx, 12
		mov bx, hurdleLocation
		mov si, 0
drawHurdlesLoop:
		mov word di, [bx+si]
		mov word [es:di], ax
		add si, 2
		loop drawHurdlesLoop
		
		pop cx
		pop bx
		pop di
		pop si
		pop es
		pop ax
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawMario:											;function to draw superMario
		push es
		push di
		push ax
		
		mov di, [marioLocation]
		mov ax, 0xb800
		mov es, ax
	
		mov al, 0x2A			;eyes
		mov ah, 11000001b
		mov word[es:di], ax
		add di, 4
		mov word[es:di], ax
		
		mov al, 0x2E
		mov ah, 01000000b
		sub di, 2
		mov word[es:di], ax		;nose
		
		mov ah, 01000100b
		add di, 160
		mov word[es:di], ax
		add di, 158
		mov word[es:di], ax
		add di, 4
		mov word[es:di], ax
		
		pop ax
		pop di
		pop es
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearMario:												;function to clear mario
		push es
		push di
		push ax
		
		mov di, [marioLocation]
		mov ax, 0xb800
		mov es, ax
		
		mov word[es:di], 0x0720
		add di, 4
		mov word[es:di], 0x0720
		
		sub di, 2
		mov word[es:di], 0x0720		;nose
		
		add di, 160
		mov word[es:di], 0x0720
		add di, 158
		mov word[es:di], 0x0720
		add di, 4
		mov word[es:di], 0x0720
		
		pop ax
		pop di
		pop es
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
enemyMovement:								;function to move enemies to and fro
		inc byte[cs:enemyMovementDelay]
		cmp byte[cs:enemyMovementDelay],5
		jne enemyMovementDone
		mov byte [cs:enemyMovementDelay],1

		cmp byte[cs:enemyMovementDirectionFlag],1
		je enemyMoveReverse
		push word [cs:enemyOneLocation]
		call clearEnemy
		push word [cs:enemyTwoLocation]
		call clearEnemy
		sub word[cs:enemyTwoLocation],2
		add word[cs:enemyOneLocation],2
		cmp word[enemyOneLocation], 3592
		jne enemyMovementDone
		mov byte [cs:enemyMovementDirectionFlag],1
		jmp enemyMovementDone
		
enemyMoveReverse:
		push word [cs:enemyOneLocation]
		call clearEnemy
		push word [cs:enemyTwoLocation]
		call clearEnemy
		add word[cs:enemyTwoLocation],2
		sub word[cs:enemyOneLocation],2
		cmp word[enemyOneLocation], 3566
		jne enemyMovementDone
		mov byte [cs:enemyMovementDirectionFlag],0
		
enemyMovementDone:
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawEnemy:								;function to draw enemy
		push bp
		mov bp, sp
		push es
		push di
		push ax
		
		mov ax, 0xb800
		mov es, ax
		
		mov word di, [bp+4]
		
		mov al, 0x30
		mov ah, 0000110b
		
		mov word [es:di], ax
		add word di, 4
		mov word [es:di], ax
		sub word di, 2
		add word di, 160
		
		mov al, 0x5E
		mov word [es:di], ax
		
		pop ax
		pop di
		pop es
		pop bp 
		ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearEnemy:						;function to clear enemy
		push bp
		mov bp, sp
		push es
		push di
		push ax
		
		mov ax, 0xb800
		mov es, ax
		mov word di, [cs:bp+4]
			
		mov word [es:di], 0x0720
		add word di, 4
		mov word [es:di], 0x0720
		sub word di, 2
		add word di, 160
		mov word [es:di], 0x0720
		
		pop ax
		pop di
		pop es
		pop bp 
		ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawMonster:				;function to draw the monster
	push di
	push ax
	push es
										;(-_(-_(-_(-_-)_-)_-)_-)
	mov di, [cs:monsterLocation]				;00000100 red
	mov ax, 0xb800			;00001110 yellow 
	mov es, ax				;0x28 (
							;0x29 )
	mov ah, 00000011b		
	mov al, 0x28			
	mov word [es:di], ax
	add word di, 6		
	mov word [es:di], ax
	add word di, 6			
	mov word [es:di], ax
	add word di, 6
	mov ah, 00001111b	
	mov word [es:di], ax	
	mov al, 0x29
	add word di, 8
	mov word [es:di], ax
	mov ah, 00000011b	
	add word di, 6
	mov word [es:di], ax
	add word di, 6
	mov word [es:di], ax
	add word di, 6
	mov word [es:di], ax
	add word di, 6
	mov word [es:di], ax
	sub word di, 2
	mov al, 0x2D
	mov ah, 00000100b
	mov word [es:di], ax
	sub word di, 6
	mov word [es:di], ax
	sub word di, 6
	mov word [es:di], ax
	sub word di, 6
	mov word [es:di], ax
	mov ah, 10000100b
	sub word di, 6
	mov word [es:di], ax
	sub word di, 4
	mov word [es:di], ax
	mov ah, 00000100b
	sub word di, 6
	mov word [es:di], ax
	sub word di, 6
	mov word [es:di], ax
	sub word di, 6
	mov word [es:di], ax
	add word di, 2
	mov ah, 00001110b
	mov al,0x5F
	mov word [es:di], ax
	add word di, 6
	mov word [es:di], ax	
	add word di, 6
	mov word [es:di], ax	
	add word di, 6
	mov ah, 00001111b
	mov word [es:di], ax	
	mov ah, 00001110b
	add di, 6
	mov word [es:di], ax	
	add word di, 6
	mov word [es:di], ax	
	add word di, 6
	mov word [es:di], ax	
	add word di, 6
	mov word [es:di], ax
	
	pop es
	pop ax
	pop di
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearMonsterLine:				;function clears the monster
		push ax
		push es
		push di
		push cx
		
		mov ax, 0xb800 					 ; load video base in ax
		mov es, ax 						 ; point es to video base
		mov di, [cs:monsterLocation]
		mov word cx, 26
		
nextMonsterChar: 
		mov word [es:di], 0x0720 		 ; clear next char on screen
		add di, 2						 ; move to next screen location
		loop nextMonsterChar					 ; if no clear next position
		
		pop cx
		pop di
		pop es
		pop ax
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
monsterMovement:				;function moves the monster to and fro
		inc byte[cs:monsterMovementDelay]
		cmp byte[cs:monsterMovementDelay],3
		jne monsterMovementDone
		mov byte[cs:monsterMovementDelay],1

		cmp byte[cs:monsterMovementDirectionFlag],1
		je monsterMoveReverse
		
		call clearMonsterLine
		add word[cs:monsterLocation],2
		cmp word[cs:monsterLocation], 424
		jne monsterMovementDone
		mov byte [cs:monsterMovementDirectionFlag],1
		jmp monsterMovementDone
		
monsterMoveReverse:
		call clearMonsterLine
		sub word[cs:monsterLocation],2
		cmp word[cs:monsterLocation], 324
		jne monsterMovementDone
		mov byte [cs:monsterMovementDirectionFlag],0
		
monsterMovementDone:
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
generateFireballs:			;function to generate fireball
	push ax 
	inc word [cs:fireball+4]
	cmp word [cs:fireball+4], 3
	jne cmpFireballTwo
	mov word [cs:fireball+4], 1
	mov word [cs:fireball], 1
	mov word ax, [cs:monsterLocation]
	add word ax, 22
	mov word [cs:fireball+2], ax

cmpFireballTwo:
fireballDrawingCompleted:
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printFireball:			;function to print fireball
	push bp
	mov bp, sp
	push ax
	push es
	push di
	
	mov ax, 0xb800
	mov es, ax 
	mov al, 0x25
	mov ah, 00001001b
	mov di, [bp+4]
	mov word [es:di], ax
	
	pop di
	pop es
	pop ax
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawCoinSackOne:			;function to draw coin sack
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	mov al, 0x3D
	mov ah, 00001110b
	mov word [es:2576], ax
	mov word [es:2578], ax
	mov word [es:2736], ax
	mov word [es:2738], ax   
	pop es
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearCoinSackOne:			;function to clear coin sack
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	mov word [es:2576], 0x0720
	mov word [es:2578], 0x0720
	mov word [es:2736], 0x0720
	mov word [es:2738], 0x0720
	pop es
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawCoinSackTwo:			;function to draw coin sack
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	mov al, 0x3D
	mov ah, 00001110b
	mov word [es:2690], ax
	mov word [es:2692], ax
	mov word [es:2850], ax
	mov word [es:2852], ax
	pop es
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearCoinSackTwo:			;function to clear coin sack
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	mov word [es:2690], 0x0720
	mov word [es:2692], 0x0720
	mov word [es:2850], 0x0720
	mov word [es:2852], 0x0720
	pop es
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawCoinSackThree:			;function to draw coin sack
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	mov al, 0x3D
	mov ah, 00001110b
	mov word [es:2320], ax
	mov word [es:2322], ax
	mov word [es:2480], ax
	mov word [es:2482], ax
	pop es
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearCoinSackThree:			;function to clear coin sack
	push ax
	push es
	mov ax, 0xb800
	mov es, ax
	mov word [es:2320], 0x0720
	mov word [es:2322], 0x0720
	mov word [es:2480], 0x0720
	mov word [es:2482], 0x0720
	pop es
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
drawKingdom:					;function to draw the kingdom
	push es
	push ax
	mov ax, 0xb800
	mov es, ax
	mov al, 0x7C
	mov ah, 00001011b
	mov word [es:3822], ax
	mov word [es:3662], ax
	mov word [es:3502], ax
	mov word [es:3834], ax
	mov word [es:3674] ,ax
	mov word [es:3514], ax
	mov word [es:3506], ax
	mov word [es:3510], ax
	mov word [es:3348], ax
	mov ah, 00001111b	
	mov word [es:3826], ax
	mov word [es:3830], ax
	mov al, 0x5f
	mov word [es:3824], ax
	mov word [es:3832], ax
	mov word [es:3504], ax
	mov word [es:3344], ax
	mov word [es:3512], ax	
	mov word [es:3352], ax		
	mov al, 0x30
	mov ah, 00001101b
	mov word [es:3828], ax
	mov al, 0x2F
	mov ah, 00001111b	
	mov word [es:3668],	ax
	mov al, 0x5C
	mov word [es:3670], ax
	mov ah, 00001111b	
	mov al, 0x2D
	mov word [es:3508], ax
	mov ah, 10001101b	
	mov al, 0x7E
	mov word [es:3350], ax
	
	pop ax
	pop es
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
removeFireBall:			;function to clear the fireball
	push bp
	mov bp, sp
	push ax
	push es
	push di
	
	mov ax, 0xb800
	mov es, ax 
	mov di, [bp+4]
	mov word [es:di], 0x0720
	
	pop di
	pop es
	pop ax
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fireBallCollisionDetection:		;check if character has collided with character
	push ax
	cmp word [cs:fireball], 0
	je fireballNotCollided
	mov word ax, [cs:marioLocation]
	
	cmp word ax, [cs:fireball+2]
	je fireballCollided
	add word ax, 2
	cmp word ax, [cs:fireball+2]
	je fireballCollided
	add word ax, 2
	cmp word ax, [cs:fireball+2]
	je fireballCollided
	jmp fireballNotCollided
fireballCollided:
	mov word [cs:marioLife], 0
fireballNotCollided:
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkWin:		;check if mario has reached the kingdom
	push ax
	mov word ax, [cs:marioLocation]
	add word ax, 2
	cmp word ax, 3508
	jne notWon
	mov byte [cs:winFlag], 1
notWon:	
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TIMEISR:
	push ax
	push es
	push di
	push cx
	
	cmp word [cs:fireball], 1
	je getfireballDown
	call generateFireballs
getfireballDown:
	cmp word [cs:fireball+2], 3364
	jae fireballGravityNotRequired
	push word [cs:fireball+2]
	call removeFireBall
	add word [cs:fireball+2], 160
	push word [cs:fireball+2]				;fireball functionality
	call printFireball
	call fireBallCollisionDetection
	jmp fireballDone
fireballGravityNotRequired:
	mov word [cs:fireball], 0
	push word [cs:fireball+2]
	call removeFireBall
fireballDone:	
	cmp word [cs:coinSackOneCheck], 5
	jae skipGravityCheckOfCoinOne
	cmp word [cs:marioRightLegLocation], 2416
	je gravityNotRequiredShort
	cmp word [cs:marioLeftLegLocation], 2418
	je gravityNotRequiredShort
	cmp word [cs:marioRightLegLocation], 2416
	je gravityNotRequiredShort
	cmp word [cs:marioLeftLegLocation], 2418 
	je gravityNotRequiredShort

	jmp skipGravityCheckOfCoinOne
gravityNotRequiredShort:
	jmp gravityNotRequired
skipGravityCheckOfCoinOne:
	cmp word [cs:coinSackTwoCheck], 5
	jae skipGravityCheckOfCoinTwo
	cmp word [cs:marioRightLegLocation], 2530
	je gravityNotRequired
	cmp word [cs:marioLeftLegLocation], 2532
	je gravityNotRequired
	cmp word [cs:marioRightLegLocation], 2530
	je gravityNotRequired
	cmp word [cs:marioLeftLegLocation], 2532 
skipGravityCheckOfCoinTwo:
	cmp word [cs:coinSackThreeCheck], 5
	jae skipGravityCheckOfCoinSacks
	cmp word [cs:marioRightLegLocation], 2160
	je gravityNotRequired
	cmp word [cs:marioLeftLegLocation], 2162
	je gravityNotRequired
	cmp word [cs:marioRightLegLocation], 2160
	je gravityNotRequired
	cmp word [cs:marioLeftLegLocation], 2162 
skipGravityCheckOfCoinSacks:
	cmp word [cs:marioLocation],3364
	jae gravityNotRequired
	call jumpDelay
	call clearMario
	add word [cs:marioLocation], 160
	call detectCoinSackOne				;detect coin sack
	call detectCoinSackTwo				;detect coin sack
	call detectCoinSackThree				;detect coin sack
	call detectCollision				;detect collison
	cmp word [cs:locationCheckFlag], 1
	jne gravityNotRequired
	sub word [cs:marioLocation], 160
	
gravityNotRequired:
	call clearCoinSackOne
	cmp word [cs:coinSackOneCheck], 5
	jae dontDrawCoinSackOne
	call drawCoinSackOne
dontDrawCoinSackOne:
	call clearCoinSackTwo
	cmp word [cs:coinSackTwoCheck], 5
	jae dontDrawCoinSackTwo
	call drawCoinSackTwo
dontDrawCoinSackTwo:
	call clearCoinSackThree
	cmp word [cs:coinSackThreeCheck], 5
	jae dontDrawCoinSackThree
	call drawCoinSackThree
dontDrawCoinSackThree:	
		mov word ax, 166
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, scoreMessage
		push ax
		mov ax, 7				;message length
		push ax
		call printstr
		push word [cs:score]
		call printNumber

	call drawMario					;draw mario
	call drawKingdom				;draw kingdom
	call checkWin					;check if won
	call detectCoinSackOne					;detect coin sack
	call detectCoinSackTwo					;detect coin sack
	call detectCoinSackThree				;detect coin sack
	call detectCollision					;detect collision
	call enemyMovement
	push word[cs:enemyOneLocation]
	call drawEnemy
	push word[cs:enemyTwoLocation]
	call drawEnemy				;draw enemy
	call monsterMovement		;move monsters
	call drawMonster			;draw monster
	call drawHurdles			;draw hurdles
	call drawBlueBorder			;draw border
	
	mov al, 0x20		; send EOI to PIC
	out 0x20, al
	
	pop cx
	pop di
	pop es
	pop ax
	iret 								; return from interrupt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
KBISR:
	push ax
	
	in al, 0x60							; read a char from keyboard port
	
	cmp al, 0x1E									;if left key is pressed
	jne notLeft
	call clearMario
	sub word [cs:marioLocation],2
	call detectCoinSackOne				;detect coin sack
	call detectCoinSackTwo				;detect coin sack
	call detectCoinSackThree				;detect coin sack
	call detectCollision				;detect collision
	cmp word [cs:locationCheckFlag], 1
	je leftCollisionDetected
	cmp word [cs:marioLocation],3364
	jae notDown
leftCollisionDetected:
	add word [cs:marioLocation],2
notLeft:	
	cmp al, 0x20			;if right key is pressed
	jne notRight
	cmp word [cs:locationCheckFlag], 1
	je notDown
	call clearMario
	add word [cs:marioLocation],2	
	call detectCoinSackOne			;detect coin sack
	call detectCoinSackTwo			;detect coin sack
	call detectCoinSackThree			;detect coin sack
	call detectCollision				;detect collision
	cmp word [cs:locationCheckFlag], 1
	je RightCollisionDetected	
	cmp word [cs:marioLocation],3510
	jbe notDown
RightCollisionDetected:	
	sub word [cs:marioLocation],2
notRight:
	cmp al, 0x11						;if up key is pressed
	jne notDown
	call clearMario
	sub word [cs:marioLocation], 640
notDown:
	mov word [cs:locationCheckFlag], 0
	
	pop ax
	jmp far[cs:oldKB]	                 ; call the original ISR
	
	mov al, 0x20						 ; send EOI to PIC
	out 0x20, al 
	
	pop ax
	iret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start:	
	call clearScreen				 ;call clrscr
	call startGameScreen			 ;call welcome screen 
l3:	mov ah, 0
	int 0x16	
	cmp al, 32						 ;if space is pressed continue
	jne l3							 ;no, take input again

	call clearScreen				 ;call clrscr
	
	mov ax, 0
	mov es, ax						 ; point es to IVT base
	
	mov ax, [es:9*4]				 ; save offset of old routine
	mov word [oldKB], ax
	mov ax, [es:9*4+2]
	mov word [oldKB+2], ax			 ; save segment of old routine
	
	mov ax, [es:8*4]				 ; save offset of old routine
	mov word [oldTime], ax
	mov ax, [es:8*4+2]
	mov word [oldTime+2], ax		 ; save segment of old routine
	
	cli								 ; disable interrupts
	mov word [es:9*4], KBISR		 ; store offset at n*4
	mov  [es:9*4+2], cs				 ; store segment at n*4+2
	mov word [es:8*4], TIMEISR		 ; store offset at n*4
	mov  [es:8*4+2], cs				 ; store segment at n*4+2
	sti								 ; enable interrupts
	
l1: 
	;mov ah, 0						 ; service 0 – get keystroke
	;int 0x16 						 ; call BIOS keyboard service
	cmp byte[cs:winFlag], 1
	je gameWon
	cmp byte[cs:marioLife],0
	jne l1 		;jmp l1	
cmp word [cs:winFlag], 1
jne gameLost
gameWon:
		;print message
		mov word ax, 1996
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, winMessage
		push ax
		mov ax, 8				;message length
		push ax
		call printstr
		jmp l2
gameLost:
		;print message
		mov word ax, 1996
		push ax					;location of string print
		mov ax, 00001111b		;attribute byte
		push ax 
		mov ax, gameOverMessage
		push ax
		mov ax, 9				;message length
		push ax
		call printstr
		
l2:	mov ax, [oldKB]					 ; read old offset in ax
	mov bx, [oldKB+2]				 ; read old segment in bx
	mov cx, [oldTime]
	mov dx, [oldTime+2]
	cli 							 ; disable interrupts
	mov [es:9*4], ax				 ; restore old offset from ax
	mov [es:9*4+2], bx 				 ; restore old segment from bx
	mov [es:8*4], cx 			   	 ; restore old offset from ax
	mov [es:8*4+2], dx				 ; restore old segment from bx
	sti								 ; enable interrupts
	mov ax, 0x4c00					 ; terminate program
	int 0x21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
marioLocation: dw 3364				 ;to save mario location 
oldKB: dd 0
oldTime: dd 0
winFlag: db 0
locationCheckFlag: dw 0				;to detect collison with hurdles
marioLeftLegLocation: dw 0			;loation of left leg of mario
marioRightLegLocation: dw 0			;loation of right leg of mario
screenOneMessageOne: db 'Press Space Key To Continue'
screenOneMessageTwo: db 'WELCOME TO SUPER MARIO'
screenOneMessageThree: db 'Press "w" For Up'
screenOneMessageFour: db 'Press "a" For Left'
screenOneMessageFive: db 'Press "d" For Right'
screenOneMessageSix: db 'REACH THE KINGDOM TO WIN & HIT THE BRICKS TO EARN POITNS'
scoreMessage: db 'Score: '
gameOverMessage: db 'Game Over'
winMessage:db 'YOU WON!'
enemyOneLocation: dw 3566,0,0,0,0,0,0,0,0
enemyTwoLocation: dw 3632,0,0,0,0,0,0,0,0
enemyMovementDirectionFlag: db 0		;0 for forward, 1 for backward
monsterMovementDirectionFlag: db 0		;0 for forward, 1 for backward
enemyMovementDelay: db 0				;gives delay to enemy movement
monsterMovementDelay: db 0				;gives delay to monster movement
hurdleLocation: dw 3720, 3722, 3600, 3602, 3760, 3762, 3480, 3482, 3640, 3642, 3800, 3802 ;locations of hurdles
marioLife: db 1							;wether mario is alive or not
monsterLocation: dw 324					;to save monster location
fireball: dw 0, 0, 2					;check for fireball existence, fireball position, fireball timer
coinSackOneCheck: db 0					;life check of coin sack
coinSackTwoCheck: db 0					;life check of coin sack
coinSackThreeCheck: db 0				;life check of coin sack
score: dw 0