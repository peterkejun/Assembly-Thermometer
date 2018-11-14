
; PIC16F690 Configuration Bit Settings
; Assembly source line config statements
#include "p16f690.inc"

; CONFIG
; __config 0xF014
  __CONFIG _FOSC_INTRCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_ON & _CPD_ON & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF

  cblock    0x20
  delay1Reg			;a register for delay during ramping up LEDs
  delay2Reg			;a register for delay during ramping up LEDs
  sweepDelayReg1		;a register for delay during sweep mode
  sweepDelayReg2		;a register for delay during sweep mode
  currentPattern		;a register to store current pattern of LED after player reacts (presses the button)
  random			;a register to store random delay time before LEDs ramp up
  delayInterval			;a register to store desired delay interval before calling a delay funtion ("parameter")
  maximizeReg1			;a register for random delay before LEDs ramp up
  maximizeReg2			;a register for random delay before LEDs ramp up
  maximizeReg3			;a register for random delay before LEDs ramp up
  rollOverCount			;a register to store how many times the LEDs have finished one round of ramping up 
  shortDelayReg1		;a register for short delay
  shortDelayReg2		;a register for short delay
  longDelayReg1			;a register for long delay
  longDelayReg2			;a register for long delay
  longDelayReg3			;a register for long delay
  flashCount			;a register to store numbers of flashing (to indicate player's performance)
  endc
 
    org 0			    ;assembler directive, set the location to assemble code
setup				    ;setting up the game, should be only called at the beginning of the program
    bsf	    STATUS,RP0		    ;select register page 1
    bcf	    STATUS,RP1		
    
    bcf	    OPTION_REG,NOT_RABPU	;enable pull-ups for port a and b (change NOT_RABPU to nRABPU to compile this 
					    ;code with earlier versions of MPLAB IDE. Check local "p16f690.inc" file
					    ;for possible alternative naming of pull-up enable bit of option register.
    
    movlw   0			    ;set port c to output	
    movwf   TRISC		
    
    movlw   b'11110000'		    ;set port b to input
    movwf   TRISB		
    
    bsf	    STATUS,RP1		    ;select register page 2
    bcf	    STATUS,RP0
    clrf    ANSEL		    ;diable analog ports
    clrf    ANSELH		
    
    movlw   0xFF
    movwf   WPUB		    ;enable pull-up registers for all bits of port b
    
    bcf	    STATUS,RP0		    ;back to page 0
    bcf	    STATUS,RP1		
    
start
    clrf    currentPattern	;reset registers
    clrf    delayInterval
    clrf    rollOverCount
    clrf    flashCount
    clrf    random
    
    clrf    PORTC		;clear LEDs

    movlw   255			;prepare for debounce delay
    movwf   delayInterval
    movwf   maximizeReg1
    
    goto    sweep		;start sweeping
        
sweep				;sweeping LEDs before player initiates the game
    movlw   b'00000000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'10000001'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11000011'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11100111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'01111110'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00111100'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00011000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00011000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00011000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00011000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00011000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00111100'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'01111110'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11111111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11100111'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'11000011'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'10000001'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00000000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00000000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00000000'
    movwf   PORTC
    call    sweepDelay
    
    movlw   b'00000000'
    movwf   PORTC
    call    sweepDelay
    
    goto    sweep
    
sweepDelay			;delay block designed specifically for switching LED patterns during sweep mode, 
    movlw   80			    ;while checking for button press
    movwf   sweepDelayReg1
    movwf   sweepDelayReg2
sweepDelay1
    decfsz  sweepDelayReg1,f
    goto    sweepDelay1
sweepDelay2
    incf    random
    btfss   PORTB,4		;check for button press to initiate game
    goto    debounceDelay
    decfsz  sweepDelayReg2,f
    goto    sweepDelay1
sweepDelayEnd
    return

    
    
    
debounceDelay			;delay block designed for preventing malfunctions from button debouncing
    decfsz  delayInterval,f
    goto    debounceDelay
    decfsz  maximizeReg1,f
    goto    debounceDelay
    movlw   255
    movwf   maximizeReg2
    movlw   10	
    movwf   maximizeReg3
    movlw   0xFF
    movwf   PORTC
    
randomDelay			;delay block designed for delaying a random amount of time before ramping up LEDs
    decfsz  random,f
    goto    randomDelay
    decfsz  maximizeReg2,f
    goto    randomDelay
    decfsz  maximizeReg3
    goto    randomDelay
    
loop
    movlw   50
    movwf   delayInterval
    
    movlw   b'00000001'		
    movwf   PORTC		;00000001
    movwf   currentPattern
    call    delay
    
    movlw   b'00000011'		;00000010
    movwf   PORTC
    movwf   currentPattern
    call    delay
    
    movlw   b'00000111'		;00000100
    movwf   PORTC
    movwf   currentPattern
    call    delay
    
    movlw   b'00001111'		;00001000
    movwf   PORTC
    movwf   currentPattern
    call    delay
    
    movlw   b'00011111'		;00010000
    movwf   PORTC
    movwf   currentPattern
    call    delay
    
    movlw   b'00111111'		;00100000
    movwf   PORTC
    movwf   currentPattern
    call    delay
    
    movlw   b'01111111'		;01000000
    movwf   PORTC
    movwf   currentPattern
    call    delay
    
    incf    rollOverCount	;increment rollOverCount to record whether the player has missed one round of ramping up
    goto    loop
    
delay				;delay block specifically designed for switching LED patterns while checking 
    movfw   delayInterval	    ;for player reaction
    movwf   delay1Reg
    movwf   delay2Reg
delay1
    decfsz  delay1Reg,f
    goto    delay1
delay2
    btfss   PORTB,4		;check for button press which signifies that player has reacted
    goto    finish
    
    decfsz  delay2Reg,f
    goto    delay1
endDelay
    return
    
finish				;after player reacted, indicate his performance by showing which LEDs 
    movlw   5			    ;were on before his reaction
    movwf   delayInterval
    movlw   3
    movwf   flashCount
    movfw   PORTC
    movwf   currentPattern
flash3times			;indicate the current pattern by flashing LEDs 3 times
    movfw   currentPattern
    movwf   PORTC
    call    longDelay
    movlw   0
    movwf   PORTC
    call    longDelay
    
    decfsz  flashCount,f
    goto    flash3times
    
    goto    result
    
shortDelay			;delay block used for shorter delays (1 nested delay)
    movfw   delayInterval
    movwf   shortDelayReg1
    movwf   shortDelayReg2
shortDelay1
    decfsz  shortDelayReg1,f
    goto    shortDelay1
shortDelay2
    decfsz  shortDelayReg2,f
    goto    shortDelay1
endShortDelay
    return
    
longDelay			;delay block used for longer delays (2 nested delays)
    movfw   delayInterval
    movwf   longDelayReg1
    movwf   longDelayReg2
    movwf   longDelayReg3
longDelay1
    decfsz  longDelayReg1,f
    goto    longDelay1
longDelay2
    decfsz  longDelayReg2,f
    goto    longDelay1
longDelay3
    decfsz  longDelayReg3,f
    goto    longDelay1
endLongDelay
    return
    
result
    btfsc   currentPattern,3	;if player reacted before 3rd LED was on, celebrate, otherwise restart the game
    goto    notGood
    goto    celebration

celebration
    movlw   256
    movwf   delayInterval
    movlw   30			;celebrate by switching between every first LEDs and every second LEDs for 30 times
    movwf   flashCount
celebrationFlash
    movlw   b'11010101'		;note that the eighth LED, which is the result LED, is always on to indicate "good job"		
    movwf   PORTC
    call    shortDelay
    movlw   b'10101010'
    movwf   PORTC
    call    shortDelay
    
    decfsz  flashCount,f
    goto    celebrationFlash
    
    goto    start		;restart the game
    
notGood
    goto    start		;restart the game
	
	end

