/*
 * avr_curve25519_sign.asm
 *
 *  Created: 2/7/2015 8:04:56 AM
 *   Author: Peter Reid
 */ 
.org 0


LDI R16, 0x80
LDI R17, 0xff
MOV R4, R16
MOV R5, R17
rcall mul_8_by_8



LDI R19, 7

.org 64

// mul_8_by_8: R2:R3 <- R4*R5
// R4, R5, R6, R8, R16, R17, are clobbered
// R2 holds the lower bits of the result; R3 holds the upper bits
mul_8_by_8:
	EOR R2, R2
	EOR R3, R3
	EOR R6, R6 // R6 will hold the bits of R5 that get shifted upwards.
	LDI R17, 7 // A loop counter -- once around for each of the 8 bits
	mul_8_by_8_top:
		// Build a mask (0xff or 0x00) in R16 so that we can maybe-supress the add for this bit
		MOV R16, R4
		ANDI R16, 1
		NEG R16

		// Do a conditional add of B into the product
		MOV R8, R5  // Copy so we don't clobber the lower byte of B
		AND R8, R16 // Apply the mask
		ADD R2, R8 // Accumulate into the low byte
		MOV R8, R6 // Copy so we don't clobber the upper byte of B
		AND R8, R16 // Apply the mask
		ADC R3, R8 // Accumulate into the upper byte, with carry

		//
		// Get ready to multiply the next bit of A with B.
		//

		// Shift R4 (aka A) to the right one bit, so we'll test a more significant bit next time
		LSR R4

		// Shift R5:R6 (aka B) one bit to the left. 
		LSL R5 // Moves a 0 in the lower end and sets the carry based on the bit shifted out
		ROL R6 // Incorporates the carry into the upper bits

		// Possibly go around again -- once for each bit for A
		DEC R17
		BRPL mul_8_by_8_top
	RET
	