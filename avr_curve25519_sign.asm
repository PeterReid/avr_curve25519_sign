/*
 * avr_curve25519_sign.asm
 *
 *  Created: 2/7/2015 8:04:56 AM
 *   Author: Peter Reid
 */ 
.org 0
MOV R5, R27

// Load some static data to work with into RAM, for testing
LDI R26, 0
LDI R27, 1
LDI R30, 0
LDI R31, 8
EOR R1, R1
EOR R3, R3
initialize_loop_top:
  LPM R2, Z+
  ST X+, R2
  DEC R1
  CP R1, R3
  BRNE initialize_loop_top 


LDI R26, 32
LDI R27, 1
LDI R28, 64
LDI R29, 1
LDI R30, 96
LDI R31, 1
rcall mul_32_by_32



.org 1024
.db 0xed, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0x7f


/*.db 0xec, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0x7f*/

.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
/*
.db 0xff, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
.db 0x00, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
.db 0x00, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
.db 0x00, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
*/

.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff

.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0

.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0

.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0
.db 0xd0, 0xd0, 0xd0, 0xd0,   0xd0, 0xd0, 0xd0, 0xd0


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
	

// adds a 32-byte integer in [X] to a 32-byte integer in [Y], storing the
// result in [Z]. It is assumed to not overflow.
// The upper byte of X, Y, and Z must not roll over when the register is increased by 32,
// or the register will be clobbered.
// Clobbers: R16, R2, R3
add_32_to_32:
	LDI R16, 31
	CLC
	add_32_to_32_top:
		LD R2, X+
		LD R3, Y+
		ADC R2, R3
		ST Z+, R2
		DEC R16
		BRPL add_32_to_32_top
	SBIW R26, 32
	SBIW R28, 32
	SBIW R30, 32
	RET
	
// subtracts a 32-byte integer in [Y] from a 32-byte integer in [X], storing the
// result in [Z].
// The upper byte of X, Y, and Z must not roll over when the register is increased by 32,
// or the register will be clobbered.
// Clobbers: R16, R2, R3
sub_32_from_32:
	//
	// First, do a normal 256-bit subtraction.
	//
	LDI R16, 31 // Go around 32 times
	CLC // 
	sub_32_from_32_top:
		LD R2, X+
		LD R3, Y+
		SBC R2, R3
		ST Z+, R2
		DEC R16
		BRPL sub_32_from_32_top
	
	// If we ended up with an underflow (carry set), then we need to behave as if we wrapped around
	// to 2**255-19-1 instead of 2**256-1. We can do that by subtracting off (2**256 - (2**255-19)).
	// That difference is
	// 0x8000000000000000000000000000000000000000000000000000000000000013
	LDS R17, 0x5F // Load the status register. Bit 0 (Carry) is set if we underflowed.
	ANDI R17, 1 // R17 is now 0x01 is we underflowed, and 0x00 otherwise
	NEG R17 // R17 is now 0xff is we underflowed, and 0x00 otherwise

	// The LSB is a special case, since it is 0x13
	SBIW R30, 32
	LD R2, Z
	LDI R16, 0xED
	AND R16, R17
	ADD R2, R16
	ST Z+, R2
	
	LDI R16, 29 // loop 30 times
	sub_32_from_32_wraparound_top:
		LD R2, Z
		ADC R2, R17
		ST Z+, R2 
		DEC R16
		BRPL sub_32_from_32_wraparound_top
	LD R16, Z
	ANDI R17, 0x7F
	ADC R16, R17
	ST Z, R16

	SBIW R26, 32
	SBIW R28, 32
	SBIW R30, 31

	RET

// Test if [X] >= 2^255 - 19
// R2 is set to 0xff if it is greater or equal and 0x00 if it is lesser
// Clobbbers R4, R16, R17, R18, R19
is_gte_25519:
	// 2**255-19 = 7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
	// X         = ????????????????????????????????????????????????????????????????
	//
	// We think of this in runs of bytes. Here are their values for 2**255-19
	//   Major: 0x7f
	//   Middle: ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	//   Minor: 0xed
	//
	// From this we can derive a few variables:
	//   major_lt:  is X's Major  less than that of 2**255-19 ?
	//   major_eq:  is X's Begin  equal to  that of 2**255-19 ?
	//   middle_lt: is X's Middle less than that of 2**255-19 ?
	//   minor_lt:  is X's Minor  less than that of 2**255-19 ?
	//
	// Then the answer to X < 2**255-19 is:
	//
	// In general, we can compare the (Major, Middle, Minor) tuple with
	//   major_lt | (major_eq & (middle_lt | (major_eq & minor_lt)))
	//
	// That is, the whole is lesser if either the more significant element is lesser than its partner
	// or it matches and the less signficant remainder is recursively lesser.
	// Since !middle_lt implies middle_eq for 2**255-19's Middle, though, we can simplify to:
	//
	// major_lt | (major_eq & (middle_lt | minor_lt))
	//

	// X starts pointing at the LSB, so we start by computing minor_lt
	LD R4, X+ // Load the Minor of X
	LDI R16, 0xed // Load the Minor of 2*255-19
	CP R4, R16 // Compare. The C bit (bit 0) will be set if X[0]<0xed. The Z bit (bit 1) will be set if X[0]=0xed
	LDS R17, 0x5F // Save the status register. The low bit is minor_lt.
	
	// Next we must compute middle_lt. The technique is to compute middle_eq by ANDing all the
	// bytes in X's Middle together, and then comparing with 0xff.
	LDI R19, 0xff // ANDing accumulator -- this could use a non-immediate register with only one word extra
	LDI R18, 29 // loop counter -- we must go around 30 times
	is_gte_25519_anding_loop:
		LD R4, X+
		AND R19, R4
		DEC R18
		BRPL is_gte_25519_anding_loop
	// R19 now holds all the bytes of X's middle ANDed together. (R19=0xff) <-> middle_eq
	// Conveniently, R18 has wrapped around and now holds 0xff, which we want to compare against
	CP R19, R18 // Compare X's ANDed Middle is 0xff
	LDS R18, 0x5F // R19's C bit now holds middle_lt: TODO: Verify signedness of CP

	// Finally, we must compute major_lt and major_eq
	LDI R16, 0x7f
	LD R4, X
	CP R4, R16 // Compare. The C bit (bit 0) will be set if major_lt (i.e. X[31]<0x7f). The Z bit (bit 1) will be set if major_eq (i.e. X[0]=0x7f.)
	LDS R16, 0x5F // R16 now holds major_lt and major_eq
	MOV R19, R16
	LSR R19	// R19's bit 0 holds major_eq

	// To summarize:
	// major_lt  is in R16's low bit
	// major_eq  is in R19's low bit
	// middle_lt is in R18's low bit
	// minor_lt  is in R17's low bit

	OR R18, R17 // R18's low bit now holds middle_lt | minor_lt
	AND R18, R19 // R18's low bit now holds major_eq & (middle_lt | minor_lt)
	OR R16, R18 // R16's low bit now holds major_lt | (major_eq & (middle_lt | minor_lt))
	ANDI R16, 0x01 // R16 now 0x01 if X < 2**255-19, otherwise 0x00
	NEG R16  // R16 is now 0xff if X < 2**255-19, otherwise 0x00
	COM R16  // R16 is now 0xff if X >= 2**255-19, otherwise 0x00

	MOV R2, R16
	SBIW X, 31 // Move X back to where it was
	RET


mul_32_by_32:
	//
	// Zero out Z -- we're going to be accumulating into it.
	//
	LDI R18, 63 // loop around for each of the 64 bytes in Z
	mul_32_by_32_zero_z_top:
		ST Z+, R0
		DEC R18
		BRPL mul_32_by_32_zero_z_top
	SBIW Z, 32
	SBIW Z, 32

	//
	// Do a plain 32-byte-by-32-byte multiplication, grade school-style.
	//
	LDI R18, 31 // Go around 32 times for each byte of X
	mul_32_by_32_x_top:
		LD R9, X+

		EOR R7, R7 // Initialize our carry to 0
		LDI R19, 31 // Go around 32 times for each byte of Y
		mul_32_by_32_y_top:
			LD R5, Y+
			MOV R4, R9
			rcall mul_8_by_8
			// X[i]*Y[i] is now in R2(lower):R3(upper)
			// Include the carry
			ADD R2, R7
			ADC R3, R0
			
			// Include the sum in this place so far
			LD R5, Z
			ADD R2, R5
			ADC R3, R0

			ST Z+, R2

			MOV R7, R3 // Store the carry for next time around

			DEC R19
			BRPL mul_32_by_32_y_top
		SBIW Y, 32
		
		// Write the carry into the (so-far-untouched) next destination digit
		ST Z, R7

		// Roll Z almost back to where we started this loop, but one byte short.
		// We will accumulate into higher bytes next time around.
		SBIW Z, 31 

		DEC R18
		BRPL mul_32_by_32_x_top
	RET

