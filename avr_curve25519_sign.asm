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
LDI R28, 96
LDI R29, 1
LDI R30, 128
LDI R31, 1
rcall add_32_to_32



.org 1024
.db 0xed, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0x7f


/*.db 0xec, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0x7f

.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff

.db 0xff, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
.db 0x00, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
.db 0x00, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00
.db 0x00, 0x00, 0x00, 0x00,   0x00, 0x00, 0x00, 0x00


.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff,   0xff, 0xff, 0xff, 0xff*/

.db 0x67, 0x85, 0x80, 0x39, 0x39, 0x15, 0x1a, 0x6a
.db 0xc9, 0x64, 0x65, 0xcb, 0x9d, 0xd7, 0x6d, 0x83
.db 0x5d, 0xe5, 0x90, 0x17, 0x11, 0x46, 0xb0, 0xe1
.db 0xd1, 0xa6, 0xde, 0xc4, 0x5d, 0x28, 0x43, 0x16

.db 0x5f, 0xc9, 0x96, 0x80, 0xd1, 0x99, 0x9d, 0x95
.db 0x11, 0xbf, 0x39, 0x31, 0x8f, 0xcc, 0xda, 0xee
.db 0x8f, 0xb2, 0xbf, 0x21, 0x94, 0xfa, 0xab, 0xce
.db 0x23, 0x72, 0x83, 0xb9, 0xe1, 0x38, 0xdf, 0x50

// p25519 - 3
.db 0xea, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
.db 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f

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

	RCALL maybe_subtract_p25519

	RET
	
// subtracts a 32-byte integer in [Y] from a 32-byte integer in [X] (mod p25519), storing the
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

// Test if [Z] >= 2^255 - 19
// R2 is set to 0xff if it is greater or equal and 0x00 if it is lesser
// Clobbbers R4, R16, R17, R18, R19
is_gte_25519:
	// 2**255-19 = 7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
	// Z         = ????????????????????????????????????????????????????????????????
	//
	// We think of this in runs of bytes. Here are their values for 2**255-19
	//   Major: 0x7f
	//   Middle: ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
	//   Minor: 0xed
	//
	// From this we can derive a few variables:
	//   major_lt:  is Z's Major  less than that of 2**255-19 ?
	//   major_eq:  is Z's Begin  equal to  that of 2**255-19 ?
	//   middle_lt: is Z's Middle less than that of 2**255-19 ?
	//   minor_lt:  is Z's Minor  less than that of 2**255-19 ?
	//
	// Then the answer to Z < 2**255-19 is:
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

	// Z starts pointing at the LSB, so we start by computing minor_lt
	LD R4, Z+ // Load the Minor of Z
	LDI R16, 0xed // Load the Minor of 2*255-19
	CP R4, R16 // Compare. The C bit (bit 0) will be set if Z[0]<0xed. The Z bit (bit 1) will be set if Z[0]=0xed
	LDS R17, 0x5F // Save the status register. The low bit is minor_lt.
	
	// Next we must compute middle_lt. The technique is to compute middle_eq by ANDing all the
	// bytes in Z's Middle together, and then comparing with 0xff.
	LDI R19, 0xff // ANDing accumulator -- this could use a non-immediate register with only one word extra
	LDI R18, 29 // loop counter -- we must go around 30 times
	is_gte_25519_anding_loop:
		LD R4, Z+
		AND R19, R4
		DEC R18
		BRPL is_gte_25519_anding_loop
	// R19 now holds all the bytes of Z's middle ANDed together. (R19=0xff) <-> middle_eq
	// Conveniently, R18 has wrapped around and now holds 0xff, which we want to compare against
	CP R19, R18 // Compare Z's ANDed Middle is 0xff
	LDS R18, 0x5F // R19's C bit now holds middle_lt: TODO: Verify signedness of CP

	// Finally, we must compute major_lt and major_eq
	LDI R16, 0x7f
	LD R4, Z
	CP R4, R16 // Compare. The C bit (bit 0) will be set if major_lt (i.e. Z[31]<0x7f). The Z bit (bit 1) will be set if major_eq (i.e. Z[0]=0x7f.)
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
	ANDI R16, 0x01 // R16 now 0x01 if Z < 2**255-19, otherwise 0x00
	NEG R16  // R16 is now 0xff if Z < 2**255-19, otherwise 0x00
	COM R16  // R16 is now 0xff if Z >= 2**255-19, otherwise 0x00

	MOV R2, R16
	SBIW Z, 31 // Move Z back to where it was
	RET

	
// Multiply a 32-byte integer in [Y] from a 32-byte integer in [X], storing the
// product in 64 bytes at [Z].
// 
// Clobbers: R2, R3, R4, R5, R6, R7, R8, R9, R16, R17, R18, R19
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

	// Put the pointers back where we found them
	SBIW Z, 32
	SBIW X, 32
	RET

// Subtracts p25519 from Z if Z >= p25519
// Clobbbers R2, R4, R16, R17, R18, R19
maybe_subtract_p25519:
	RCALL is_gte_25519

	LDI R16, 0xed
	LD R4, Z
	AND R16, R2
	SUB R4, R16
	ST Z+, R4

	LDI R18, 29
	maybe_subtract_p25519_loop_top:
		LD R4, Z
		SBC R4, R2
		ST Z+, R4

		DEC R18
		BRPL maybe_subtract_p25519_loop_top
		
	LD R4, Z
	LSR R2 // Finally subtrahend, the high byte of p25519 is either 0x7f or 0x00. In either case, that is the mask shifted right
	SBC R4, R2
	ST Z+, R4

	SBIW Z, 32
	RET
	
// Multiply a 32-byte integer in [Y] from a 32-byte integer in [X], storing the
// product in 64 bytes at [Z].
// 
// Clobbers: R2, R3, R4, R5, R6, R7, R8, R9, R16, R17, R18, R19
mul_32_by_32_mod_p25519:
	RCALL mul_32_by_32

	// In Z, we have a 64-byte product. We can call its halves z_low and z_high.
	//
	//   product % p25519 
	// = (z_low + z_high*2^256) % p25519
	// = ((z_low % p25519) + (z_high*2^256 % p25519)) % p25519
	// = ((z_low % p25519) + (z_high*38    % p25519)) % p25519    since 2^256 = p25519+p25519+38
	// = (z_low + z_high*38) % p25519
	//
	// This is what we will compute.

	// Multiply z_high by 38 while accumulating the sum into z_low
	MOVW X, Z
	ADIW X, 32

	EOR R7, R7 // initialize carry to 0
	LDI R18, 31 // Loop 32 times
	LDI R19, 38
	mul_32_by_32_mod_p25519_mul38_top:
		MOV R4, R19
		LD R5, X+
		RCALL mul_8_by_8
		// z_high[i]*38 is now in R2:R3

		// Add in the existing byte of z_low
		LD R6, Z
		ADD R2, R6
		ADC R3, R0

		// Add in the carry
		ADD R2, R7
		ADC R3, R0

		ST Z+, R2
		MOV R7, R3

		DEC R18
		BRPL mul_32_by_32_mod_p25519_mul38_top

	// R2:R7 now contain the upper two bytes of z_low+38*z_high. The number
	// of times we subtract off p25519 is controlled by the upper bit of R2
	// and the lower bits of R7, since each bit being on there corresponds
	// to 2**n copies of p25519 needing to be subtracted off.
	//
	// (For example, if the high bit of R2 is set, that represents 2**255,
	// which accounts for one p25519 being subtracted off. The lowest it of
	// R7 being set represents 2**256, which accounts for two p25519s being
	// subtracted off.)
	ADD R2, R2 // Double R2 in order to set the carry bit to its upper bit
	ADC R7, R7 // Double R7 to left-shift it one
	
	// Total subtrahend will be accumulated into R2:R3
	EOR R2, R2
	EOR R3, R3
	// The lower bytes of the potential subtrahend will in R16:R17
	LDI R16, 0xed
	LDI R17, 0xff

	LDI R18, 5 // TODO: This may need to be just 4
	mul_32_by_32_mod_p25519_reducer_top:
		// Turn the lower bit of R7 into a mask
		MOV R19, R7
		ANDI R19, 1
		NEG R19

		ASR R7

		// Duplicate the potential subtrahend lower bits
		MOV R4, R16
		MOV R5, R17

		// Mask the potential subtrahend lower bits
		AND R4, R19
		AND R5, R19

		// Accumulate into our total subtrahend
		ADD R2, R4
		ADC R3, R5
		
		// Double the subtrahend for the next iteration
		ADD R16, R16
		ADC R17, R17

		DEC R18
		BRPL mul_32_by_32_mod_p25519_reducer_top

	SBIW Z, 32
	LD R4, Z
	SUB R4, R2
	ST Z+, R4
	LD R4, Z
	SBC R4, R3
	ST Z+, R4

	LDI R19, 28
	mul_32_by_32_mod_p25519_subtractor_top:
		LD R4, Z
		SBC R4, R18 // R18 is 0xff, since we used to as a loop counter
		ST Z+, R4

		DEC R19
		BRPL mul_32_by_32_mod_p25519_subtractor_top

	LD R18, Z
	ANDI R18, 0x7f
	ST Z+, R18

	SBIW Z, 32
	RCALL maybe_subtract_p25519

	// Now we have to reduce this 33-byte quantity modulo p25519.
	// We can bound how large the quantity is. z_high is, at most, the upper half of (p25519-1)*(p25519-1)
	// z_low could be up to 2**256-1.
	// Their product could be, at most, 38 * ((p25519-1)*(p25519-1)/(2**256)) + (2**256-1)
	// = 1215816936991820051947495342591223032459334838989225922414304632083087861218567
	// This is less than p25519*21, so we can bound how many copies of p25519 we need to subtract off.

	// For convenience, we will actually handle up to 31 copies of p25519.

	RET

// Mask in R10: 0xff to swap, 0x00 to not swap.
// Clobbers R2, R3, R4, R18
maybe_swap:
	LDI R18, 31

	maybe_swap_loop_top:
		LD R2, X
		LD R3, Y
		MOV R4, R2
		EOR R4, R3
		AND R4, R10
		EOR R2, R4
		EOR R3, R4
		ST X+, R2
		ST Y+, R3

		DEC R18
		BRPL maybe_swap_loop_top
	SBIW X, 32
	SBIW Y, 32
	RET

mainloop:

	// TODO: Some setup
	LDI R27, 1
	LDI R29, 1
	LDI R31, 1

	LDI R20, 254
	mainloop_top:
		// TODO: Compute mask into R20
		
		// maybeswap(a, b)
		LDI R26, 0
		LDI R28, 32
		RCALL maybe_swap

		// maybeswap(c, d)
		LDI R26, 64
		LDI R28, 96
		RCALL maybe_swap

		// e = a + c
		LDI R26, 0
		LDI R28, 64
		LDI R30, 128
		RCALL add_32_to_32

		// a = a - c
		LDI R28, 64
		LDI R30, 0
		RCALL sub_32_from_32

		// c = b + d
		LDI R26, 32
		LDI R28, 96
		LDI R20, 64
		RCALL add_32_to_32

		// b = b - d
		LDI R28, 96
		LDI R30, 32
		RCALL sub_32_from_32


		DEC R20
		BRPL mainloop_top
