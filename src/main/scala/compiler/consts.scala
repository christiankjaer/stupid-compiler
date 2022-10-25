package compiler

val wordSize = 8
val charShift = 8
val charTag = 0x0f
val charMask = 0xff

val intShift = 2
val intMask = 0x3
val intTag = 0x0

val falseVal = 0x2f
val trueVal = 0x6f
val unitVal = 0x3f

val boolShift = 6
val boolMask = 0xbf
val boolTag = 0x2f
