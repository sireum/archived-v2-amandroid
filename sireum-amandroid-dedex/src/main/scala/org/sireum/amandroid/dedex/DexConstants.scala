/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.dedex

import org.sireum.util._

/**
 * @author fgwei
 */
trait DexConstants {
  object InstructionType extends Enumeration {
    val
      UNKNOWN_INSTRUCTION,
      REGCONST4,
      REGSTRINGCONST,
      REGSTRINGCONST_JUMBO,
      METHODINVOKE,
      METHODINVOKE_STATIC,
      QUICKMETHODINVOKE,
      INLINEMETHODINVOKE,
      INLINEMETHODINVOKE_RANGE,
      NEWARRAY,
      FILLARRAYDATA,
      ONEREGFIELD_READ,
      ONEREGFIELD_READ_WIDE,
      ONEREGFIELD_READ_OBJECT,
      ONEREGFIELD_WRITE,
      TWOREGSFIELD_READ,
      TWOREGSFIELD_READ_WIDE,
      TWOREGSFIELD_READ_OBJECT,
      TWOREGSFIELD_WRITE,
      NOPARAMETER,
      REGCONST16,
      REGCONST16_WIDE,
      THREEREGS,
      THREEREGS_WIDE,
      AGET,
      APUT,
      PACKEDSWITCH,
      SPARSESWITCH,
      ONEREG,
      MOVERESULT,
      OFFSET8,
      NEWINSTANCE,
      TWOREGSTYPE,
      REGOFFSET16,
      OFFSET16,
      TWOREGSOFFSET16,
      MOVE,
      MOVE_OBJECT,
      TWOREGSPACKED_SINGLE,
      TWOREGSPACKED_DOUBLE,
      TWOREGSCONST8,
      REGCLASSCONST,
      REGCONST32,
      REGCONST32_WIDE,
      REGCONST64,
      REG8REG16,
      REG8REG16_OBJECT,
      REG16REG16,
      REG16REG16_OBJECT,
      TWOREGSPACKEDCONST16,
      METHODINVOKE_RANGE,
      METHODINVOKE_RANGE_STATIC,
      QUICKMETHODINVOKE_RANGE,
      FILLEDARRAY,
      FILLEDARRAY_RANGE,
      TWOREGSQUICKOFFSET,
      TWOREGSQUICKOFFSET_WIDE,
      TWOREGSQUICKOFFSET_OBJECT,
      TWOREGSQUICKOFFSET_WRITE,
      CHECKCAST = Value
  }

  val instructionTypes: IList[InstructionType.Value] = List(
    InstructionType.NOPARAMETER,                        // 0
    InstructionType.MOVE,                               // 1
    InstructionType.REG8REG16,                          // 2
    InstructionType.REG16REG16,                         // 3
    InstructionType.MOVE,                               // 4
    InstructionType.REG8REG16,                          // 5
    InstructionType.REG16REG16,                         // 6
    InstructionType.MOVE_OBJECT,                        // 7
    InstructionType.REG8REG16_OBJECT,                   // 8
    InstructionType.REG16REG16_OBJECT,                  // 9
    InstructionType.MOVERESULT,                         // a
    InstructionType.MOVERESULT,                         // b
    InstructionType.MOVERESULT,                         // c
    InstructionType.MOVERESULT,                         // d
    InstructionType.NOPARAMETER,                        // e
    InstructionType.ONEREG,                             // f
    InstructionType.ONEREG,                             // 10
    InstructionType.ONEREG,                             // 11
    InstructionType.REGCONST4,                          // 12
    InstructionType.REGCONST16,                         // 13
    InstructionType.REGCONST32,                         // 14
    InstructionType.REGCONST16,                         // 15
    InstructionType.REGCONST16_WIDE,                    // 16
    InstructionType.REGCONST32_WIDE,                    // 17
    InstructionType.REGCONST64,                         // 18
    InstructionType.REGCONST16_WIDE,                    // 19
    InstructionType.REGSTRINGCONST,                     // 1a
    InstructionType.REGSTRINGCONST_JUMBO,               // 1b
    InstructionType.REGCLASSCONST,                      // 1c
    InstructionType.ONEREG,                             // 1d
    InstructionType.ONEREG,                             // 1e
    InstructionType.CHECKCAST,                          // 1f
    InstructionType.TWOREGSTYPE,                        // 20
    InstructionType.TWOREGSPACKED_SINGLE,               // 21
    InstructionType.NEWINSTANCE,                        // 22
    InstructionType.NEWARRAY,                           // 23
    InstructionType.FILLEDARRAY,                        // 24
    InstructionType.FILLEDARRAY_RANGE,                  // 25
    InstructionType.FILLARRAYDATA,                      // 26
    InstructionType.ONEREG,                             // 27
    InstructionType.OFFSET8,                            // 28
    InstructionType.OFFSET16,                           // 29
    InstructionType.UNKNOWN_INSTRUCTION,                // 2a
    InstructionType.PACKEDSWITCH,                       // 2b
    InstructionType.SPARSESWITCH,                       // 2c
    InstructionType.THREEREGS,                          // 2d
    InstructionType.THREEREGS,                          // 2e
    InstructionType.THREEREGS,                          // 2f
    InstructionType.THREEREGS,                          // 30
    InstructionType.THREEREGS,                          // 31
    InstructionType.TWOREGSOFFSET16,                    // 32
    InstructionType.TWOREGSOFFSET16,                    // 33
    InstructionType.TWOREGSOFFSET16,                    // 34
    InstructionType.TWOREGSOFFSET16,                    // 35
    InstructionType.TWOREGSOFFSET16,                    // 36
    InstructionType.TWOREGSOFFSET16,                    // 37
    InstructionType.REGOFFSET16,                        // 38
    InstructionType.REGOFFSET16,                        // 39
    InstructionType.REGOFFSET16,                        // 3a
    InstructionType.REGOFFSET16,                        // 3b
    InstructionType.REGOFFSET16,                        // 3c
    InstructionType.REGOFFSET16,                        // 3d
    InstructionType.UNKNOWN_INSTRUCTION,                // 3e
    InstructionType.UNKNOWN_INSTRUCTION,                // 3f
    InstructionType.UNKNOWN_INSTRUCTION,                // 40
    InstructionType.UNKNOWN_INSTRUCTION,                // 41
    InstructionType.UNKNOWN_INSTRUCTION,                // 42
    InstructionType.UNKNOWN_INSTRUCTION,                // 43
    InstructionType.AGET,                               // 44
    InstructionType.AGET,                               // 45
    InstructionType.AGET,                               // 46
    InstructionType.AGET,                               // 47
    InstructionType.AGET,                               // 48
    InstructionType.AGET,                               // 49
    InstructionType.AGET,                               // 4a
    InstructionType.APUT,                               // 4b
    InstructionType.APUT,                               // 4c
    InstructionType.APUT,                               // 4d
    InstructionType.APUT,                               // 4e
    InstructionType.APUT,                               // 4f
    InstructionType.APUT,                               // 50
    InstructionType.APUT,                               // 51
    InstructionType.TWOREGSFIELD_READ,                  // 52
    InstructionType.TWOREGSFIELD_READ_WIDE,             // 53
    InstructionType.TWOREGSFIELD_READ_OBJECT,           // 54
    InstructionType.TWOREGSFIELD_READ,                  // 55
    InstructionType.TWOREGSFIELD_READ,                  // 56
    InstructionType.TWOREGSFIELD_READ,                  // 57
    InstructionType.TWOREGSFIELD_READ,                  // 58
    InstructionType.TWOREGSFIELD_WRITE,                 // 59
    InstructionType.TWOREGSFIELD_WRITE,                 // 5a
    InstructionType.TWOREGSFIELD_WRITE,                 // 5b
    InstructionType.TWOREGSFIELD_WRITE,                 // 5c
    InstructionType.TWOREGSFIELD_WRITE,                 // 5d
    InstructionType.TWOREGSFIELD_WRITE,                 // 5e
    InstructionType.TWOREGSFIELD_WRITE,                 // 5f
    InstructionType.ONEREGFIELD_READ,                   // 60
    InstructionType.ONEREGFIELD_READ_WIDE,              // 61
    InstructionType.ONEREGFIELD_READ_OBJECT,            // 62
    InstructionType.ONEREGFIELD_READ,                   // 63
    InstructionType.ONEREGFIELD_READ,                   // 64
    InstructionType.ONEREGFIELD_READ,                   // 65
    InstructionType.ONEREGFIELD_READ,                   // 66
    InstructionType.ONEREGFIELD_WRITE,                  // 67
    InstructionType.ONEREGFIELD_WRITE,                  // 68
    InstructionType.ONEREGFIELD_WRITE,                  // 69
    InstructionType.ONEREGFIELD_WRITE,                  // 6a
    InstructionType.ONEREGFIELD_WRITE,                  // 6b
    InstructionType.ONEREGFIELD_WRITE,                  // 6c
    InstructionType.ONEREGFIELD_WRITE,                  // 6d
    InstructionType.METHODINVOKE,                       // 6e
    InstructionType.METHODINVOKE,                       // 6f
    InstructionType.METHODINVOKE,                       // 70
    InstructionType.METHODINVOKE_STATIC,                // 71
    InstructionType.METHODINVOKE,                       // 72
    InstructionType.UNKNOWN_INSTRUCTION,                // 73
    InstructionType.METHODINVOKE_RANGE,                 // 74
    InstructionType.METHODINVOKE_RANGE,                 // 75
    InstructionType.METHODINVOKE_RANGE,                 // 76
    InstructionType.METHODINVOKE_RANGE_STATIC,          // 77
    InstructionType.METHODINVOKE_RANGE,                 // 78
    InstructionType.UNKNOWN_INSTRUCTION,                // 79
    InstructionType.UNKNOWN_INSTRUCTION,                // 7a
    InstructionType.TWOREGSPACKED_SINGLE,               // 7b
    InstructionType.UNKNOWN_INSTRUCTION,                // 7c
    InstructionType.TWOREGSPACKED_DOUBLE,               // 7d
    InstructionType.UNKNOWN_INSTRUCTION,                // 7e
    InstructionType.TWOREGSPACKED_SINGLE,               // 7f
    InstructionType.TWOREGSPACKED_DOUBLE,               // 80
    InstructionType.TWOREGSPACKED_DOUBLE,               // 81
    InstructionType.TWOREGSPACKED_SINGLE,               // 82
    InstructionType.TWOREGSPACKED_DOUBLE,               // 83
    InstructionType.TWOREGSPACKED_SINGLE,               // 84
    InstructionType.TWOREGSPACKED_SINGLE,               // 85
    InstructionType.TWOREGSPACKED_DOUBLE,               // 86
    InstructionType.TWOREGSPACKED_SINGLE,               // 87
    InstructionType.TWOREGSPACKED_DOUBLE,               // 88
    InstructionType.TWOREGSPACKED_DOUBLE,               // 89
    InstructionType.TWOREGSPACKED_SINGLE,               // 8a
    InstructionType.TWOREGSPACKED_DOUBLE,               // 8b
    InstructionType.TWOREGSPACKED_SINGLE,               // 8c
    InstructionType.TWOREGSPACKED_SINGLE,               // 8d
    InstructionType.TWOREGSPACKED_SINGLE,               // 8e
    InstructionType.TWOREGSPACKED_SINGLE,               // 8f
    InstructionType.THREEREGS,                          // 90
    InstructionType.THREEREGS,                          // 91
    InstructionType.THREEREGS,                          // 92
    InstructionType.THREEREGS,                          // 93
    InstructionType.THREEREGS,                          // 94
    InstructionType.THREEREGS,                          // 95
    InstructionType.THREEREGS,                          // 96
    InstructionType.THREEREGS,                          // 97
    InstructionType.THREEREGS,                          // 98
    InstructionType.THREEREGS,                          // 99
    InstructionType.THREEREGS,                          // 9a
    InstructionType.THREEREGS_WIDE,                     // 9b
    InstructionType.THREEREGS_WIDE,                     // 9c
    InstructionType.THREEREGS_WIDE,                     // 9d
    InstructionType.THREEREGS_WIDE,                     // 9e
    InstructionType.THREEREGS_WIDE,                     // 9f
    InstructionType.THREEREGS_WIDE,                     // a0
    InstructionType.THREEREGS_WIDE,                     // a1
    InstructionType.THREEREGS_WIDE,                     // a2
    InstructionType.THREEREGS_WIDE,                     // a3
    InstructionType.THREEREGS_WIDE,                     // a4
    InstructionType.THREEREGS_WIDE,                     // a5
    InstructionType.THREEREGS,                          // a6
    InstructionType.THREEREGS,                          // a7
    InstructionType.THREEREGS,                          // a8
    InstructionType.THREEREGS,                          // a9
    InstructionType.THREEREGS,                          // aa
    InstructionType.THREEREGS_WIDE,                     // ab
    InstructionType.THREEREGS_WIDE,                     // ac
    InstructionType.THREEREGS_WIDE,                     // ad
    InstructionType.THREEREGS_WIDE,                     // ae
    InstructionType.THREEREGS_WIDE,                     // af
    InstructionType.TWOREGSPACKED_SINGLE,               // b0
    InstructionType.TWOREGSPACKED_SINGLE,               // b1
    InstructionType.TWOREGSPACKED_SINGLE,               // b2
    InstructionType.TWOREGSPACKED_SINGLE,               // b3
    InstructionType.TWOREGSPACKED_SINGLE,               // b4
    InstructionType.TWOREGSPACKED_SINGLE,               // b5
    InstructionType.TWOREGSPACKED_SINGLE,               // b6
    InstructionType.TWOREGSPACKED_SINGLE,               // b7
    InstructionType.TWOREGSPACKED_SINGLE,               // b8
    InstructionType.TWOREGSPACKED_SINGLE,               // b9
    InstructionType.TWOREGSPACKED_SINGLE,               // ba
    InstructionType.TWOREGSPACKED_DOUBLE,               // bb
    InstructionType.TWOREGSPACKED_DOUBLE,               // bc
    InstructionType.TWOREGSPACKED_DOUBLE,               // bd
    InstructionType.TWOREGSPACKED_DOUBLE,               // be
    InstructionType.TWOREGSPACKED_DOUBLE,               // bf
    InstructionType.TWOREGSPACKED_DOUBLE,               // c0
    InstructionType.TWOREGSPACKED_DOUBLE,               // c1
    InstructionType.TWOREGSPACKED_DOUBLE,               // c2
    InstructionType.TWOREGSPACKED_DOUBLE,               // c3
    InstructionType.TWOREGSPACKED_DOUBLE,               // c4
    InstructionType.TWOREGSPACKED_DOUBLE,               // c5
    InstructionType.TWOREGSPACKED_SINGLE,               // c6
    InstructionType.TWOREGSPACKED_SINGLE,               // c7
    InstructionType.TWOREGSPACKED_SINGLE,               // c8
    InstructionType.TWOREGSPACKED_SINGLE,               // c9
    InstructionType.TWOREGSPACKED_SINGLE,               // ca
    InstructionType.TWOREGSPACKED_DOUBLE,               // cb
    InstructionType.TWOREGSPACKED_DOUBLE,               // cc
    InstructionType.TWOREGSPACKED_DOUBLE,               // cd
    InstructionType.TWOREGSPACKED_DOUBLE,               // ce
    InstructionType.TWOREGSPACKED_DOUBLE,               // cf
    InstructionType.TWOREGSPACKEDCONST16,               // d0
    InstructionType.TWOREGSPACKEDCONST16,               // d1
    InstructionType.TWOREGSPACKEDCONST16,               // d2
    InstructionType.TWOREGSPACKEDCONST16,               // d3
    InstructionType.TWOREGSPACKEDCONST16,               // d4
    InstructionType.TWOREGSPACKEDCONST16,               // d5
    InstructionType.TWOREGSPACKEDCONST16,               // d6
    InstructionType.TWOREGSPACKEDCONST16,               // d7
    InstructionType.TWOREGSCONST8,                      // d8
    InstructionType.TWOREGSCONST8,                      // d9
    InstructionType.TWOREGSCONST8,                      // da
    InstructionType.TWOREGSCONST8,                      // db
    InstructionType.TWOREGSCONST8,                      // dc
    InstructionType.TWOREGSCONST8,                      // dd
    InstructionType.TWOREGSCONST8,                      // de
    InstructionType.TWOREGSCONST8,                      // df
    InstructionType.TWOREGSCONST8,                      // e0
    InstructionType.TWOREGSCONST8,                      // e1
    InstructionType.TWOREGSCONST8,                      // e2
    InstructionType.TWOREGSFIELD_READ,                  // e3
    InstructionType.TWOREGSFIELD_WRITE,                 // e4
    InstructionType.ONEREGFIELD_READ,                   // e5
    InstructionType.ONEREGFIELD_WRITE,                  // e6
    InstructionType.TWOREGSFIELD_READ_OBJECT,           // e7
    InstructionType.TWOREGSFIELD_READ_WIDE,             // e8
    InstructionType.TWOREGSFIELD_WRITE,                 // e9
    InstructionType.ONEREGFIELD_READ_WIDE,              // ea
    InstructionType.ONEREGFIELD_WRITE,                  // eb
    InstructionType.UNKNOWN_INSTRUCTION,                // ec
    InstructionType.UNKNOWN_INSTRUCTION,                // ed
    InstructionType.INLINEMETHODINVOKE,                 // ee
    InstructionType.INLINEMETHODINVOKE_RANGE,           // ef
    InstructionType.METHODINVOKE,                       // f0
    InstructionType.NOPARAMETER,                        // f1
    InstructionType.TWOREGSQUICKOFFSET,                 // f2
    InstructionType.TWOREGSQUICKOFFSET_WIDE,            // f3
    InstructionType.TWOREGSQUICKOFFSET_OBJECT,          // f4
    InstructionType.TWOREGSQUICKOFFSET_WRITE,           // f5
    InstructionType.TWOREGSQUICKOFFSET_WRITE,           // f6
    InstructionType.TWOREGSQUICKOFFSET_WRITE,           // f7
    InstructionType.QUICKMETHODINVOKE,                  // f8
    InstructionType.QUICKMETHODINVOKE_RANGE,            // f9
    InstructionType.QUICKMETHODINVOKE,                  // fa
    InstructionType.QUICKMETHODINVOKE_RANGE,            // fb
    InstructionType.TWOREGSFIELD_WRITE,                 // fc
    InstructionType.ONEREGFIELD_READ_OBJECT,            // fd
    InstructionType.ONEREGFIELD_WRITE,                  // fe
    InstructionType.UNKNOWN_INSTRUCTION                 // ff
  )

  final val NOP = 0x0
  final val MOVE = 0x1
  final val MOVE_FROM16 = 0x2
  final val MOVE_16 = 0x3
  final val MOVE_WIDE = 0x4
  final val MOVE_WIDE_FROM16 = 0x5
  final val MOVE_WIDE_16 = 0x6
  final val MOVE_OBJECT = 0x7
  final val MOVE_OBJECT_FROM16 = 0x8
  final val MOVE_OBJECT_16 = 0x9
  final val MOVE_RESULT = 0xa
  final val MOVE_RESULT_WIDE = 0xb
  final val MOVE_RESULT_OBJECT = 0xc
  final val MOVE_EXCEPTION = 0xd
  final val RETURN_VOID = 0xe
  final val RETURN = 0xf
  final val RETURN_WIDE = 0x10
  final val RETURN_OBJECT = 0x11
  final val CONST_4 = 0x12
  final val CONST_16 = 0x13
  final val CONST = 0x14
  final val CONST_HIGH16 = 0x15
  final val CONST_WIDE_16 = 0x16
  final val CONST_WIDE_32 = 0x17
  final val CONST_WIDE = 0x18
  final val CONST_WIDE_HIGH16 = 0x19
  final val CONST_STRING = 0x1a
  final val CONST_STRING_JUMBO = 0x1b
  final val CONST_CLASS = 0x1c
  final val MONITOR_ENTER = 0x1d
  final val MONITOR_EXIT = 0x1e
  final val CHECK_CAST = 0x1f
  final val INSTANCE_OF = 0x20
  final val ARRAY_LENGTH = 0x21
  final val NEW_INSTANCE = 0x22
  final val NEW_ARRAY = 0x23
  final val FILLED_NEW_ARRAY = 0x24
  final val FILLED_NEW_ARRAY_RANGE = 0x25
  final val FILL_ARRAY_DATA = 0x26
  final val THROW = 0x27
  final val GOTO = 0x28
  final val GOTO_16 = 0x29
  final val GOTO_32 = 0x2a
  final val PACKED_SWITCH = 0x2b
  final val SPARSE_SWITCH = 0x2c
  final val CMPL_FLOAT = 0x2d
  final val CMPG_FLOAT = 0x2e
  final val CMPL_DOUBLE = 0x2f
  final val CMPG_DOUBLE = 0x30
  final val CMP_LONG = 0x31
  final val IF_EQ = 0x32
  final val IF_NE = 0x33
  final val IF_LT = 0x34
  final val IF_GE = 0x35
  final val IF_GT = 0x36
  final val IF_LE = 0x37
  final val IF_EQZ = 0x38
  final val IF_NEZ = 0x39
  final val IF_LTZ = 0x3a
  final val IF_GEZ = 0x3b
  final val IF_GTZ = 0x3c
  final val IF_LEZ = 0x3d
  // unused_3E
  // unused_3F
  // unused_40
  // unused_41
  // unused_42
  // unused_43
  final val AGET = 0x44
  final val AGET_WIDE = 0x45
  final val AGET_OBJECT = 0x46
  final val AGET_BOOLEAN = 0x47
  final val AGET_BYTE = 0x48
  final val AGET_CHAR = 0x49
  final val AGET_SHORT = 0x4a
  final val APUT = 0x4b
  final val APUT_WIDE = 0x4c
  final val APUT_OBJECT = 0x4d
  final val APUT_BOOLEAN = 0x4e
  final val APUT_BYTE = 0x4f
  final val APUT_CHAR = 0x50
  final val APUT_SHORT = 0x51
  final val IGET = 0x52
  final val IGET_WIDE = 0x53
  final val IGET_OBJECT = 0x54
  final val IGET_BOOLEAN = 0x55
  final val IGET_BYTE = 0x56
  final val IGET_CHAR = 0x57
  final val IGET_SHORT = 0x58
  final val IPUT = 0x59
  final val IPUT_WIDE = 0x5a
  final val IPUT_OBJECT = 0x5b
  final val IPUT_BOOLEAN = 0x5c
  final val IPUT_BYTE = 0x5d
  final val IPUT_CHAR = 0x5e
  final val IPUT_SHORT = 0x5f
  final val SGET = 0x60
  final val SGET_WIDE = 0x61
  final val SGET_OBJECT = 0x62
  final val SGET_BOOLEAN = 0x63
  final val SGET_BYTE = 0x64
  final val SGET_CHAR = 0x65
  final val SGET_SHORT = 0x66
  final val SPUT = 0x67
  final val SPUT_WIDE = 0x68
  final val SPUT_OBJECT = 0x69
  final val SPUT_BOOLEAN = 0x6a
  final val SPUT_BYTE = 0x6b
  final val SPUT_CHAR = 0x6c
  final val SPUT_SHORT = 0x6d
  final val INVOKE_VIRTUAL = 0x6e
  final val INVOKE_SUPER = 0x6f
  final val INVOKE_DIRECT = 0x70
  final val INVOKE_STATIC = 0x71
  final val INVOKE_INTERFACE = 0x72
  // unused_73
  final val INVOKE_VIRTUAL_RANGE = 0x74
  final val INVOKE_SUPER_RANGE = 0x75
  final val INVOKE_DIRECT_RANGE = 0x76
  final val INVOKE_STATIC_RANGE = 0x77
  final val INVOKE_INTERFACE_RANGE = 0x78
  // unused_79
  // unused_7a
  final val NEG_INT = 0x7b
  final val NOT_INT = 0x7c
  final val NEG_LONG = 0x7d
  final val NOT_LONG = 0x7e
  final val NEG_FLOAT = 0x7f
  final val NEG_DOUBLE = 0x80
  final val INT_TO_LONG = 0x81
  final val INT_TO_FLOAT = 0x82
  final val INT_TO_DOUBLE = 0x83
  final val LONG_TO_INT = 0x84
  final val LONG_TO_FLOAT = 0x85
  final val LONG_TO_DOUBLE = 0x86
  final val FLOAT_TO_INT = 0x87
  final val FLOAT_TO_LONG = 0x88
  final val FLOAT_TO_DOUBLE = 0x89
  final val DOUBLE_TO_INT = 0x8a
  final val DOUBLE_TO_LONG = 0x8b
  final val DOUBLE_TO_FLOAT = 0x8c
  final val INT_TO_BYTE = 0x8d
  final val INT_TO_CHAR = 0x8e
  final val INT_TO_SHORT = 0x8f
  final val ADD_INT = 0x90
  final val SUB_INT = 0x91
  final val MUL_INT = 0x92
  final val DIV_INT = 0x93
  final val REM_INT = 0x94
  final val AND_INT = 0x95
  final val OR_INT = 0x96
  final val XOR_INT = 0x97
  final val SHL_INT = 0x98
  final val SHR_INT = 0x99
  final val USHR_INT = 0x9a
  final val ADD_LONG = 0x9b
  final val SUB_LONG = 0x9c
  final val MUL_LONG = 0x9d
  final val DIV_LONG = 0x9e
  final val REM_LONG = 0x9f
  final val AND_LONG = 0xa0
  final val OR_LONG = 0xa1
  final val XOR_LONG = 0xa2
  final val SHL_LONG = 0xa3
  final val SHR_LONG = 0xa4
  final val USHR_LONG = 0xa5
  final val ADD_FLOAT = 0xa6
  final val SUB_FLOAT = 0xa7
  final val MUL_FLOAT = 0xa8
  final val DIV_FLOAT = 0xa9
  final val REM_FLOAT = 0xaa
  final val ADD_DOUBLE = 0xab
  final val SUB_DOUBLE = 0xac
  final val MUL_DOUBLE = 0xad
  final val DIV_DOUBLE = 0xae
  final val REM_DOUBLE = 0xaf
  final val ADD_INT_2ADDR = 0xb0
  final val SUB_INT_2ADDR = 0xb1
  final val MUL_INT_2ADDR = 0xb2
  final val DIV_INT_2ADDR = 0xb3
  final val REM_INT_2ADDR = 0xb4
  final val AND_INT_2ADDR = 0xb5
  final val OR_INT_2ADDR = 0xb6
  final val XOR_INT_2ADDR = 0xb7
  final val SHL_INT_2ADDR = 0xb8
  final val SHR_INT_2ADDR = 0xb9
  final val USHR_INT_2ADDR = 0xba
  final val ADD_LONG_2ADDR = 0xbb
  final val SUB_LONG_2ADDR = 0xbc
  final val MUL_LONG_2ADDR = 0xbd
  final val DIV_LONG_2ADDR = 0xbe
  final val REM_LONG_2ADDR = 0xbf
  final val AND_LONG_2ADDR = 0xc0
  final val OR_LONG_2ADDR = 0xc1
  final val XOR_LONG_2ADDR = 0xc2
  final val SHL_LONG_2ADDR = 0xc3
  final val SHR_LONG_2ADDR = 0xc4
  final val USHR_LONG_2ADDR = 0xc5
  final val ADD_FLOAT_2ADDR = 0xc6
  final val SUB_FLOAT_2ADDR = 0xc7
  final val MUL_FLOAT_2ADDR = 0xc8
  final val DIV_FLOAT_2ADDR = 0xc9
  final val REM_FLOAT_2ADDR = 0xca
  final val ADD_DOUBLE_2ADDR = 0xcb
  final val SUB_DOUBLE_2ADDR = 0xcc
  final val MUL_DOUBLE_2ADDR = 0xcd
  final val DIV_DOUBLE_2ADDR = 0xce
  final val REM_DOUBLE_2ADDR = 0xcf
  final val ADD_INT_LIT16 = 0xd0
  final val RSUB_INT = 0xd1
  final val MUL_INT_LIT16 = 0xd2
  final val DIV_INT_LIT16 = 0xd3
  final val REM_INT_LIT16 = 0xd4
  final val AND_INT_LIT16 = 0xd5
  final val OR_INT_LIT16 = 0xd6
  final val XOR_INT_LIT16 = 0xd7
  final val ADD_INT_LIT8 = 0xd8
  final val RSUB_INT_LIT8 = 0xd9
  final val MUL_INT_LIT8 = 0xda
  final val DIV_INT_LIT8 = 0xdb
  final val REM_INT_LIT8 = 0xdc
  final val AND_INT_LIT8 = 0xdd
  final val OR_INT_LIT8 = 0xde
  final val XOR_INT_LIT8 = 0xdf
  final val SHL_INT_LIT8 = 0xe0
  final val SHR_INT_LIT8 = 0xe1
  final val USHR_INT_LIT8 = 0xe2
  final val IGET_VOLATILE = 0xe3
  final val IPUT_VOLATILE = 0xe4
  final val SGET_VOLATILE = 0xe5
  final val SPUT_VOLATILE = 0xe6
  final val IGET_OBJECT_VOLATILE = 0xe7
  final val IGET_WIDE_VOLATILE = 0xe8
  final val IPUT_WIDE_VOLATILE = 0xe9
  final val SGET_WIDE_VOLATILE = 0xea
  final val SPUT_WIDE_VOLATILE = 0xeb
  // unused_EC
  // unused_ED
  final val EXECUTE_INLINE = 0xee
  final val EXECUTE_INLINE_RANGE = 0xef
  final val INVOKE_DIRECT_EMPTY = 0xf0
  final val RETURN_VOID_BARRIER = 0xf1
  final val IGET_QUICK = 0xf2
  final val IGET_WIDE_QUICK = 0xf3
  final val IGET_OBJECT_QUICK = 0xf4
  final val IPUT_QUICK = 0xf5
  final val IPUT_WIDE_QUICK = 0xf6
  final val IPUT_OBJECT_QUICK = 0xf7
  final val INVOKE_VIRTUAL_QUICK = 0xf8
  final val INVOKE_VIRTUAL_QUICK_RANGE = 0xf9
  final val INVOKE_SUPER_QUICK = 0xfa
  final val INVOKE_SUPER_QUICK_RANGE = 0xfb
  final val IPUT_OBJECT_VOLATILE = 0xfc
  final val SGET_OBJECT_VOLATILE = 0xfd
  final val SPUT_OBJECT_VOLATILE = 0xfe
  // unused_FF
}