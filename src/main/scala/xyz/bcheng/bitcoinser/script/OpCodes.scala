package xyz.bcheng.bitcoinser.script

// Not a comprehensive list; just opcodes utilized in script analysis.

object OpCodes {
  val OP_DUP:         Byte = 0x76.toByte
  val OP_EQUAL:       Byte = 0x87.toByte
  val OP_EQUALVERIFY: Byte = 0x88.toByte
  val OP_HASH160:     Byte = 0xa9.toByte
  val OP_CHECKSIG:    Byte = 0xac.toByte
}
