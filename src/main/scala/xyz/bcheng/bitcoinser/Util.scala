package xyz.bcheng.bitcoinser

import java.nio.ByteBuffer
import org.bitcoinj.core.Utils

object BinUtil {

  // ReadVarInt will return the incorrect value for 64-bit variable length
  // ints with the highest significant bit set, as it will be interpreted
  // as a negative. However, doing this any other way would require upgrading
  // to a bigint or raw bytefield.
  def readVarInt(buf: ByteBuffer): Long = {
    buf.get() match {
      case 0xFD => readUint16(buf)
      case 0xFE => readUint32(buf)
      case 0xFF => buf.getLong()
      case x => x & 0xffl
    }
  }

  def readHash(buf: ByteBuffer) : List[Byte] = {
    var dst = new Array[Byte](32)
    buf.get(dst)
    dst.toList
  }

  // Unsigned reads proceed below.
  // All unsigned reads incur an allocation penalty due to needing to upgrade
  // their types to the next larger size to accomodate the differnece in range 
  // relative to the signed type.
  // If the actual value is not necessary, consider using the signed version;
  // the bytes will be represented faithfully, even if the value is not.


  // Read the next 4 bytes of the buffer as a uint32.
  // Byte order is determined by buf.
  def readUint32(buf: ByteBuffer): Long = {
    var raw = buf.getInt()

    // Bears a little explanation, since its somewhat counter-intuitive.
    // This implicitly converts the Int to a Long, which moves the sign bit
    // up to the first bit and fills the remainder with 0's (positive numbers)
    // or 1's (negative numbers). This keeps the lower significance bits 
    // unchanged. Then we mask off just the original bits, removing the sign
    // bit and retaining the value as if it had been interpreted as an 
    // unsigned value,
    raw & 0xffffffffl
  }

  // Read the next 16 bytes of the buffer as a uint16.
  // Byte order is determined by buf.
  def readUint16(buf: ByteBuffer): Int = {
    var raw = buf.getShort()

    // see readUint32
    raw & 0xffff
  }

  // Read an unsigned byte.
  // Note java bytes are signed, so if you need to know the actual value
  // packed inside, you will want to use this facility instead of just the
  // raw readbyte.
  def readUint8(buf: ByteBuffer): Int = {
    var raw = buf.get() 

    raw & 0xff
  }
}
