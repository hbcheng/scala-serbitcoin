package xyz.bcheng.bitcoinser

import java.nio.ByteBuffer

// TODO: This is pretty rough, rethink this
trait Field[A] {
  // getLength must return a length (int). If no read is necessary,
  // the Field should return its configured length without reading.
  // If getLength is called on a buffer whose length has already been read,
  // the behavior is undefined.
  def getLength(buf: ByteBuffer): Int

  // The primary access points of Field are read and skip.
  
  // read returns the field type. If the length is not yet known, it is read
  // from the buffer.
  // Buffer's cursor is expected to be advanceable. This may require providing
  // Field with a copy of the buffer. However, Buffer's cursor must not be 
  // assumed to be set at 0.
  def read(buf: ByteBuffer): A

  def skip(buf: ByteBuffer) {
    buf.position(getLength(buf) + buf.position)
  }

  // Read the raw underlying bytes corresponding to this field.
  protected def readBytes(buf: ByteBuffer): Array[Byte] = {
    val dst = new Array[Byte](getLength(buf))
    buf.get(dst)
    dst
  }
}

trait VariableLengthField[A] extends Field[A] {
  def getVarLength(buf: ByteBuffer): Long = {
    BinUtil.readVarInt(buf)
  }
}

class Uint16() extends Field[Int] {
  def getLength(b: ByteBuffer): Int = {
    2
  }

  def read(buf: ByteBuffer): Int = {
    BinUtil.readUint16(buf)
  }
}

class Uint32() extends Field[Long] {
  def getLength(b: ByteBuffer): Int = {
    4
  }
  def read(buf: ByteBuffer): Long = {
    BinUtil.readUint32(buf)
  }
}

class ByteArray() extends VariableLengthField[Array[Byte]] {
  def getLength(buf: ByteBuffer): Int = {
    getVarLength(buf).toInt
  }
  def read(buf: ByteBuffer): Array[Byte] = {
    readBytes(buf)
  }
}

class TxHashReader() extends Field[TransactionHash] {
  def getLength(b: ByteBuffer): Int = {
    32
  }

  def read(buf: ByteBuffer): TransactionHash = {
    TransactionHash(readBytes(buf))
  }
}
