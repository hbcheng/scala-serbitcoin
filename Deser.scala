package xyz.bcheng.bitcoinser

/**
 * @author hbcheng
 */

import java.nio.ByteBuffer

object BitcoinSer {
  def readTransaction(tx: Array[Byte], hash: Option[TransactionHash]): RawTransaction = {
    new RawTransaction(tx, hash)
  }
}

class TransactionHash(h: Array[Byte]) {
}

object TransactionType extends Enumeration {
  type TransactionType = Value
  val Unknown, Legacy, Segwit = Value
}

abstract class TransactionType
case class Unknown() extends TransactionType
case class Legacy() extends TransactionType
case class Segwit(ver: Int) extends TransactionType


class RawTransaction(payload: Array[Byte], cachedHash: Option[TransactionHash]) extends RawDataBacked(ByteBuffer.wrap(payload, 0, payload.length).asReadOnlyBuffer()) {
  var tType: TransactionType = Unknown()
  def this(payload: Array[Byte]) = this(payload, None)

  def Inputs(): Iterator[Input] = {
    new InputIterator(backingBuffer)
  }
}

class InputIterator(backing: ByteBuffer) extends RawDataBacked(backing) with Iterator[Input] {
  val inputArrayLength = readVarInt()
  var inputArrayPos = 0

  def hasNext(): Boolean = {
    inputArrayPos < inputArrayLength
  }
  def next(): Input = {
    readInput()
  }
  def readInput(): Input = {
    // Placeholder
    new Input(new OutputReference(new TransactionHash(readHash()), 0), new Array[Byte](0), 0)
  }
}

// Things that extend RawDataBacked are views of an underlying backingArray,
// and fields requested via accessors are interpreted on demand. The backing
// array may be shared between multiple instances and cannot be modified.
// RawDataBacked provides seek* and read* helper methods to interact with the
// array.
class RawDataBacked(val backingBuffer: ByteBuffer) {
  // A bitcoin varint can be anything from 1 byte to 8. 
  // There is overhead of up to a single byte for longer numbers.
  def readVarInt(): Long = {
    backingBuffer.get() match {
      case 0xFD => backingBuffer.getChar().toLong
      case 0xFE => backingBuffer.getInt().toLong
      case 0xFF => backingBuffer.getLong()
      case x => x.toLong
    }
  }
  def readHash(): Array[Byte] = {
    var dst = new Array[Byte](32)
    backingBuffer.get(dst)
    dst
  }
}


case class Input(outputRef: OutputReference, script: Array[Byte], Sequence: Int) 
case class OutputReference(txHash: TransactionHash, Index: Int) 
