package xyz.bcheng.bitcoinser

/**
 * @author hbcheng
 */

import java.nio.ByteBuffer
import java.nio.ByteOrder

import org.bitcoinj.core.Utils

object BitcoinSer {
  def readTransaction(tx: Array[Byte], hash: Option[TransactionHash]): RawTransaction = {
    new RawTransaction(tx, hash)
  }
}

object TransactionHash {
  def apply(h: Seq[Byte]): TransactionHash = {
    new TransactionHash(h.toList)
  }
  val zeroHash = new TransactionHash(List.fill(32)(0.toByte))
}

@SerialVersionUID(10L)
class TransactionHash(val h: List[Byte]) {
  // byte-order reversal.
  // Over the network as little endian; common representation is big-endian
  def reverse(): TransactionHash = {
    new TransactionHash(h.reverse.toList)
  }

  override def equals(other: Any): Boolean = 
    other match {
      case otherHash: TransactionHash => h.sameElements(otherHash.h)
      case rawHash: Seq[_] => h.sameElements(rawHash)
      // defined separately because other:Any doesn't seem to trigger
      // WrappedArray, which causes Seq not to match. Is there a better way?
      case rawArray: Array[Byte] =>  h.sameElements(rawArray)
      case _ => false
    }
}

object TransactionType extends Enumeration {
  type TransactionType = Value
  val Unknown, Legacy, Segwit = Value
}

abstract class TransactionType
case class Unknown() extends TransactionType
case class Legacy() extends TransactionType
case class Segwit(ver: Int) extends TransactionType


@SerialVersionUID(10L)
class RawTransaction(payload: Array[Byte], cachedHash: Option[TransactionHash]) extends RawDataBacked(ByteBuffer.wrap(payload, 0, payload.length).asReadOnlyBuffer().order(ByteOrder.LITTLE_ENDIAN)) {
  var tType: TransactionType = Unknown()
  def this(payload: Array[Byte]) = this(payload, None)

  def inputs(): Iterator[Input] = {
    var buf = backingBuffer.slice()
    for (i <- 0 until getInputOffset()) {
      buf.get()
    }
    new InputIterator(buf)
  }

  def getInputOffset(): Int = {
    tType match {
      case Unknown() => detectTransactionType()
        getInputOffset()
      case Legacy()  => 4
      case Segwit(_) =>  6
    }
  }

  def detectTransactionType() {
    backingBuffer.get(4) match {
      case 0x00 => tType = Segwit(1)
      case _ => tType = Legacy()
    }
  }
}

class InputIterator(backing: ByteBuffer) extends RawDataBacked(backing) with Iterator[Input] {
  val inputArrayLength = readVarInt()
  var inputArrayPos = 0

  def hasNext(): Boolean = {
    inputArrayPos < inputArrayLength
  }
  def next(): Input = {
    inputArrayPos += 1
    readInput()
  }
  def readInput(): Input = {
    new Input(new OutputReference(new TransactionHash(readHash()).reverse(), readUInt()),
        readVarByteArray(), readUInt())
  }
}

// Things that extend RawDataBacked are views of an underlying backingArray,
// and fields requested via accessors are interpreted on demand. The backing
// array may be shared between multiple instances and cannot be modified.
// RawDataBacked provides seek* and read* helper methods to interact with the
// array.
@SerialVersionUID(10L)
class RawDataBacked(val backingBuffer: ByteBuffer) extends java.io.Serializable {
  // A bitcoin varint can be anything from 1 byte to 8. 
  // There is overhead of up to a single byte for longer numbers.
  
  // These methods access via absolute position
  def readVarIntAt(pos: Int): Long = {
    backingBuffer.get(pos) match {
      case 0xFD => backingBuffer.getChar(pos+1).toLong
      case 0xFE => readUIntAt(pos+1)
      case 0xFF => backingBuffer.getLong(pos+1)
      case x => x & 0xffl
    }
  }

  def readHashAt(pos: Int): List[Byte] = {
    var dst = new Array[Byte](32)
    backingBuffer.get(dst, pos, 32)
    dst.toList
  }

  def readUIntAt(pos: Int): Long = {
    var bytes = new Array[Byte](4)
    backingBuffer.get(bytes, pos, 4)
    Utils.readUint32(bytes,0)
  }

  // These methods advance the reader cursor
  def readVarInt(): Long = {
    backingBuffer.get() match {
      case 0xFD => backingBuffer.getChar().toLong
      case 0xFE => readUInt()
      case 0xFF => backingBuffer.getLong()
      case x => x & 0xffl
    }
  }

  def readHash(): List[Byte] = {
    var dst = new Array[Byte](32)
    backingBuffer.get(dst)
    dst.toList
  }

  def readUInt(): Long = {
    var bytes = new Array[Byte](4)
    backingBuffer.get(bytes)
    Utils.readUint32(bytes,0)
  }

  def readVarByteArray(): List[Byte] = {
    val len = readVarInt()
    var bytes = new Array[Byte](len.toInt)
    backingBuffer.get(bytes)
    bytes.toList
  }
}


@SerialVersionUID(10L)
case class Input(outputRef: OutputReference, script: List[Byte], sequence: Long) 

@SerialVersionUID(10L)
case class OutputReference(txHash: TransactionHash, index: Long) 
