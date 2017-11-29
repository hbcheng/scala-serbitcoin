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
class TransactionHash(val h: List[Byte]) extends Serializable {
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
class RawTransaction(payload: Array[Byte], cachedHash: Option[TransactionHash]) extends RawDataBacked(ByteBuffer.wrap(payload, 0, payload.length).asReadOnlyBuffer().order(ByteOrder.LITTLE_ENDIAN)) with Serializable {
  var tType: TransactionType = Unknown()
  def this(payload: Array[Byte]) = this(payload, None)

  def size(): Int = {
    payload.length
  }

  def inputs(): Iterator[Input] = {
    inputIterator()
  }

  def inputIterator(): InputIterator = {
    var buf = backingBuffer.slice()
    buf.position(getInputOffset()).asInstanceOf[ByteBuffer].
      order(ByteOrder.LITTLE_ENDIAN)
    new InputIterator(buf)
  }
  def outputs(): Iterator[Output] = {
    var buf = backingBuffer.slice()
    buf.position(getOutputOffset()).asInstanceOf[ByteBuffer].
      order(ByteOrder.LITTLE_ENDIAN)

    new OutputIterator(buf) 
  }

  def getInputOffset(): Int = {
    tType match {
      case Unknown() => detectTransactionType()
        getInputOffset()
      case Legacy()  => 4
      case Segwit(_) =>  6
    }
  }

  def getOutputOffset(): Int = {
    var buf = backingBuffer.slice()
    buf.position(getInputOffset()).asInstanceOf[ByteBuffer].
      order(ByteOrder.LITTLE_ENDIAN)
    println("before:", buf.position)
    // make an input iterator, to handle skipping past (variable length) inputs
    val ii = new InputIterator(buf)
    ii.skipAll()
    println("after:", buf.position)
    buf.position
  }

  def detectTransactionType() {
    backingBuffer.get(4) match {
      case 0x00 => tType = Segwit(1)
      case _ => tType = Legacy()
    }
  }
}

class InputIterator(backing: ByteBuffer) extends Iterator[Input] {
  val inputArrayLength = BinUtil.readVarInt(backing)
  var inputArrayPos = 0

  def hasNext(): Boolean = {
    inputArrayPos < inputArrayLength
  }
  def next(): Input = {
    inputArrayPos += 1
    readInput()
  }
  def skipAll() {
    for (i <- 0 until inputArrayLength.toInt) {
      skipInput()
    }
  }

  def skipInput() {
    new TxHashReader().skip(backing) 
    new Uint32().skip(backing) 
    new ByteArray().skip(backing) 
    new Uint32().skip(backing) 
  }
  
  def readInput(): Input = {
    new Input(new OutputReference(new TxHashReader().read(backing).reverse(),
      new Uint32().read(backing)), new ByteArray().read(backing).toList,
      new Uint32().read(backing))
  }
}

class OutputIterator(backing: ByteBuffer) extends Iterator[Output] {
  val outputArrayLength = BinUtil.readVarInt(backing)
  var outputArrayPos = 0

  def hasNext(): Boolean = {
    outputArrayPos < outputArrayLength
  }

  def next(): Output = {
    outputArrayPos += 1
    readOutput()
  }

  def readOutput(): Output = {
    new Output(backing.getLong(), new ByteArray().read(backing).toList)
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

@SerialVersionUID(10L)
case class Output(value: Long, script: List[Byte])
