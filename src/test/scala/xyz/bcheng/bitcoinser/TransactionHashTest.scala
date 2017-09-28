import org.scalatest.FunSpec
import org.apache.commons.codec.binary.Hex

import xyz.bcheng.bitcoinser.TransactionHash

class TransactionHashSpec extends FunSpec {
  println("Entered TransactionHashSpec")
  describe("A TransactionHash") {
    println("1")
    describe("when initialized from a Sequence of Bytes") {
      val bs: Seq[Byte] = new Array[Byte](32)
      val txhAllZeroes = TransactionHash(bs)
      it("should equal another TransactionHash with the same byte values") {
        val zb: Byte = 0
        val ns: Seq[Byte] = List.fill(32)(zb)
        val txhOther = TransactionHash(ns)
        assert(txhAllZeroes == txhOther)
      }
      it("should equal another sequence with the same byte values") {
        val zs: Seq[Byte] = new Array[Byte](32)
        assert(txhAllZeroes == zs)
      }
      it("should not equal another TransactionHash with different values") {
        val ob: Byte = 1
        val os: Seq[Byte] = List.fill(32)(ob)
        val txhOnes = TransactionHash(os)
        assert(txhAllZeroes != txhOnes)
      }
    }
    describe("when initialized from a realistic byte array") {
      val hash = Hex.decodeHex("9256332b9ca52cbcb06f57296dfd982d8da3f7d4696b4c10cf9bb93dae6edf58".toArray)
      val txHash = TransactionHash(hash)
      describe("and reversed") {
        val reversed = txHash.reverse
        it("should produce a new TransactionHash") {
          assert(txHash != reversed)
          assert(!txHash.h.sameElements(reversed.h))
        }
        it("should produce the desired result") {
          val expected = Hex.decodeHex("58df6eae3db99bcf104c6b69d4f7a38d2d98fd6d29576fb0bc2ca59c2b335692".toArray)
          assert(reversed == expected)
        }
      }
    }
  }
}

