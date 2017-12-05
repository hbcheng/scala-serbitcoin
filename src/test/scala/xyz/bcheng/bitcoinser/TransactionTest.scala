import org.scalatest.FunSpec
import org.apache.commons.codec.binary.Hex

import xyz.bcheng.bitcoinser.{TransactionHash, RawTransaction, Legacy}

import scala.io.Source
import java.io.File

class BasicTransactionSpec extends FunSpec {
  println("Entered BasicTransactionSpec")
  describe("A Transaction") {
    describe("when backed by a valid transaction") {
      val txBytes = Hex.decodeHex("01000000022642531f6e9c63233ec5b2b663f1ebd0445dfd2c38f8125bceb6bdbdf071d33a010000006b483045022100ea8b2aaec83a79a012a3084e3f06dedf28d69caacbf2e06bd10d7e5a8a550b2802205f187e965c7d8ce1dc3c60da39ec29daf0dd5bf003a6aa155e18b6eb24c99280012103463408c7ea8db1c2d7187647523686f6f1e745ef11c97621ab5ddbb8974520a8ffffffff6a405cbb2c83b58feb69e36688c8a83d13982165323d2356fdec203173324024210000006b483045022100b629e5ade7f99c15628fe8aa5dce4627ac1ee714cf53a88ca19d8b98712c1ec4022035f1732ca358158f977788226d769d7c982d86fa5dd2895cf6753cc633c262c6012102523ad3a0c8456a0342567ca6432725168fd36756245e3eecdb62a79200f9bb58ffffffff0153ab530d000000001976a914d243d38e7183ec200e66256acb2579e12fdcd9b788ac00000000".toArray)

      val tx = new RawTransaction(txBytes)

      it("should have the expected transaction type") {
        tx.detectTransactionType()
        assert(tx.tType == Legacy())
      }

      describe("when reading out inputs") {
        val inIter = tx.inputs()
        val firstIn = inIter.next()
        val secondIn = inIter.next()
        it("should produce the expected results") {
          // Note: this fails if transaction hash test fails.
          val firstInRef = Hex.decodeHex("3ad371f0bdbdb6ce5b12f8382cfd5d44d0ebf163b6b2c53e23639c6e1f534226".toArray)
          val firstInHash = TransactionHash(firstInRef)
          assert(firstIn.outputRef.txHash == firstInHash)
          assert(firstIn.outputRef.index == 1L)
          assert(firstIn.sequence == 4294967295L)
          assert(firstIn.script.sameElements(Hex.decodeHex("483045022100ea8b2aaec83a79a012a3084e3f06dedf28d69caacbf2e06bd10d7e5a8a550b2802205f187e965c7d8ce1dc3c60da39ec29daf0dd5bf003a6aa155e18b6eb24c99280012103463408c7ea8db1c2d7187647523686f6f1e745ef11c97621ab5ddbb8974520a8".toArray)))

          val secondInRef = Hex.decodeHex("244032733120ecfd56233d32652198133da8c88866e369eb8fb5832cbb5c406a".toArray)
          val secondInHash = TransactionHash(secondInRef)
          assert(secondIn.outputRef.txHash == secondInHash)
          assert(secondIn.outputRef.index == 33L)
          assert(secondIn.sequence == 4294967295L)
          assert(secondIn.script.sameElements(Hex.decodeHex("483045022100b629e5ade7f99c15628fe8aa5dce4627ac1ee714cf53a88ca19d8b98712c1ec4022035f1732ca358158f977788226d769d7c982d86fa5dd2895cf6753cc633c262c6012102523ad3a0c8456a0342567ca6432725168fd36756245e3eecdb62a79200f9bb58".toArray)))
        }
        it("should be the right length") {
          assert(inIter.hasNext == false)
        }
      }
      describe("when reading out outputs") {
        val outIter = tx.outputs()
        val out = outIter.next()
        it("should produce the expected results") {
          assert(out.value == 223587155)
          assert(out.script.sameElements(Hex.decodeHex("76a914d243d38e7183ec200e66256acb2579e12fdcd9b788ac".toArray)))
        }
        it("should be the right length") {
          assert(outIter.hasNext == false)
        }

      }
    }
  }
}

class LargeTransactionSpec extends FunSpec {
  describe("A RawTransaction, loaded from a large example") {
    val f = new File(getClass.getClassLoader.getResource("large-tx.txt").getPath)
    val dataFile = Source.fromFile(f)
    val txString = try dataFile.mkString finally dataFile.close()
    val txBytes = Hex.decodeHex(txString.trim.toArray)

    // This is tx 0f5007ea2f309b3609c08a6caab8986e04e7f064db2e2b68446c27c6e7c52232
    val tx = new RawTransaction(txBytes)

    it("should have the expected transaction type") {
      tx.detectTransactionType()
      assert(tx.tType == Legacy())
    }

    describe("when reading out inputs") {
      val inArray = tx.inputs().toArray
      
      it("should be the right length") {
        assert(inArray.length == 381)
      }
    }
    describe("when reading out outputs") {
      val outArray = tx.outputs().toArray
      it("should be the right length") {
        assert(outArray.length == 2)
      }

    }
  }
}
