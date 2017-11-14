import org.scalatest.FunSpec

import org.apache.commons.codec.binary.Hex
import xyz.bcheng.bitcoinser.script.{Analyzer, ScriptType, P2PKH, P2SH}

// This test is intended to exercise end-to-end analysis using the ScriptType
// API.
class ScriptTypeSpec extends FunSpec {
  describe("Building an address analyzer for bitcoin") {
    val analyzer = new Analyzer(0, 5)

    describe("and preparing a P2PKH scriptPubKey") {
      val scriptPubKey = Hex.
        decodeHex("76a91482a5ffffb97f6e419a8f1b1e5b7c0a0d32d289d288ac".toArray).
        toList

      describe("when analyzing the script") {
        val p2pkhScriptType = Analyzer.
          extractScriptType(Some(scriptPubKey), None, None)
        it("should produce the correct ScriptType") {
          assert(p2pkhScriptType.isInstanceOf[P2PKH])
        }
        it("and should recover the correct address") {
          assert(analyzer.extractAddress(p2pkhScriptType) == Some("1CuohpZ2XGG7RtMV4CgCg6QGEkWeRWuDvf"))
        }

        // TODO: Exercise what happens when we complete this ScriptType with a
        // corresponding spending input
      }
    }
    describe("and preparing a P2SH scriptPubKey") {
      val scriptPubKey = Hex.decodeHex("a91401282dbb43f4fbe58bd9ddb4bf963bf86eb5c67687".toArray).toList
      describe("when analyzing the script") {
        val p2shScriptType = Analyzer.
          extractScriptType(Some(scriptPubKey), None, None)

        it("should produce the correct script type") {
          assert(p2shScriptType.isInstanceOf[P2SH])
        }
        it("should recover the correct address") {
          assert(analyzer.extractAddress(p2shScriptType) == Some("31o8jPwYhSjGUMSzeZDxhKgrvn7btGfxLX"))
        }
      }
    }
  }
}

