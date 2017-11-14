import org.scalatest.FunSpec

import org.apache.commons.codec.binary.Hex
import xyz.bcheng.bitcoinser.script.{Analyzer, ScriptType, P2SH, P2SHMultisig, P2PKH}

// This test exercises the base level analysis routines only, not the
// scripttype wrappers or any other components
// The test values here have already been extracted properly from the enclosing
// scripts
class BaseAddressParseSpec extends FunSpec {
  describe("Building an address analyzer for bitcoin") {
    val analyzer = new Analyzer(0, 5)

    describe("and parsing a few addresses") {
      it("should recover p2pkh addresses") {
        val pkh = Hex.decodeHex("42622c5f3412ff9ff6bb848e4ee332acd9fa6632".toArray)
        assert(analyzer.pubkeyHashToAddress(pkh.toList) == Some("1741Cs4oXbP9XGBYh7cAYNiskaA2xNRJfb"))

        val pkh2 = Hex.decodeHex("104398aa8b6e078a535c294e5857f1bcf37ea4cc".toArray)
        assert(analyzer.pubkeyHashToAddress(pkh2.toList) == Some("12UznkHAHPrsw9qCJkr2zbww7ydi343a82"))

        val pkh3 = Hex.decodeHex("d5e02146552e40bff9348fbaf976b5408c80125e".toArray)
        assert(analyzer.pubkeyHashToAddress(pkh3.toList) == Some("1LVsRgfWV64qHYEVhAcrCeYts1wV3WXHSj"))

      }
      it("should recover p2sh addresses") {
        val sh = Hex.decodeHex("8eb64ebbb6b3da6a11486508c4a2a1de7b6ce171".toArray)
        assert(analyzer.scriptHashToAddress(sh.toList) == Some("3EhcGjYuJPyCQMXQ31Pcqw2ydno63FUwSH"))

        val sh2 = Hex.decodeHex("02a71669ac8e64baaf0811219a8e006721448151".toArray)
        assert(analyzer.scriptHashToAddress(sh2.toList) == Some("31w3SNe6a4LKR6Epdru6ECVxz3cP48664k"))
      }

      it("should recover old p2pk addresses") {
        val pk = Hex.decodeHex("04851659a1420612c046fcd0085d884f185ce8b5da448c1b5e8735b4b13c5626841a2a172ba770df5e0ea0354562d939284fad9baef0cd1dbc43b69a5a963409c5".toArray)
        assert(analyzer.pubkeyToAddress(pk.toList) == Some("19B32mm7b5x6HiBCRVpgQZUswbQ29b9YTo"))
      }
    }
  }
  describe("Building an address analyzer for litecoin") {
    // using legacy addresses for now
    val liteAnalyzer = new Analyzer(48, 5)
    it("should recover litecoin addresses") {
      val pkh = Hex.decodeHex("166297ee8cbf2d5449cc6af9cbd406412d41409e".toArray)
      assert(liteAnalyzer.pubkeyHashToAddress(pkh.toList) == Some("LMGKEq1kq4jm3JenCFFxFUNb595zfRyB1p"))
    }
  }
}

// This test builds on BaseAddressParseSpec with information from script 
// type examination.
// This is not an end-to-end test; it leverages prebuilt ScriptType types
// with properly instantiated values.
class ScriptAddressParseSpec extends FunSpec {
  describe("Building an address analyzer for bitcoin") {
    val analyzer = new Analyzer(0, 5)
    describe("and preparing a P2SH transaction ScriptType") {
      val scriptHash = Hex.decodeHex("6a1ecdb5a9fd365b03d90df5e6ac30cfc08e9091".toArray)
      val p2shExample = new P2SH(scriptHash.toList)
      it("should recover the correct address") {
        assert(analyzer.extractAddress(p2shExample) == Some("3BN8T9RzfrjFHzDJ5jG5xNt4EeTMnDaTFa"))
      }
      describe("and preparing a P2SH-derived child ScriptType") {
        // Mocking the rest of the values, which should not matter for
        // address extraction
        val msigExample = new P2SHMultisig(scriptHash.toList, 2, 3, List[List[Byte]]())
        it("should still recover the correct address") {
          assert(analyzer.extractAddress(msigExample) == Some("3BN8T9RzfrjFHzDJ5jG5xNt4EeTMnDaTFa"))
        }

      }
    }
    describe("and preparing a P2PKH transaction ScriptType") {
      val addressHash = Hex.decodeHex("5e11f5ccdb0326dfc884bd71417df56fd8a2101b".toArray)
      val p2pkhExample = new P2PKH(addressHash.toList)
      it("should extract the correct address") {
        assert(analyzer.extractAddress(p2pkhExample) == Some("19aQ2vgtjc5TEcYneMXbeVHk1xBwTfzSyQ"))
      }
    }
  }
}
