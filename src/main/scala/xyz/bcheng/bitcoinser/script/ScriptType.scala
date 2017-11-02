package xyz.bcheng.bitcoinser.script

sealed abstract class ScriptType

// Allows matching on parent type
class P2SH(val scriptHash: List[Byte]) extends ScriptType { }
object P2SH {
  def unapply(p2sh: P2SH): Option[List[Byte]] = {
    Some(p2sh.scriptHash)
  }
}

case class P2SHWPKH(legacyScriptHash: List[Byte], pubkey: List[Byte]) extends P2SH(legacyScriptHash)
case class P2SHWP2SH(legacyScriptHash: List[Byte], witnessScriptHash: List[Byte]) extends P2SH(legacyScriptHash)
case class P2SHMultisig(rawScriptHash: List[Byte], n: Int, m: Int, pubkeys: List[List[Byte]]) extends P2SH(rawScriptHash)
case class P2PKH(pubkeyHash: List[Byte]) extends ScriptType
case class P2PK(pubkey: List[Byte]) extends ScriptType

// Enable pattern matching on witness types
class NativeWitness() extends ScriptType

object NativeWitness {
  def unapply(nw: NativeWitness): Boolean = {
    true
  }
}

case class P2WPKH(pubkeyHash: List[Byte]) extends NativeWitness
case class P2WSH(witnessScriptHash: List[Byte]) extends NativeWitness
case class Unknown(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]]) extends ScriptType 

