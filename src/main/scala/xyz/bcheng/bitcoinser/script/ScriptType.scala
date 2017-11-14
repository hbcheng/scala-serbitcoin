package xyz.bcheng.bitcoinser.script

sealed abstract class ScriptType {
  // A ScriptType consists of three script-based components: scriptPubKey,
  // scriptSig, and witness. ScriptType may be instantiated in an incomplete
  // manner- for example, if the output is unspent, or the spending reference
  // comes from a separate source. In those situations, the incomplete script
  // can then be refined into a more specific type. 
  def refineWith(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]): ScriptType
}

// A LeafType represents a ScriptType that is as specific as possible; it 
// cannot be refined further.
trait LeafType extends ScriptType {
  override def refineWith(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]): ScriptType = {
    this
  }
}

// Represents a P2SH scriptPubKey where we don't know anything further at this
// time. 
class P2SH(val scriptHash: List[Byte]) extends ScriptType { 
  // Since we initially just need to know the address, we will just return
  // P2SH.
  def refineWith(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]): ScriptType = {
    this
  }
  /*
    scriptSig match {
      case S
      case Some(x) => guessP2SHType(x)
      case None => this
    }
  }
  def guessP2SHType(scriptSig: List[Byte]): ScriptType = {

  }
  */
}
// This extractor allows matching on parent type in cases where the specific 
// subtype does not matter (eg. if we're talking about legacy addresses only)
object P2SH {
  def unapply(p2sh: P2SH): Option[List[Byte]] = {
    Some(p2sh.scriptHash)
  }
}

case class P2SHWPKH(legacyScriptHash: List[Byte], pubkey: List[Byte]) extends P2SH(legacyScriptHash) with LeafType 

case class P2SHWP2SH(legacyScriptHash: List[Byte], witnessScriptHash: List[Byte]) extends P2SH(legacyScriptHash) with LeafType 

case class P2SHMultisig(rawScriptHash: List[Byte], n: Int, m: Int, pubkeys: List[List[Byte]]) extends P2SH(rawScriptHash) with LeafType

case class P2PKH(pubkeyHash: List[Byte]) extends LeafType
case class P2PK(pubkey: List[Byte]) extends LeafType

// Enable pattern matching on witness types
class NativeWitness() extends ScriptType {
  //TODO: Implement me
  def refineWith(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]): ScriptType = {
    this
  }
}

object NativeWitness {
  def unapply(nw: NativeWitness): Boolean = {
    true
  }
}

case class P2WPKH(pubkeyHash: List[Byte]) extends NativeWitness with LeafType
case class P2WSH(witnessScriptHash: List[Byte]) extends NativeWitness with LeafType

case class Unknown(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]) extends ScriptType {
  def refineWith(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]): ScriptType = {
    this
  }
}

