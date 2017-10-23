package xzy.bcheng.bitcoinser

abstract class ScriptType
case class P2SH() extends ScriptType
case class P2SHWPKH(pubkey: List[Byte]) extends P2SH
case class P2SHWP2SH() extends P2SH
case class P2SHMultisig(n: Int, m: Int, pubkeys: List[List[Byte]]) extends P2SH
case class P2PKH(pubkey: List[Byte]) extends ScriptType
case class P2PK(pubkey: List[Byte]) extends ScriptType
case class NativeWitness() extends ScriptType
case class P2WPKH(pubkey: List[Byte]) extends NativeWitness
case class P2WSH() extends NativeWitness
