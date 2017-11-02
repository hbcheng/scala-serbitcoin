package xyz.bcheng.bitcoinser.script

import org.bitcoinj.core.{Base58, Sha256Hash, Utils}

class Analyzer(addrPrefix: Byte, p2shAddrPrefix: Byte) {
  def extractScriptType(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]]): ScriptType = {
    new Unknown(scriptPubKey, scriptSig)  
  }

  def extractAddress(stype: ScriptType): Option[String] = {
    stype match {
      case Unknown(spk, ss) => {
        extractScriptType(spk, ss) match {
          case Unknown(_, _) => None
          case st => extractAddress(st)
        }
      }
      case P2SH(sh) => scriptHashToAddress(sh)
      case P2PKH(pkh) => pubkeyHashToAddress(pkh)
      case P2PK(pk) => pubkeyToAddress(pk)
      case P2WPKH(_) | P2WSH(_) => {
        // At this time, don't know what to do with native witness.
        // We could extract a legacy address, but this would further blur the
        // line between a public key and address
        None
      }
    }
  }
 
  // A P2SH address is the scriptHash, plus the bit indicating the type,
  // and a checksum
  def scriptHashToAddress(sh: List[Byte]): Option[String] = {
    Some(Base58.encode((addressBytes(sh, p2shAddrPrefix).toArray)))
  }

  def pubkeyHashToAddress(pkh: List[Byte]): Option[String] = {
    Some(Base58.encode(addressBytes(pkh, addrPrefix).toArray))
  }

  def pubkeyToAddress(pk: List[Byte]): Option[String] = {
    pubkeyHashToAddress(addressHash(pk))
  }

  // Pubkey to pubkey hash
  def addressHash(pk: List[Byte]): List[Byte] = {
    // Not yet implemented
    Utils.sha256hash160(pk.toArray).toList
  }

  def addressBytes(payload: List[Byte], prefix: Byte): List[Byte] = {
    val checksum = Sha256Hash.twiceOf((prefix :: payload).toArray).getBytes().toList
    prefix :: payload ::: checksum.slice(0, 4)
  }
}
