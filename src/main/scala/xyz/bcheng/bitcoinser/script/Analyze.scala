package xyz.bcheng.bitcoinser.script

import org.bitcoinj.core.{Base58, Sha256Hash}

class Analyze(addrPrefix: Byte, p2shAddrPrefix: Byte, msingAddrPrefix: Byte) {
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
      case P2SH(sh) => extractP2SHAddress(sh)
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
  def extractP2SHAddress(sh: List[Byte]): Option[String] = {
    // Not yet implemented
    Some(Base58.encode((addressBytes(sh, p2shAddrPrefix).toArray)))
  }

  def pubkeyHashToAddress(pkh: List[Byte]): Option[String] = {
    // Not yet implemented
    Some(Base58.encode(addressBytes(pkh, addrPrefix).toArray))
  }

  def pubkeyToAddress(pk: List[Byte]): Option[String] = {
    pubkeyHashToAddress(addressHash(pk))
  }

  // Pubkey to pubkey hash
  def addressHash(pk: List[Byte]): List[Byte] = {
    // Not yet implemented
    pk
  }

  def addressBytes(payload: List[Byte], prefix: Byte): List[Byte] = {
    prefix :: payload ::: Sha256Hash.twiceOf(payload.toArray).getBytes().toList
  }
}