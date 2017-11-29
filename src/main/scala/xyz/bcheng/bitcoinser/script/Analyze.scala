package xyz.bcheng.bitcoinser.script

import org.bitcoinj.core.{Base58, Sha256Hash, Utils}

object Analyzer {
  // Having None for script pub key or script sig means that the relevant script
  // is not known, _NOT_ that it is empty. An empty script should have an empty
  // list.
  def extractScriptType(scriptPubKey: Option[List[Byte]], scriptSig: Option[List[Byte]], witness: Option[List[List[Byte]]]): ScriptType = {
    (guessScriptPubKeyType(scriptPubKey) match {
      case Some(st) => st
      case None => Unknown(scriptPubKey, None, None)
    }).refineWith(scriptPubKey, scriptSig, witness)
  }

  def guessScriptPubKeyType(scriptPubKey: Option[List[Byte]]): Option[ScriptType] = {
    if (scriptPubKey.isEmpty) {
      None
    } else {
      val spk = scriptPubKey.get
      spk.length match {
        case 25 => tryPayToPubkeyHash(spk)
        case 23 => tryPayToScriptHash(spk)
        case 35 | 67 => tryPayToPubkey(spk)
        case _ => None //checkRawMultisig(spk)
      }
    }
  }

  // Expects length to be verified to be 25.
  private def tryPayToPubkeyHash(scriptPubKey: List[Byte]) : Option[ScriptType] = {
    println("Entered tryPayToPubkeyHash")
    println(scriptPubKey)
    if (scriptPubKey(0) == OpCodes.OP_DUP && scriptPubKey(1) == OpCodes.OP_HASH160 && scriptPubKey(2) == 0x14.toByte && scriptPubKey(23) == OpCodes.OP_EQUALVERIFY && scriptPubKey(24) == OpCodes.OP_CHECKSIG) {
      println("matched")
      Some(new P2PKH(scriptPubKey.slice(3,23)))
    } else {
      println("not matched")
      None
    }
  }

  // Expects length verified to 23
  private def tryPayToScriptHash(scriptPubKey: List[Byte]) : Option[ScriptType] = {
    if (scriptPubKey(0) == OpCodes.OP_HASH160 && scriptPubKey(1) == 0x14.toByte && scriptPubKey(22) == OpCodes.OP_EQUAL) {
      Some(new P2SH(scriptPubKey.slice(2,22)))
    } else {
      None
    }
  }

  private def tryPayToPubkey(scriptPubKey: List[Byte]) : Option[ScriptType] = {
    if (scriptPubKey(scriptPubKey.length - 1) == OpCodes.OP_CHECKSIG) {
      Some(new P2PK(scriptPubKey.slice(1, scriptPubKey.length-1)))
    } else {
      None
    }
  }

  val bitcoinAnalyzer = new Analyzer(0, 5)
}

@SerialVersionUID(10L)
class Analyzer(addrPrefix: Byte, p2shAddrPrefix: Byte) extends Serializable {
  def extractAddress(stype: ScriptType): Option[String] = {
    stype match {
      case Unknown(spk, ss, wit) => {
        Analyzer.extractScriptType(spk, ss, wit) match {
          case Unknown(_, _, _) => None
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
