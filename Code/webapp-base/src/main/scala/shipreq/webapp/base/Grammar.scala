package shipreq.webapp.base

import shipreq.webapp.base.validation.Rules

object Grammar {
  final class RegexChar(chars: String, ruleErrMsg: String) {
    @inline def one  = "[" + chars + "]"
    @inline def not  = "[^" + chars + "]"
    @inline def *    = "[" + chars + "]*"
    @inline def +    = "[" + chars + "]+"
    @inline def rule = Rules.whitelistCharsR(chars, ruleErrMsg)
  }

  /** [[shipreq.webapp.base.data.ReqType.Mnemonic]] min & max lengths. */
  final val reqTypeMnemonicLength = 1 to 6

  final val reqTypeMnemonicChars =
    new RegexChar("A-Z", "may only consist of letters.")

  /** [[shipreq.webapp.base.data.HashRefKey]] min & max lengths. */
  final val hashRefKeyLength = 1 to 20

  // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
  // Must not contain: []{}<>#
  final val hashRefKeyChars =
    new RegexChar("""A-Za-z0-9\._=\-""", "may only consist of letters, numbers, and these symbols: . _ = -")

  /** [[shipreq.webapp.base.data.FieldRefKey]] min & max lengths. */
  final val fieldRefKeyLength = hashRefKeyLength

  // DD-20: Field refkeys must match this format: /[a-z][a-z0-9_]*/
  // Must not contain: []{}<>.?"
  final val fieldRefKeyChars =
    new RegexChar("""a-z0-9_""", "may only consist of letters, numbers, and underscores.")

  /** [[shipreq.webapp.base.data.ReqCode.Node]] min & max lengths. */
  final val reqCodeNodeLength = hashRefKeyLength

}
