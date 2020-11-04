package shipreq.webapp.base.util

import shipreq.base.util.Util
import shipreq.webapp.base.util.CharSubset.EscapeChars

/** Scala.JS regex doesn't support Java-style unicode char selectors like "\p{S}".
  *
  * This exists a solution designed to minimise output JS size impact.
  */
final class CharSubset(_direct: List[Int], _ranges: List[Range]) {

  private var direct = _direct
  private var ranges = _ranges

  private var initRemaining = 2 // Number of dependent lazy vals

  def unsafeConsume[A](a: A): A = {
    assert(initRemaining > 0)
    initRemaining -= 1
    if (initRemaining == 0) {
      direct = null
      ranges = null
    }
    a
  }

  def unsafeForeach(f: Int => Unit): Unit = {
    direct foreach f
    ranges foreach (_ foreach f)
  }

  /** Example: `"a-zA-Z"` */
  lazy val regexCharRange: String =
    unsafeConsume(
      Util.quickSB { sb =>
        val addChar: Int => Unit = i => {
          if (EscapeChars.contains(i))
            sb append '\\'
          sb append i.toChar
        }

        direct foreach addChar

        ranges foreach { r =>
          addChar(r.start)
          sb append '-'
          addChar(r.end)
        }
      }
    )

  /** Example: `"[a-zA-Z]"` */
  lazy val regexChar: String =
    "[" + regexCharRange + "]"

  /** Example: `"[^a-zA-Z]"` */
  lazy val regexCharNot: String =
    "[^" + regexCharRange + "]"

  /** A regex string that returns the middle, so long as a char in this range isn't on either side.
    *
    * Because JS doesn't support look-behind, the char prior to the middle is captured.
    */
  def regexNotAround(middleRegex: String): String =
    s"(?:(^|$regexCharNot)(?:$middleRegex)(?!$regexChar))"

  /** Creates a function that replaces all occurrences of the middle, so long as a char in this range isn't on either
    * side.
    */
  def notAroundReplaceAll(middleRegex: String, replacement: String): String => String = {
    val r = regexNotAround(middleRegex).r
    val to = "$1" + replacement
    r.replaceAllIn(_, to)
  }
}

object CharSubset {
  private val EscapeChars: Set[Int] =
    """-[]\""".toCharArray.map(_.toInt).toSet

  /** `\p{javaWhitespace}` */
  val Whitespace =
    new CharSubset(5760::6158::8232::8233::8287::12288::Nil, (9 to 13)::(28 to 32)::(8192 to 8198)::(8200 to 8202)::Nil)

  /** `\p{S}\p{P}` */
  val PunctuationOrSymbol =
    new CharSubset(171::172::180::187::191::215::247::749::885::894::900::901::903::1014::1154::1417::1418::1423::1470::1472::1475::1478::1523::1524::1563::1566::1567::1748::1758::1769::1789::1790::2142::2404::2405::2416::2546::2547::2554::2555::2800::2801::2928::3199::3449::3572::3647::3663::3674::3675::3892::3894::3896::3973::4254::4255::4347::5120::5741::5742::5787::5788::5941::5942::6464::6468::6469::6686::6687::7294::7295::7379::8125::8189::8190::8448::8449::8456::8457::8468::8485::8487::8489::8494::8506::8507::8527::11518::11519::11632::12336::12342::12343::12443::12444::12448::12539::12688::12689::12880::42238::42239::42611::42622::42784::42785::42889::42890::43214::43215::43310::43311::43359::43486::43487::43742::43743::43760::43761::44011::64297::64830::64831::65020::65021::65532::65533::Nil, (33 to 47)::(58 to 64)::(91 to 96)::(123 to 126)::(161 to 169)::(174 to 177)::(182 to 184)::(706 to 709)::(722 to 735)::(741 to 747)::(751 to 767)::(1370 to 1375)::(1542 to 1551)::(1642 to 1645)::(1792 to 1805)::(2038 to 2041)::(2096 to 2110)::(3059 to 3066)::(3841 to 3863)::(3866 to 3871)::(3898 to 3901)::(4030 to 4037)::(4039 to 4044)::(4046 to 4058)::(4170 to 4175)::(4960 to 4968)::(5008 to 5017)::(5867 to 5869)::(6100 to 6102)::(6104 to 6107)::(6144 to 6154)::(6622 to 6655)::(6816 to 6822)::(6824 to 6829)::(7002 to 7018)::(7028 to 7036)::(7164 to 7167)::(7227 to 7231)::(7360 to 7367)::(8127 to 8129)::(8141 to 8143)::(8157 to 8159)::(8173 to 8175)::(8208 to 8231)::(8240 to 8286)::(8314 to 8318)::(8330 to 8334)::(8352 to 8378)::(8451 to 8454)::(8470 to 8472)::(8478 to 8483)::(8512 to 8516)::(8522 to 8525)::(8592 to 9203)::(9216 to 9254)::(9280 to 9290)::(9372 to 9449)::(9472 to 9983)::(9985 to 10101)::(10132 to 11084)::(11088 to 11097)::(11493 to 11498)::(11513 to 11516)::(11776 to 11822)::(11824 to 11835)::(11904 to 11929)::(11931 to 12019)::(12032 to 12245)::(12272 to 12283)::(12289 to 12292)::(12296 to 12320)::(12349 to 12351)::(12694 to 12703)::(12736 to 12771)::(12800 to 12830)::(12842 to 12871)::(12896 to 12927)::(12938 to 12976)::(12992 to 13054)::(13056 to 13311)::(19904 to 19967)::(42128 to 42182)::(42509 to 42511)::(42738 to 42743)::(42752 to 42774)::(43048 to 43051)::(43062 to 43065)::(43124 to 43127)::(43256 to 43258)::(43457 to 43469)::(43612 to 43615)::(43639 to 43641)::(64434 to 64449)::(65040 to 65049)::(65072 to 65106)::(65108 to 65126)::(65128 to 65131)::(65281 to 65295)::(65306 to 65312)::(65339 to 65344)::(65371 to 65381)::(65504 to 65510)::(65512 to 65518)::Nil)
}

