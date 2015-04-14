package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import shapeless.syntax.singleton._
import shipreq.base.util.{UnivEq, Must, Util}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Presentation, Grammar}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import TC.{Query, Strategy}

object AutoComplete {

  def optionallyPrefixedStrategy[A](prefixRegex      : String,
                                    mainRegex        : String,
                                    searchFn         : Query[A])
                                   (replacementA     : A => String,
                                    replacementPrefix: String => String,
                                    replacementEnd   : String)
                                   (prefix           : Boolean): Strategy.B3[A] =
    if (prefix)
      Strategy(s"$prefixRegex$mainRegex", index = 1)
        .search(searchFn)
        .replace(s => replacementPrefix(replacementA(s)) + replacementEnd)
    else
      Strategy(s"(^|\\s)$prefixRegex?$mainRegex", index = 2)
        .search(searchFn)
        .replace(s => "$1" + replacementA(s) + replacementEnd)

  // ===================================================================================================================
  // #ISSUE #TAG

  def hashtag(legal: Stream[HashRefKey], prefix: Boolean): Strategy = {
    import Grammar.{hashRefKey => G}
    val prefixRegex = Util.regexEscapeAndWrap(G.prefix)
    val mainRegex   = s"(${G.firstChar.one}${G.allChars.*})$$"
    val searchFn    = TC.caseInsensitiveContains(legal.map(_.value).sorted)
    optionallyPrefixedStrategy(prefixRegex, mainRegex, searchFn)(identity, G.prefix + _, " ")(prefix)
  }

  def hashtag(legalIssues: Stream[CustomIssueType], legalTags: Stream[ApplicableTag], prefix: Boolean): Strategy =
    hashtag(legalIssues.map(_.key) append legalTags.map(_.key), prefix)

  def tag(legal: Stream[ApplicableTag], prefix: Boolean): Strategy =
    hashtag(legal.map(_.key), prefix)

  def issue(legal: Stream[CustomIssueType], prefix: Boolean): Strategy =
    hashtag(legal.map(_.key), prefix)

  // ===================================================================================================================
  // [REF]

  def reqItems(p: Project, reqDesc: Req => String): Stream[ReqItem] =
    reqItems(p, reqDesc, p.reqs.data.reqs.values.toStream)

  def reqItems(p: Project, reqDesc: Req => String, legal: Stream[Req]): Stream[ReqItem] = {
    val m = Must.foldMapM[Req, Stream, ReqItem](legal)(req =>
      p.reqType(req.pubid.reqTypeId).map(rt =>
        new ReqItem(req, rt, reqDesc(req))))
    mustResolve(m)(Stream.empty).sortBy(_.sortKey)
  }

  def req(legal: Stream[ReqItem], prefix: Boolean): Strategy = {
    val prefixRegex = Util.regexEscapeAndWrap(Grammar.reflinkPrefix)
    val suffixRegex = Util.regexEscapeAndWrap(Grammar.reflinkSuffix)
    val mainRegex = s"(\\S+?)$suffixRegex?$$"

    // TODO [pri=low] Search algorithm won't scale well
    val searchFn: TC.Query[ReqItem] = { term =>
      val np = normaliseReqPubid(term)
      val nd = normaliseReqDesc(term)
      legal.filter(i => i.pubidStrNorm.contains(np) || i.descNorm.contains(nd))
    }

    def li(i: ReqItem): ReactElement =
      *.reqAutoComplete('req)(r => _('desc)(d =>
        <.div(
          <.div(r, i.pubidStr),
          <.div(d, i.desc))
      ))

    optionallyPrefixedStrategy(prefixRegex, mainRegex, searchFn)(
      _.pubidStr, Grammar.reflinkPrefix + _ + Grammar.reflinkSuffix, " ")(prefix)
      .template(i => React.renderToStaticMarkup(li(i)))
  }

  final class ReqItem(val req: Req, val reqType: ReqType, val desc: String) {
    val pubidStr     = Presentation.pubid(reqType, req.pubid.pos)
    val pubidStrNorm = normaliseReqPubid(pubidStr)
    val descNorm     = normaliseReqDesc(desc)
    val sortKey      = (reqType.mnemonic.value, req.pubid.pos.value)
  }

  def normaliseReqPubid(s: String): String =
    s.replace("-", "").toUpperCase

  def normaliseReqDesc(s: String): String =
    normaliseReqPubid(s).replace(" ", "")
}
