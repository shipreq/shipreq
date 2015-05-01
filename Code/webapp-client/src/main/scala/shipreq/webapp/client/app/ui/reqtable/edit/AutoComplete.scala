package shipreq.webapp.client.app.ui.reqtable
package edit

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import shapeless.syntax.singleton._
import shipreq.base.util.{UnivEq, Must, Util}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText, Grammar}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import TC.{Query, Strategy, StrategyA}

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

  def reqItems(p: Project, pt: PlainText.ForProject): Stream[ReqItem] =
    reqItems(p, pt, p.reqs.data.reqs.values.toStream)

  def reqItems(p: Project, pt: PlainText.ForProject, legal: Stream[Req]): Stream[ReqItem] = {
    val m = Must.foldMapM[Req, Stream, ReqItem](legal)(req =>
      p.reqType(req.pubid.reqTypeId).map(rt =>
        new ReqItem(req, rt, pt reqTitle req)))
    mustResolve(m)(Stream.empty).sortBy(_.sortKey)
  }

  def req(textSearch: TextSearch, legal: Stream[ReqItem], prefix: Boolean): StrategyA[ReqItem] = {
    val searchTitles =
      textSearch.ignoreCaseNoWhitespace
        .filterByIds(legal.map(_.req.id).toSet)
        .searchOnlyTitles

    val searchFn: TC.Query[ReqItem] = { term =>
      val titles = searchTitles(term).take(10).map(_.id).toSet
      val np     = normaliseReqPubid(term)
      // TODO TextComplete should use multiple result tiers. Titles shouldn't be searched when there are matching pubids
      legal.filter(i => i.pubidStrNorm.contains(np) || titles.contains(i.req.id))
    }

    val (prefixRegex, suffixRegex) = Grammar.reflinkSurround.parsing.regexEscapeAndWrap
    val mainRegex = s"(\\S+?)$suffixRegex?$$"
    def li(i: ReqItem): ReactElement =
      *.reqAutoComplete('req)(r => _('desc)(d =>
        <.div(
          <.div(r, i.pubidStr),
          <.div(d, i.title))
      ))

    optionallyPrefixedStrategy(prefixRegex, mainRegex, searchFn)(
      _.pubidStr, Grammar.reflinkSurround.display.apply, " ")(prefix)
      .template(i => React.renderToStaticMarkup(li(i)))
  }

  final class ReqItem(val req: Req, val reqType: ReqType, val title: String) {
    val pubidStr     = PlainText.pubid(reqType, req.pubid.pos)
    val pubidStrNorm = normaliseReqPubid(pubidStr)
    val sortKey      = (reqType.mnemonic.value, req.pubid.pos.value)
  }

  @inline def normaliseReqPubid(s: String): String =
    Grammar.pubidSeqFormat.normEach(s)

  // ===================================================================================================================
  // <math>

  private def htmllike = Stream("math")

  def math: Strategy =
    Strategy("""(^|\s)<([a-z]+)$""", index = 2)
    .search(term => htmllike.filter(_ startsWith term))
    .replace2(tag => (s"$$1<$tag>", s"</$tag>"))
}
