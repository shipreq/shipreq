package shipreq.webapp.client.lib

import scala.annotation.tailrec
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.univeq._
import scalajs.js.{UndefOr, undefined}
import scalaz.{\/-, -\/, \/}
import shapeless.syntax.singleton._
import shipreq.base.util._
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, GrammarSpec, PlainText, Text, TextSearch}
import shipreq.webapp.client.app.Style.{reqtable => *}
import shipreq.webapp.client.jsfacade.{TextComplete => TC}
import shipreq.webapp.client.data.{Contextualise, Plain}
import shipreq.webapp.client.feature.AutoCompleteFeature
import AutoCompleteFeature.{Strategies, autoLiftSingleStrategy}
import TC.{Query, Strategy}

object AutoComplete {

  def forRichText(text: Text.Generic)
                 (p: Project, pt: PlainText.ForProject, ts: TextSearch): AutoCompleteFeature.ForChild = {
    var ac = Vector.empty[Strategy]

    ac ++= AutoComplete.hashtag(p, HideDead, issues = text.supportsIssues, tags = text.supportsTags)(Contextualise)

    if (text.supportsReqRefs) {
      ac ++= AutoComplete.reqCode.ref(p, pt)
      ac ++= AutoComplete.req(ts, AutoComplete.reqItems(p, pt), Contextualise)
    }

    if (text.supportsPTM)
      ac ++= AutoComplete.math

    ac
  }

  // ===================================================================================================================

  private class Context(val prefixRegex: String,
                        val suffixRegex: String,
                        val applyContext: String => String) {
    // Util.regexEscapeAndWrap turns empty strings into (?:) which is fine
    // val acSuffix = if (suffixRegex.isEmpty) "$" else suffixRegex + "?$"

    def strategy[A](mainRegex     : String,
                    searchFn      : Query[A])
                   (replacementA  : A => String,
                    replacementEnd: String): Contextualise => Strategy.B3[A] = {

      case Contextualise =>
        Strategy.pattern(s"$prefixRegex$mainRegex$suffixRegex?$$", index = 1)
          .search(searchFn)
          .replace(s => applyContext(replacementA(s)) + replacementEnd)

      case Plain =>
        Strategy.pattern(s"(^|\\s)$prefixRegex?$mainRegex$suffixRegex?$$", index = 2)
          .search(searchFn)
          .replace(s => "$1" + replacementA(s) + replacementEnd)
    }
  }

  private object Context {
    def apply(s: GrammarSpec.Surrounds): Context = {
      val (a, b) = s.parsing.regexEscapeAndWrap
      new Context(a, b, s.display.apply)
    }

    def literal(pre: String, suf: String): Context =
      new Context(Util regexEscapeAndWrap pre, Util regexEscapeAndWrap suf, pre + _ + suf)
  }

  // ===================================================================================================================
  // #ISSUE #TAG

  private val hashtagContext = Context.literal(Grammar.hashRefKey.prefix, "")

  def hashtag(legal: Stream[HashRefKey]): Contextualise => Strategies = {
    import Grammar.{hashRefKey => G}
    val mainRegex = s"(|${G.firstChar.one}${G.tailChars.*})$$"
    val searchFn  = TC.caseInsensitiveContains(legal.map(_.value).sorted)
    hashtagContext.strategy(mainRegex, searchFn)(identity, "")(_)
  }

  def hashtag(issues: Stream[CustomIssueType],
              tags  : Stream[ApplicableTag],
              fd    : FilterDead): Contextualise => Strategies =
    hashtag(
      fd(issues)(_.live).map(_.key) append
      fd(tags  )(_.live).map(_.key))

  def hashtag(p: Project, fd: FilterDead, issues: Boolean, tags: Boolean): Contextualise => Strategies =
    if (issues || tags)
      hashtag(
        if (issues) p.config.customIssueTypes.values.toStream else Stream.empty,
        if (tags)   p.config.atagIterator.toStream            else Stream.empty,
        fd)
    else
      _ => Vector.empty

  def issue(legal: Stream[CustomIssueType], fd: FilterDead): Contextualise => Strategies =
    hashtag(legal, Stream.empty, fd)

  def tag(legal: Stream[ApplicableTag], fd: FilterDead): Contextualise => Strategies =
    hashtag(Stream.empty, legal, fd)

  // ===================================================================================================================
  // [REF]

  private val reflinkContext = Context(Grammar.reflinkSurround)

  def reqItems(p: Project, pt: PlainText.ForProject): Stream[ReqItem] =
    reqItems(p, pt, p.reqs.reqs.values.toStream)

  def reqItems(p: Project, pt: PlainText.ForProject, legal: Stream[Req]): Stream[ReqItem] = {
    legal.filter(_.live(p.config.reqTypes) :: Live)
      .map(req => new ReqItem(req.id, req.pubid, p.config.reqTypes.need(req.pubid.reqTypeId), pt reqTitle req))
      .sortBy(_.sortKey)
  }

  def req(textSearch: TextSearch, legal: Stream[ReqItem], Contextualise: Contextualise): Strategies = {
    val searchTitles =
      textSearch.ignoreCaseNoWhitespace
        .filterReqsIds(legal.map(_.reqId).toSet)
        .titlesOnly

    val searchFn: TC.Query[ReqItem] = { term =>
      val titles = searchTitles.searchAll(term).take(10).map(_.id).toSet
      val np     = normaliseReqPubid(term)
      // TODO TextComplete should use multiple result tiers. Titles shouldn't be searched when there are matching pubids
      legal.filter(i => i.pubidStrNorm.contains(np) || titles.contains(i.reqId))
    }

    def li(i: ReqItem): ReactElement =
      <.div(
        <.div(*.autoCompleteItemTitle, i.pubidStr),
        <.div(*.autoCompleteItemDesc, i.title))

    reflinkContext.strategy(s"(\\S+?)", searchFn)(_.pubidStr, " ")(Contextualise)
      .template((i, _) => ReactDOMServer.renderToStaticMarkup(li(i)))
  }

  case class ReqItem(reqId: ReqId, pubid: Pubid, reqType: ReqType, title: String) {
    val pubidStr     = PlainText.pubid(reqType, pubid.pos)
    val pubidStrNorm = normaliseReqPubid(pubidStr)
    val sortKey      = (reqType.mnemonic.value, pubid.pos.value)
  }

  implicit def univEqReqItem: UnivEq[ReqItem] =
    UnivEq.derive

  @inline def normaliseReqPubid(s: String): String =
    Grammar.pubid.seqFormat.normEach(s)

  // ===================================================================================================================
  // ReqCodes

  object reqCode {
    import Grammar.{reqCode => G}
    import ReqCode.{Node, Trie, Value => Path, ActiveGroup, ActiveReq}

    private val sepStr = G.nodeSeparator.toString
    private val sep    = Util regexEscapeAndWrap sepStr
    private val sepR   = sep.r
    private val node   = s"(?:${G.firstChar.one}${G.tailChars.*})"

    /**
     * Matches ReqCode prefixes. eg. "1.2" of "1.2.3.4".
     *
     * Useful for ReqCode editing.
     */
    def prefixes(trie: Trie): Strategies = {
      @inline def contextualise = Plain

      // Example: abc & abc.def
      def completeFromStart(trie: Trie): Strategy = {
        val mainRegex = s"($node($sep$node)*$sep?)"

        type A = (Vector[Node], String)

        val searchFn0: TC.Query[A] = { term =>

          // Parse input
          var nodes = term.split(G.nodeSeparator).toVector
          var lead: Option[String] = None
          if (term.last != G.nodeSeparator) {
            lead = nodes.lastOption
            nodes = nodes.init
          }
          val path = nodes.map(Node.applyFn)

          // Find suggestions
          val t = NonEmptyVector.maybe(path, trie)(trie.dropPath)
          var r = t.iterator.filter(_._2.existsV(_.isActive)).map(_._1.value)
          for (l <- lead)
            r = r.filter(_ startsWith l)
          r.toStream.sorted.map((path, _))
        }

        val searchFn = TC.ignorePerfectMatch(searchFn0)(_ ==* _._2)

        def replace(r: A) =
          (r._1.map(_.value) :+ r._2).mkString(G.nodeSeparator.toString)

        reflinkContext.strategy(mainRegex, searchFn)(replace, "")(contextualise)
          .template((v, _) => v._2)
      }

      // Example: .xyz
      def completeFromMid(trie: Trie): Strategy = {
        val mainRegex = s"$sep($node)"

        val activePaths: Stream[Path] =
          trie.flatStream.filter(_._2.isActive).map(_._1)

        def isMatch(term: String, path: Path): UndefOr[Path] = {
          @tailrec def go(prefix: Path, suffix: Vector[Node]): UndefOr[Path] =
            if (suffix.isEmpty)
              undefined
            else if (suffix.head.value startsWith term)
              prefix :+ suffix.head
            else
              go(prefix :+ suffix.head, suffix.tail)
          go(NonEmptyVector one path.head, path.tail)
        }

        val searchFn: TC.Query[String] = term =>
          activePaths.map(isMatch(term, _))
            .jsDefined
            .distinctSafe
            .map(PlainText.reqCode)
            .sorted

        reflinkContext.strategy(mainRegex, searchFn)(identity, "")(contextualise)
      }

      Vector(
        completeFromStart(trie),
        completeFromMid(trie))
    }

    /**
     * ReqCode references.
     *
     * Matches whole paths, wraps in reflink syntax.
     */
    def ref(project: Project, pt: PlainText.ForProject): Strategies = {
      val mainRegex = s"($sep?$node($sep$node)*$sep?)"

      type A = (String, ActiveGroup \/ ActiveReq)

      val activePaths: Stream[A] =
        project.reqCodes.trie.flatStream
          .collect {
            case (c, a: ActiveReq)   => (PlainText reqCode c, \/-(a))
            case (c, a: ActiveGroup) => (PlainText reqCode c, -\/(a))
          }
          .sortBy(_._1)

      def termToRegex(term0: String) = {
        import shipreq.base.util.SafeStringOps._
        var term = term0
        var regex = "^"
        def add(s: String) = regex = regex ~ s
        if (term startsWith sepStr) {
          term = term drop sepStr.length
          add(".+" ~ sep)
        }
        var first = true
        for (n <- sepR.split(term)) {
          if (first) first = false else add(sep)
          add(".*" ~ Util.regexEscape(n) ~".*")
        }
        if (term endsWith sepStr)
          add(sep ~ ".+")
        add("$")
        regex.r
      }

      val searchFn: TC.Query[A] = term => {
        val p = termToRegex(term).pattern
        activePaths.filter(x => p.matcher(x._1).matches)
      }

      def li(a: A): ReactElement = {
        val code = a._1
        a._2 match {
          case \/-(a) =>
            <.div(
              <.div(
                <.span(*.autoCompleteItemTitle, code),
                <.span(*.autoCompleteItemTitle2, s"(${PlainText.pubidByReqId(a.reqId, project)})"),
              <.div(*.autoCompleteItemDesc, pt.reqTitleById(a.reqId))))
          case -\/(a) =>
            <.div(
              <.div(*.autoCompleteItemTitle, code),
              <.div(*.autoCompleteItemDesc, pt.reqCodeGroupTitle(a.group)))
        }
      }

      reflinkContext.strategy(mainRegex, searchFn)(_._1, " ")(Contextualise)
        .template((i, _) => ReactDOMServer.renderToStaticMarkup(li(i)))
    }

  }

  // ===================================================================================================================
  // <math>

  private def htmllike = Stream("math")

  def math: Strategies =
    Strategy.pattern("""(^|\s)<([a-z]+)$""", index = 2)
      .search(term => htmllike.filter(_ startsWith term))
      .replace2(tag => (s"$$1<$tag>", s"</$tag>"))
}
