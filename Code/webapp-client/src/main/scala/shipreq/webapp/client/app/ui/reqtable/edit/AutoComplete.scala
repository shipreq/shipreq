package shipreq.webapp.client.app.ui.reqtable
package edit

import scala.annotation.tailrec
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import scalajs.js.{UndefOr, undefined}
import scalaz.{\/-, -\/, \/}
import scalaz.std.string.stringInstance
import scalaz.syntax.equal._
import shapeless.syntax.singleton._
import shipreq.base.util._
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText, Grammar}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.{FilterDead, Plain, Contextualise}
import TC.{Query, Strategy, StrategyA, Strategies}

object AutoComplete {

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
    def apply(s: Grammar.Surrounds): Context = {
      val (a, b) = s.parsing.regexEscapeAndWrap
      new Context(a, b, s.display.apply)
    }

    def literal(pre: String, suf: String): Context =
      new Context(Util regexEscapeAndWrap pre, Util regexEscapeAndWrap suf, pre + _ + suf)
  }

  // ===================================================================================================================
  // #ISSUE #TAG

  private val hashtagContext = Context.literal(Grammar.hashRefKey.prefix, "")

  def hashtag(legal: Stream[HashRefKey]): Contextualise => Strategy = {
    import Grammar.{hashRefKey => G}
    val mainRegex = s"(|${G.firstChar.one}${G.allChars.*})$$"
    val searchFn  = TC.caseInsensitiveContains(legal.map(_.value).sorted)
    hashtagContext.strategy(mainRegex, searchFn)(identity, "")(_)
  }

  def hashtag(issues: Stream[CustomIssueType],
              tags  : Stream[ApplicableTag],
              fd    : FilterDead): Contextualise => Strategy =
    hashtag(
      fd(issues)(_.live).map(_.key) append
      fd(tags  )(_.live).map(_.key))

  def hashtag(p: Project, fd: FilterDead, issues: Boolean, tags: Boolean): Contextualise => Strategy =
    hashtag(
      if (issues) p.config.customIssueTypes.values.toStream else Stream.empty,
      if (tags)   p.config.atags                            else Stream.empty,
      fd)

  def issue(legal: Stream[CustomIssueType], fd: FilterDead): Contextualise => Strategy =
    hashtag(legal, Stream.empty, fd)

  def tag(legal: Stream[ApplicableTag], fd: FilterDead): Contextualise => Strategy =
    hashtag(Stream.empty, legal, fd)

  // ===================================================================================================================
  // [REF]

  private val reflinkContext = Context(Grammar.reflinkSurround)

  def reqItems(p: Project, pt: PlainText.ForProject): Stream[ReqItem] =
    reqItems(p, pt, p.reqs.reqs.values.toStream)

  def reqItems(p: Project, pt: PlainText.ForProject, legal: Stream[Req]): Stream[ReqItem] = {
    legal.filter(_.live(p.config.customReqTypes) :: Live)
      .map(req => new ReqItem(req, p.config.reqType(req.pubid.reqTypeId), pt reqTitle req))
      .sortBy(_.sortKey)
  }

  def req(textSearch: TextSearch, legal: Stream[ReqItem], Contextualise: Contextualise): StrategyA[ReqItem] = {
    val searchTitles =
      textSearch.ignoreCaseNoWhitespace
        .filterReqsIds(legal.map(_.req.id).toSet)
        .titlesOnly

    val searchFn: TC.Query[ReqItem] = { term =>
      val titles = searchTitles.searchAll(term).take(10).map(_.id).toSet
      val np     = normaliseReqPubid(term)
      // TODO TextComplete should use multiple result tiers. Titles shouldn't be searched when there are matching pubids
      legal.filter(i => i.pubidStrNorm.contains(np) || titles.contains(i.req.id))
    }

    def li(i: ReqItem): ReactElement =
      *.reqAutoComplete('req)(r => _('desc)(d =>
        <.div(
          <.div(r, i.pubidStr),
          <.div(d, i.title))
      ))

    reflinkContext.strategy(s"(\\S+?)", searchFn)(_.pubidStr, " ")(Contextualise)
      .template((i, _) => React.renderToStaticMarkup(li(i)))
  }

  final class ReqItem(val req: Req, val reqType: ReqType, val title: String) {
    val pubidStr     = PlainText.pubid(reqType, req.pubid.pos)
    val pubidStrNorm = normaliseReqPubid(pubidStr)
    val sortKey      = (reqType.mnemonic.value, req.pubid.pos.value)
  }

  @inline def normaliseReqPubid(s: String): String =
    Grammar.pubidSeqFormat.normEach(s)

  // ===================================================================================================================
  // ReqCodes

  object reqCode {
    import Grammar.{reqCode => G}
    import ReqCode.{Node, Trie, Value => Path, ActiveGroup, ActiveReq}

    private val sepStr = G.nodeSeparator.toString
    private val sep    = Util regexEscapeAndWrap sepStr
    private val sepR   = sep.r
    private val node   = s"(?:${G.firstChar.one}${G.allChars.*})"

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

      Strategies(
        completeFromStart(trie),
        completeFromMid(trie))
    }

    /**
     * ReqCode references.
     *
     * Matches whole paths, wraps in reflink syntax.
     */
    def ref(project: Project, pt: PlainText.ForProject): Strategy = {
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
            *.codeRefToReqAutoComplete('code)(c => _('pubid)(p => _('desc)(d =>
              <.div(
                <.div(
                  <.span(c, code),
                  <.span(p, s"(${PlainText.pubid(project, a.reqId)})"),
                <.div(d, pt.reqTitleById(a.reqId))))
            )))
          case -\/(a) =>
            *.codeRefToGroupAutoComplete('req)(r => _('desc)(d =>
              <.div(
                <.div(r, code),
                <.div(d, pt.reqCodeGroupTitle(a.group)))
            ))
        }
      }

      reflinkContext.strategy(mainRegex, searchFn)(_._1, " ")(Contextualise)
        .template((i, _) => React.renderToStaticMarkup(li(i)))
    }

  }

  // ===================================================================================================================
  // <math>

  private def htmllike = Stream("math")

  def math: Strategy =
    Strategy.pattern("""(^|\s)<([a-z]+)$""", index = 2)
    .search(term => htmllike.filter(_ startsWith term))
    .replace2(tag => (s"$$1<$tag>", s"</$tag>"))
}
