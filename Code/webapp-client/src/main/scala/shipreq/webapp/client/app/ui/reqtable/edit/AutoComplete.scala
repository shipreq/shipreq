package shipreq.webapp.client.app.ui.reqtable
package edit

import scala.annotation.tailrec
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import scalajs.js.{UndefOr, undefined}
import scalaz.std.string.stringInstance
import scalaz.syntax.equal._
import shapeless.syntax.singleton._
import shipreq.base.util.MTrie
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.{NonEmptyVector, UnivEq, Must, Util}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText, Grammar}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.ui.UI
import TC.{Query, Strategy, StrategyA, Strategies}

object AutoComplete {

  sealed trait WithSyntax
  case object WithSyntax extends WithSyntax
  case object WithoutSyntax extends WithSyntax

  class Syntax(val prefixRegex: String,
               val suffixRegex: String,
               val applySyntax: String => String) {
    // Util.regexEscapeAndWrap turns empty strings into (?:) which is fine
    // val acSuffix = if (suffixRegex.isEmpty) "$" else suffixRegex + "?$"
    
    def strategy[A](mainRegex     : String,
                    searchFn      : Query[A])
                   (replacementA  : A => String,
                    replacementEnd: String): WithSyntax => Strategy.B3[A] = {
      case WithSyntax =>
        Strategy(s"$prefixRegex$mainRegex$suffixRegex?$$", index = 1)
          .search(searchFn)
          .replace(s => applySyntax(replacementA(s)) + replacementEnd)

      case WithoutSyntax =>
        Strategy(s"(^|\\s)$prefixRegex?$mainRegex$suffixRegex?$$", index = 2)
          .search(searchFn)
          .replace(s => "$1" + replacementA(s) + replacementEnd)
    }
  }
  
  object Syntax {
    def apply(s: Grammar.Surrounds): Syntax = {
      val (a, b) = s.parsing.regexEscapeAndWrap
      new Syntax(a, b, s.display.apply)
    }

    def literal(pre: String, suf: String): Syntax =
      new Syntax(Util regexEscapeAndWrap pre, Util regexEscapeAndWrap suf, pre + _ + suf)
  }

  // ===================================================================================================================
  // #ISSUE #TAG

  val hashtagSyntax = Syntax.literal(Grammar.hashRefKey.prefix, "")

  def hashtag(legal: Stream[HashRefKey]): WithSyntax => Strategy = {
    import Grammar.{hashRefKey => G}
    val mainRegex = s"(${G.firstChar.one}${G.allChars.*})"
    val searchFn  = TC.caseInsensitiveContains(legal.map(_.value).sorted)
    hashtagSyntax.strategy(mainRegex, searchFn)(identity, " ")(_)
  }

  def hashtag(legalIssues: Stream[CustomIssueType], legalTags: Stream[ApplicableTag]): WithSyntax => Strategy =
    hashtag(legalIssues.map(_.key) append legalTags.map(_.key))

  def tag(legal: Stream[ApplicableTag]): WithSyntax => Strategy =
    hashtag(legal.map(_.key))

  def issue(legal: Stream[CustomIssueType]): WithSyntax => Strategy =
    hashtag(legal.map(_.key))

  // ===================================================================================================================
  // [REF]
  
  val reflinkSyntax = Syntax(Grammar.reflinkSurround)

  def reqItems(p: Project, pt: PlainText.ForProject): Stream[ReqItem] =
    reqItems(p, pt, p.reqs.data.reqs.values.toStream)

  def reqItems(p: Project, pt: PlainText.ForProject, legal: Stream[Req]): Stream[ReqItem] = {
    val m = Must.foldMapM[Req, Stream, ReqItem](legal)(req =>
      p.reqType(req.pubid.reqTypeId).map(rt =>
        new ReqItem(req, rt, pt reqTitle req)))
    mustResolve(m)(Stream.empty).sortBy(_.sortKey)
  }

  def req(textSearch: TextSearch, legal: Stream[ReqItem], withSyntax: WithSyntax): StrategyA[ReqItem] = {
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

    def li(i: ReqItem): ReactElement =
      *.reqAutoComplete('req)(r => _('desc)(d =>
        <.div(
          <.div(r, i.pubidStr),
          <.div(d, i.title))
      ))

    reflinkSyntax.strategy(s"(\\S+?)", searchFn)(_.pubidStr, " ")(withSyntax)
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
  // ReqCodes

  object reqCode {
    import Grammar.{reqCode => G}
    import ReqCode.{Node, Trie, ActiveData, Value => Path}

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
      @inline def withSyntax = WithoutSyntax
  
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
          var r = t.toStream.filter(_._2.existsV(_.active.isDefined)).map(_._1.value)
          for (l <- lead)
            r = r.filter(_ startsWith l)
          r.sorted.map((path, _))
        }
  
        val searchFn = TC.ignorePerfectMatch(searchFn0)(_ ≟ _._2)
  
        def replace(r: A) =
          (r._1.map(_.value) :+ r._2).mkString(G.nodeSeparator.toString)
  
        reflinkSyntax.strategy(mainRegex, searchFn)(replace, "")(withSyntax)
          .template(_._2)
      }
  
      // Example: .xyz
      def completeFromMid(trie: Trie): Strategy = {
        val mainRegex = s"$sep($node)"

        val activePaths: Stream[Path] =
          trie.flatStream.filter(_._2.active.isDefined).map(_._1)

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
  
        reflinkSyntax.strategy(mainRegex, searchFn)(identity, "")(withSyntax)
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

      type A = (String, ActiveData)

      val activePaths: Stream[A] =
        project.reqCodes.data.trie.flatStream
          .filter(_._2.active.isDefined)
          .map(x => (PlainText reqCode x._1, x._2.active.get))
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
        val (code, data) = a
        data.target match {
          case id: ReqId =>
            *.codeRefToReqAutoComplete('code)(c => _('pubid)(p => _('desc)(d =>
              <.div(
                <.div(
                  <.span(c, code),
                  <.span(p, s"(${UI mustA PlainText.pubid(project, id)})"),
                <.div(d, UI mustA pt.reqTitleById(id))))
            )))
          case g: ReqCodeGroup =>
            *.codeRefToGroupAutoComplete('req)(r => _('desc)(d =>
              <.div(
                <.div(r, code),
                <.div(d, pt.reqCodeGroupTitle(data.id, g)))
            ))
        }
      }

      reflinkSyntax.strategy(mainRegex, searchFn)(_._1, " ")(WithSyntax)
        .template(i => React.renderToStaticMarkup(li(i)))
    }

  }

  // ===================================================================================================================
  // <math>

  private def htmllike = Stream("math")

  def math: Strategy =
    Strategy("""(^|\s)<([a-z]+)$""", index = 2)
    .search(term => htmllike.filter(_ startsWith term))
    .replace2(tag => (s"$$1<$tag>", s"</$tag>"))
}
