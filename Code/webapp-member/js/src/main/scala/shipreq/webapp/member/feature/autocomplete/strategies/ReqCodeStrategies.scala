package shipreq.webapp.member.feature.autocomplete.strategies

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.{Utils => Util}
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.View
import scala.scalajs.js.{UndefOr, undefined}
import scalacss.ScalaCssReact._
import shipreq.base.util.MTrie.Ops
import shipreq.webapp.member.project.data.ReqCode.{ActiveGroup, ActiveReq, Node, Trie, Value => Path}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Grammar.{reqCode => G}
import shipreq.webapp.member.project.text.PlainText
import shipreq.webapp.member.ui.BaseStyles.{autoComplete => *}

private[strategies] object ReqCodeStrategies {
  private val sepStr = G.nodeSeparator.toString
  private val sep    = Util.regexEscapeAndWrap(sepStr)
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
    def completeFromStart(trie: Trie): Strategies = {
      val mainRegex = s"($node($sep$node)*$sep?)"

      type A = (Vector[Node], String)

      val searchFn0: Query[A] = { term =>

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
        MutableArray(r).sort.map((path, _)).arraySeq
      }

      val searchFn: String => IterableOnce[A] =
        Query.ignorePerfectMatch(searchFn0)(_ ==* _._2).andThen(_.iterator.take(MaxResults))

      val replace: A => String =
        a => (a._1.map(_.value) :+ a._2).mkString(G.nodeSeparator.toString)

      Context.reflink[A](
        mainRegex, replace, "",
        _.search(searchFn).template((v, _) => v._2)
      )(contextualise)
    }

    // Example: .xyz
    def completeFromMid(trie: Trie): Strategies = {
      val mainRegex = s"$sep($node)"

      val activePaths: Iterable[Path] =
        View.fromIteratorProvider(() =>
          trie.flatIterator().filter(_._2.isActive).map(_._1))

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

      val searchFn: String => IterableOnce[String] = term =>
        MutableArray(
          activePaths.iterator.map(isMatch(term, _))
            .filterDefined
            .toSet)
          .map(PlainText.reqCode)
          .sort
          .iterator()
          .take(MaxResults)

      Context.reflink[String](mainRegex, identity, "", _.search(searchFn))(contextualise)
    }

    completeFromStart(trie) ++ completeFromMid(trie)
  }

  def refCandidates(project: Project, pt: PlainText.ForProject.AnyCtx): RefStrategies.Candidates = {
    import RefStrategies.Candidate

    type A = (String, ActiveGroup \/ ActiveReq)

    val activePaths: ArraySeq[A] =
      MutableArray(
        project.content.reqCodes.trie.flatIterator()
          .collect {
            case (c, a: ActiveReq)   => (PlainText reqCode c, \/-(a))
            case (c, a: ActiveGroup) => (PlainText reqCode c, -\/(a))
          }
      )
        .sortBy(_._1)
        .arraySeq

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

    val toCandidate: A => Candidate = {

      case (code, -\/(a)) =>
        Candidate.standard(
          title       = code,
          desc        = pt.codeGroupTitle(a.group),
          replacement = code,
        )

      case (code, \/-(a)) =>
        val render = () =>
          <.div(
            <.div(
              <.span(*.itemTitle, code),
              <.span(*.itemTitle2, s"(${PlainText.pubidByReqId(a.reqId, project)})"),
              <.div(*.itemDesc, pt.reqTitleById(a.reqId))))
        Candidate(
          title       = code,
          replacement = code,
          render      = render
        )
    }

    term => {
      val p = termToRegex(term).pattern
      activePaths
        .iterator
        .filter(x => p.matcher(x._1).matches)
        .map(toCandidate)
    }
  }

}
