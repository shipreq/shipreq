package shipreq.webapp.base.feature.autocomplete

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.{Utils => Util}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scala.annotation.tailrec
import scala.collection.View
import scala.collection.immutable.ArraySeq
import scalajs.js.{UndefOr, undefined}
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, Grammar, PlainText, Text, TextSearch}
import shipreq.webapp.base.data.{Contextualise, Plain}
import shipreq.webapp.base.jsfacade.TextComplete.Strategy
import shipreq.webapp.base.ui.BaseStyles.{autoComplete => *}
import Implicits.autoLiftTextCompleteStrategy
import Utils.{Context, Strategies}

object ProjectStrategies {
  import Atom.TypeGroup

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Rich text

  def richText(text: Text.Generic)
              (p: Project,
               naTags: NaTags,
               pt: PlainText.ForProject.AnyCtx,
               ts: TextSearch): Strategies = {
    val s = Vector.newBuilder[Strategy[_]]

    s ++= hashtag(
      p,
      HideDead,
      issues = text.supports(TypeGroup.Issue),
      tags   = text.supports(TypeGroup.TagRef),
      naTags = naTags)(
      Contextualise)

    if (text.supports(TypeGroup.ContentRef)) {
      s ++= reqCode.ref(p, pt)
      s ++= req(ts, reqItems(p, pt), Contextualise)
    }

    if (text.supports(Atom.Type.TeX))
      s ++= tex

    s.result()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // #ISSUE #TAG

  private val hashtagContext = Context.literal(Grammar.hashRefKey.prefix, "")

  def hashtag(legal: IterableOnce[HashRefKey]): Contextualise => Strategies = {
    import Grammar.{hashRefKey => G}
    val mainRegex = s"(|${G.firstChar.one}${G.tailChars.*})$$"
    val terms     = ArraySeq unsafeWrapArray MutableArray(legal.iterator.map(_.value)).sort.array
    val searchFn  = Utils.caseInsensitiveContains(terms)
    hashtagContext[String](mainRegex, Identity.apply, "", _.search(searchFn))
  }

  def hashtag(issues: IterableOnce[CustomIssueType],
              tags  : IterableOnce[ApplicableTag],
              fd    : FilterDead): Contextualise => Strategies =
    hashtag(
      fd(issues.iterator)(_.live).map(_.key) ++
      fd(tags  .iterator)(_.live).map(_.key))

  def hashtag(p: Project, fd: FilterDead, issues: Boolean, tags: Boolean, naTags: NaTags): Contextualise => Strategies =
    if (issues || tags) {

      val issueOptions: Iterable[CustomIssueType] =
        if (issues)
          p.config.customIssueTypes.values
        else
          Nil

      val tagOptions: IterableOnce[ApplicableTag] =
        if (tags)
          p.config.tags.applicableTagIterator().filter(_.live is Live).filter(t => !naTags.set.contains(t.id))
        else
          Nil

      hashtag(issueOptions, tagOptions, fd)
    } else
      _ => Vector.empty

  def tag(legal: Iterable[ApplicableTag], fd: FilterDead): Contextualise => Strategies =
    hashtag(Nil, legal, fd)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // [REF]

  private val reflinkContext = Context(Grammar.reflinkSurround)

  def reqItems(p: Project, pt: PlainText.ForProject.AnyCtx): Iterable[ReqItem] =
    reqItems(p, pt, p.content.reqs.all)

  def reqItems(p: Project, pt: PlainText.ForProject.AnyCtx, legal: Iterable[Req]): Iterable[ReqItem] =
    MutableArray(
      legal
        .iterator
        .filter(_.live(p.config.reqTypes) is Live)
        .map(req => ReqItem(req.id, req.pubid, p.config.reqTypes.need(req.pubid.reqTypeId), pt reqTitle req)))
      .sortBy(_.sortKey)
      .arraySeq

  def req(textSearch: TextSearch, legal: Iterable[ReqItem], Contextualise: Contextualise): Strategies = {
    val searchTitles =
      textSearch.ignoreCaseNoWhitespace
        .filterReqsIds(legal.map(_.reqId).toSet)
        .titlesOnly

    def li(i: ReqItem): VdomElement =
      <.div(
        <.div(*.itemTitle, i.pubidStr),
        <.div(*.itemDesc, i.title))

    reflinkContext[ReqItem](
      s"(\\S+?)", _.pubidStr, " ",
      _.search { term =>
        val titles = searchTitles.searchAll(term).take(10).map(_.id).toSet
        val np = normaliseReqPubid(term)
        // TODO TextComplete should use multiple result tiers. Titles shouldn't be searched when there are matching pubids
        legal.filter(i => i.pubidStrNorm.contains(np) || titles.contains(i.reqId))
      }
        .template((i, _) => ReactDOMServer.renderToStaticMarkup(li(i)))
    )(Contextualise)
  }

  final case class ReqItem(reqId: ReqId, pubid: Pubid, reqType: ReqType, title: String) {
    val pubidStr     = PlainText.pubid(reqType, pubid.pos)
    val pubidStrNorm = normaliseReqPubid(pubidStr)
    val sortKey      = (reqType.mnemonic.value, pubid.pos.value)
  }

  implicit def univEqReqItem: UnivEq[ReqItem] =
    UnivEq.derive

  @inline def normaliseReqPubid(s: String): String =
    Grammar.pubid.seqFormat.normEach(s)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // ReqCodes

  object reqCode {
    import Grammar.{reqCode => G}
    import ReqCode.{Node, Trie, Value => Path, ActiveGroup, ActiveReq}

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

        val searchFn0: Utils.Query[A] = { term =>

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

        val searchFn: Utils.Query[A] =
          Utils.ignorePerfectMatch(searchFn0)(_ ==* _._2)

        val replace: A => String =
          a => (a._1.map(_.value) :+ a._2).mkString(G.nodeSeparator.toString)

        reflinkContext[A](
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

        val searchFn: Utils.Query[String] = term =>
          MutableArray(
            activePaths.iterator.map(isMatch(term, _))
              .filterDefined
              .toSet)
            .map(PlainText.reqCode)
            .sort
            .arraySeq

        reflinkContext[String](mainRegex, identity, "", _.search(searchFn))(contextualise)
      }

      completeFromStart(trie) ++ completeFromMid(trie)
    }

    /**
     * ReqCode references.
     *
     * Matches whole paths, wraps in reflink syntax.
     */
    def ref(project: Project, pt: PlainText.ForProject.AnyCtx): Strategies = {
      val mainRegex = s"($sep?$node($sep$node)*$sep?)"

      type A = (String, ActiveGroup \/ ActiveReq)

      val activePaths: View[A] =
        MutableArray(
          project.content.reqCodes.trie.flatIterator()
            .collect {
              case (c, a: ActiveReq)   => (PlainText reqCode c, \/-(a))
              case (c, a: ActiveGroup) => (PlainText reqCode c, -\/(a))
            }
        )
          .sortBy(_._1)
          .view

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

      val searchFn: Utils.Query[A] = term => {
        val p = termToRegex(term).pattern
        activePaths.filter(x => p.matcher(x._1).matches)
      }

      def li(a: A): VdomElement = {
        val code = a._1
        a._2 match {
          case \/-(a) =>
            <.div(
              <.div(
                <.span(*.itemTitle, code),
                <.span(*.itemTitle2, s"(${PlainText.pubidByReqId(a.reqId, project)})"),
              <.div(*.itemDesc, pt.reqTitleById(a.reqId))))
          case -\/(a) =>
            <.div(
              <.div(*.itemTitle, code),
              <.div(*.itemDesc, pt.codeGroupTitle(a.group)))
        }
      }

      reflinkContext[A](
        mainRegex, _._1, " ",
        _.search(searchFn).template((i, _) => ReactDOMServer.renderToStaticMarkup(li(i)))
      )(Contextualise)
    }

  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // <tex>

  lazy val tex: Strategies = {
    val tags = List(Grammar.texTag)
    Strategy.builder
      .regex("""(^|\s)<([a-z]+)$""", index = 2)
      .search(term => tags.filter(_ startsWith term))
      .replace2(tag => (s"$$1<$tag>", s"</$tag>"))
      .result()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Req type mnemonics

  def reqTypeMnemonics(reqTypes: ReqTypes, exclude: Set[String]): Strategies = {
    import Grammar.{reqTypeMnemonic => G}
    Strategy.builder
      .regex(s"(^|\\s|,)(|${G.caseInsensitiveRegexStr})$$", index = 2)
      .search(term =>
        reqTypes.liveSortedByMnemonic
          .iterator
          .filterNot(rt => exclude.contains(rt.mnemonic.value))
          .filter(_.mnemonic.value startsWith term)
          .map(r => s"${r.mnemonic.value}: ${r.name}")
      )
      .replace(rt => s"$$1${rt.takeWhile(_ != ':')}")
      .result()
  }
}
