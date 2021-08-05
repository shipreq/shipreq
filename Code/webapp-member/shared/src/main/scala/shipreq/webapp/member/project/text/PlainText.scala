package shipreq.webapp.member.project.text

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.{ConciseIntSetFormat, Memo}
import scala.collection.immutable.SortedSet
import shipreq.base.util.SafeStringOps._
import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Atom.{AnyAtom, DisplayReqRef}
import shipreq.webapp.member.project.text.GrammarSpec.Surrounds
import shipreq.webapp.member.project.text.ProjectText.SetRenderStyle
import shipreq.webapp.member.project.text.{Grammar => G}
import shipreq.webapp.member.project.util.ReqCodeTreeItem

/**
 * Turns elements of data into user-facing plain text.
 */
object PlainText {

  type NoCtx  = ProjectText[ProjectText.Context.None, String]

  object ForProject {
    type AnyCtx = ForProject[ProjectText.Context]
    type NoCtx  = ForProject[ProjectText.Context.None]

    def apply[Ctx <: ProjectText.Context](p: Project, ctx: Ctx): ForProject[Ctx] =
      new ForProject(p, ctx)

    object noCtx {

      def apply(p: Project): NoCtx =
        ForProject(p, ProjectText.Context.None)

      lazy val empty: NoCtx =
        apply(Project.empty)
    }
  }

  val reqCodeIndentation: NonEmptyVector[ReqCodeTreeItem.Indent] => String =
    Memo { is =>
      import ReqCodeTreeItem._
      val I = "│"
      is.reduceMapLeft1({
        case IndentChild    => I
        case IndentSpace(l) => I ~ (" " * (l - 1))
      })(_ ~ ' ' ~ _)
    }

  def reqCodeTreeItem(ti: ReqCodeTreeItem): String =
    NonEmptyVector.option(ti.indent) match {
      case None     => reqCode(ti.suffix)
      case Some(is) => reqCodeIndentation(is) ~ G.reqCode.nodeSeparator ~ reqCode(ti.suffix)
    }

  def reqCode(c: ReqCode.Value): String =
    ReqCode.Value.toStr(c, G.reqCode.nodeSeparator)

  def reqCodeById(id: ReqCodeGroupId, p: Project): String =
    reqCode(p.content.reqCodes.reqCode(id))

  def hashtag(key: HashRefKey): String =
    G.hashRefKey.prefix ~ key.value

  def pubidByReqId(id: ReqId, p: Project): String =
    pubidByReqId(id, p.content.reqs, p.config.reqTypes)

  def pubidByReqId(id: ReqId, reqs: Requirements, reqTypes: ReqTypes): String = {
    val pid = reqs.need(id).pubid
    pubid(pid, reqTypes)
  }

  def pubid(pid: Pubid, p: Project): String =
    pubid(pid, p.config.reqTypes)

  def pubid(pid: Pubid, reqTypes: ReqTypes): String = {
    val rt = pid.reqTypeId.foldId(identity, reqTypes.need)
    pubid(rt, pid.pos)
  }

  def pubid(reqType: ReqType, pos: ReqTypePos): String =
    pubid(reqType.mnemonic, pos)

  def pubid(externalPubid: ExternalPubid): String =
    pubid(externalPubid.mnemonic, externalPubid.pos)

  def pubid(mnemonic: ReqType.Mnemonic, pos: ReqTypePos): String =
    mnemonic.value ~ "-" ~ pos.value

  def concisePubidSet(reqIds: NonEmptySet[ReqId], p: Project, sep: String = ", "): String = {
    val reqs = p.content.reqs
    var byType = Multimap.empty[ReqTypeId, Set, Int]
    for (reqId <- reqIds) {
      val pubid = reqs.need(reqId).pubid
      byType = byType.add(pubid.reqTypeId, pubid.pos.value)
    }
    val reqTypes = MutableArray(byType.keyIterator).sort(p.config.reqTypes.reqTypeIdOrdering)
    reqTypes.iterator().map { rt =>
      val prefix = p.config.reqTypes.need(rt).mnemonic.value + "-"
      val ps = byType(rt)
      if (ps.sizeIs == 1)
        prefix + ps.head.toString
      else
        s"$prefix{${ConciseIntSetFormat(ps)}}"
    }.mkString(sep)
  }

  def reqTypeShort(rt: ReqType): String =
    rt.mnemonic.value

  def reqTypeFull(rt: ReqType): String =
    s"${rt.mnemonic.value}: ${rt.name}"

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private implicit def surroundDisplay(s: Surrounds) = s.display

  private implicit class OptionalTextOps[T <: Text.Generic](val _t: T#OptionalText) extends AnyVal {
    @inline def asOption: Option[_t.type] =
      if (_t.isEmpty) None else Some[_t.type](_t)

    @inline def net: Option[T#NonEmptyText] =
      NonEmptyArraySeq.option(_t)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class ForProject[+Ctx <: ProjectText.Context](p: Project, ctx: Ctx) extends ProjectText[Ctx, String](p, ctx) {

    override protected def _implicationList(ids: Vector[Pubid], style: SetRenderStyle): String =
      style match {

        case SetRenderStyle.SingleLineBrief =>
          ids.iterator.map(pubid(_, p)).mkString(", ")

        case SetRenderStyle.MultiLineDetailed =>
          ids.iterator.map { pid =>
            val reqId = p.content.reqs.reqIdByPubid(pid)
            "* " + pubid(pid, p) + ": " + reqTitleById(reqId)
          }.mkString("\n")
      }

    override protected def _text(text: Text.AnyOptional, live: Live, tagValidity: ApplicableTagId => Validity): String =
      nestedText(live, text, includeMarkup = true)

    // Keep in sync with ProjectWidgets because it's used together for sorting/rendering in ReqTable
    override protected def deletionReasonWhenNoneGiven: String =
      ""

    // Keep in sync with ProjectWidgets because it's used together for sorting/rendering in ReqTable
    override protected def deletionReasonWhenReqTypeIsDead(rt: ReqType): String =
      SpecialBuiltInField.ReqType.name + " " + rt.mnemonic.value + " is deleted."

    override protected def emptyText = ""

    override protected val useCaseFlowElement: UseCaseStep.Focus => String =
      useCaseStepLabel

    override def whenBlankButMandatory = ""

    private def issue(id: CustomIssueTypeId, desc: Option[String]): String = {
      val it = p.config.customIssueTypes.need(id)
      desc.foldLeft(hashtag(it.key))(_ ~ G.issueDescSurround(_))
    }

    private val ulLead: () => String =
      () => "* "

    private def olLead(): () => String = {
      var prev = 0
      () => {
        prev += 1
        prev.toString ~ ". "
      }
    }

    private def spaces(len: Int): String =
      len match {
        case 2 => "  "
        case 3 => "   "
        case 4 => "    "
        case 5 => "     "
        case 6 => "      "
        case n => " " * n
      }

    // There are some non-trivial rules in place here:
    //
    // 1. if an LI contains blanklines, or contains a list followed by anything else
    //  * use spaces between immediate child lists
    //  * all sibling LIs must be separated by spaces
    //  * parent LIs should also be separated by spaces
    //
    private def nestedText(live         : Live,
                           atoms        : ArraySeq[AnyAtom],
                           includeMarkup: Boolean,
                          ): String = {

      def nextLevel(acc              : String,
                    indent           : String,
                    atoms            : ArraySeq[AnyAtom],
                    parentIsSpacyList: Boolean = false
                   ): String = {

        lazy val listsAtThisLevel: Int =
          atoms.count {
            case _: Atom.ListMarkup # ListBase => true
            case _                             => false
          }

        @tailrec def go(acc                        : String,
                        idx                        : Int,
                        seenMultilineAtThisLevelYet: Boolean,
                       ): String = {
          import Atom._

          val nextIdx = idx + 1
          val nextIsEmpty = nextIdx == atoms.length

          def heading(markup: String, title: Headings#HeadingTitle) = {
            val line = {
              val t = nextLevel("", "", title.whole)
              if (!includeMarkup)
                t
              else if (t.isEmpty)
                markup
              else
                markup ~ ' ' ~ t
            }
            val prefix = if (acc.isEmpty || acc.endsWith("\n\n")) "" else "\n\n"
            val suffix = if (nextIsEmpty) "" else "\n\n"
            prefix ~ line ~ suffix
          }

          def style(markup: String, innerText: NonEmptyArraySeq[AnyAtom]) = {
            val inner = nextLevel("", "", innerText.whole)
            if (includeMarkup)
              markup ~ inner ~ markup
            else
              inner
          }

          def list(l: ListMarkup # ListBase, nextBullet: () => String) = {
            val lis                   = l.items.whole
            val liSpace               = l.itemNeedsSpace.whole
            val spaceForSubsequentLIs = if (l.needsSpace) "\n\n" else "\n"

            var result = ""
            var i = 0
            while (i < lis.length) {
              val li = lis(i)

              // Add the new line
              if (i == 0) {

                // First LI

                @inline def alreadyInPlace =
                  acc.isEmpty || acc.endsWith("\n\n" ~ indent)

                @inline def atRoot =
                  indent.isEmpty

                @inline def separateFromParentListItem =
                  parentIsSpacyList && (seenMultilineAtThisLevelYet || listsAtThisLevel != 1)

                if (!alreadyInPlace) {
                  if (atRoot || separateFromParentListItem) {
                    result += "\n"
                  }
                  result += "\n"
                  result += indent
                }

              } else {
                // Subsequent LIs
                result += spaceForSubsequentLIs
                result += indent
              }

              // Add the bullet
              val bullet = nextBullet()
              result += bullet

              // Add the LI content
              val nextIndent = indent + spaces(bullet.length)
              if (li.nonEmpty) {
                result = nextLevel(result, nextIndent, li, liSpace(i))
              }

              i += 1
            }

            if (nextIsEmpty)
              result
            else
              result ~ "\n\n" ~ indent
          }

          val atom = atoms(idx)

          val cur = atom match {
            case a: Literal         # Literal        => a.value
            case _: NewLine         # BlankLine      => "\n\n" ~ indent
            case a: ContentRef      # ReqRef         => reqRef(a.id, a.display, includeMarkup = includeMarkup)
            case a: ContentRef      # CodeRef        => codeRef(a.id, a.display, includeMarkup = includeMarkup)
            case a: ContentRef      # UseCaseStepRef => useCaseStepRef(a.value)
            case a: Issue           # Issue          => issue(a.typ, a.desc.asOption.map(text(_, live, Optional)))
            case a: PlainTextMarkup # EmailAddress   => a.value
            case a: PlainTextMarkup # Monospace      => if (includeMarkup) '`' ~ a.value ~ '`' else a.value
            case a: PlainTextMarkup # TeX            => G.texSurround(a.value)
            case a: PlainTextMarkup # WebAddress     => a.value
            case a: TagRef          # TagRef         => tagRef(a.value)
            case a: Headings        # Heading1       => heading("#", a.title)
            case a: Headings        # Heading2       => heading("##", a.title)
            case a: Headings        # Heading3       => heading("###", a.title)
            case a: Headings        # Heading4       => heading("####", a.title)
            case a: Headings        # Heading5       => heading("#####", a.title)
            case a: Headings        # Heading6       => heading("######", a.title)
            case a: PlainTextMarkup # Bold           => style("**", a.inner)
            case a: PlainTextMarkup # Italic         => style("//", a.inner)
            case a: PlainTextMarkup # Strikethrough  => style("~~", a.inner)
            case a: PlainTextMarkup # Underline      => style("__", a.inner)
            case a: ListMarkup      # OrderedList    => list(a, olLead())
            case a: ListMarkup      # UnorderedList  => list(a, ulLead)

            // ---------------------------------------------------------------------------------------------------------
            case a: CodeBlock # CodeBlock =>

              val firstLine: String =
                if (includeMarkup)
                  a.detail match {
                    case Some(d) => "```" ~ d.toText ~ '\n'
                    case None    => "```\n"
                  }
                else
                  ""

              if (indent.isEmpty) {
                // top-level

                val head =
                  if (acc.isEmpty || acc.endsWith("\n\n"))
                    "" // no top-margin required
                  else if (acc.endsWith("\n"))
                    "\n" // shouldn't happen but just in case - ensure our top-margin is only one line
                  else
                    "\n\n" // add top-margin of one line

                val tail = if (nextIsEmpty) "" else "\n\n"

                val lastLine = if (includeMarkup) "\n```" else ""

                head ~ firstLine ~ a.code ~ lastLine ~ tail

              } else {
                // we're in a list

                val head =
                  if (acc == "* " || acc.endsWith("\n* ") || acc.endsWith("\n" ~ indent))
                    "" // no top-margin or indentation required
                  else if (acc.endsWith("\n"))
                    "\n" ~ indent // shouldn't happen but just in case - ensure our top-margin is only one line
                  else
                    "\n\n" ~ indent // add top-margin of one line, and indentation

                val tail = if (nextIsEmpty) "" else "\n\n" ~ indent

                val lastLine = if (includeMarkup) "\n" ~ indent ~ "```" else ""

                head ~ firstLine ~ a.code.indentLines(indent) ~ lastLine ~ tail
              }

          }
          // -----------------------------------------------------------------------------------------------------------

          val nextAcc = acc ~ cur
          if (nextIsEmpty)
            nextAcc
          else
            go(nextAcc, nextIdx, seenMultilineAtThisLevelYet || atom.containsMultipleLines)
        }

        if (atoms.isEmpty)
          acc
        else
          go(acc, 0, seenMultilineAtThisLevelYet = false)
      }

      nextLevel("", "", atoms)
    }

    private def _reqRef(display: DisplayReqRef, includeMarkup: Boolean)
                       (id: String, title: => String): String = {
      val label =
        if (includeMarkup)
          display match {
            case DisplayReqRef.AsId         => id
            case DisplayReqRef.AsIdAndTitle => id ~ ":"
          }
        else
          display match {
            case DisplayReqRef.AsId         => id
            case DisplayReqRef.AsIdAndTitle => s"$id: $title"
          }
      G.reflinkSurround(label)
    }

    private def codeRef(id: ReqCodeId, display: DisplayReqRef, includeMarkup: Boolean): String = {
      import ProjectText.ReqCodeResolution, ReqCodeResolution._

      def withCode(c: ReqCode.Value, title: => String) =
        _reqRef(display, includeMarkup)(
          id    = reqCode(c),
          title = title,
        )

      ReqCodeResolution(id, p.content.reqCodes) match {
        case ActiveCodeToReq     (c, r) => withCode(c, reqTitleWithoutMarkupById(r))
        case ActiveCodeToGroup   (c, g) => withCode(c, codeGroupTitle(g))
        case DeadGroup           (c, g) => withCode(c, codeGroupTitle(g))
        case ReqWithAltCode      (c, r) => withCode(c, reqTitleWithoutMarkupById(r))
        case ReqWithoutActiveCode(_, r) => reqRef(r, display, includeMarkup)
      }
    }

    private def reqRef(id: ReqId, display: DisplayReqRef, includeMarkup: Boolean): String = {
      val req      = p.content.reqs.need(id)
      val rt       = p.config.reqTypes.need(req.pubid.reqTypeId)
      def live     = req.live(p.config.reqTypes)
      _reqRef(display, includeMarkup)(
        id    = pubid(rt, req.pubid.pos),
        title = textWithoutMarkup(req.title, live),
      )
    }

    private def tagRef(id: ApplicableTagId): String = {
      val t = p.config.tags.needApplicableTag(id)
      hashtag(t.key)
    }

    def useCaseStepLabel(focus: UseCaseStep.Focus): String = {
      val fmt = gctx match {
     // case ProjectText.Context.Req(i: UseCaseId) if i ==* uc.id => UseCaseStepLabelFmt.    `.m` // looks too confusing
        case ProjectText.Context.Req(_: UseCaseId)                => UseCaseStepLabelFmt.   `N.m`
        case ProjectText.Context.None
           | ProjectText.Context.Req(_: GenericReqId)             => UseCaseStepLabelFmt.`UC-N.m`
      }
      focus.label(fmt)
    }

    private def useCaseStepLabelById(id: UseCaseStepId): String =
      useCaseStepLabel(p.content.reqs.useCases.focusStep(id))

    private def useCaseStepRef(id: UseCaseStepId): String =
      G.reflinkSurround(useCaseStepLabelById(id))

    override def pastPubids(ids: SortedSet[ExternalPubid]): String =
      ids.iterator.map(pubid(_)).mkString(", ")

    override def reqCode(c: ReqCode.Value): String =
      PlainText.reqCode(c)

    override def reqCodes(reqCodes: IterableOnce[ReqCode.Value]): String =
      reqCodes.iterator.map(reqCode).mkString("\n")

    override def reqCodeTree(items: Vector[ReqCodeTreeItem]): String =
      items.iterator.map(reqCodeTreeItem).mkString("\n")

    override def reqCodeTreeItem(item: ReqCodeTreeItem): String =
      PlainText.reqCodeTreeItem(item)

    override def reqTypeShort(id: ReqTypeId): String =
      p.config.reqTypes.get(id).fold("?")(PlainText.reqTypeShort)

    override def reqTypeFull(id: ReqTypeId): String =
      p.config.reqTypes.get(id).fold("?")(PlainText.reqTypeFull)

    def text(text: Text.AnyOptional, live: Live, mandatory: Mandatory): String =
      this.text(text, live, Valid.always, mandatory) // Valid.always because TagValidity doesn't affect PlainText output

    override def useCaseStepTextAndFlow(step: UseCaseStepFlowText.TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]],
                                        live: Live): String =
      Util.quickSB { sb =>
        sb append text(step.text, live, Mandatory.when(step.flow.forall(_.isEmpty)))
        for (d <- UseCaseStepFlowText.DefaultArrowOrder) {
          val ids = step.flow(d)
          if (ids.nonEmpty) {
            if (sb.nonEmpty) sb append ' '
            sb append UseCaseStepFlowText.AsciiArrows(d)
            for (ref <- useCaseFlowElementsById(ids).iterator()) {
              sb append ' '
              sb append ref
            }
          }
        }
      }

    override def withCtx[Ctx2 <: ProjectText.Context](newCtx: Ctx2): ForProject[Ctx2] =
      if (newCtx ==* ctx)
        this.asInstanceOf[ForProject[Ctx2]]
      else
        ForProject(p, newCtx)

    def tagListWithHashtags(ids: Vector[ApplicableTagId]): String =
      ids.iterator.map(p.config.tags.needApplicableTag(_).key.with_#).mkString(" ")

    val reqTitleWithoutMarkup: Req => String = {
      def make(t: Text.AnyOptional, req: Req) = textWithoutMarkup(t, req.live(cfg.reqTypes))
      memoByReqId {
        case gr: GenericReq => make(gr.title, gr)
        case uc: UseCase    => make(uc.title, uc)
      }
    }

    def reqTitleWithoutMarkupById(id: ReqId): String =
      reqTitleWithoutMarkup(p.content.reqs.need(id))

    def textWithoutMarkup(text: Text.AnyOptional, live: Live): String =
      nestedText(live, text, includeMarkup = false)
  }
}
