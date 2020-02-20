package shipreq.webapp.base.text

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.utils.Memo
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import shipreq.base.util._
import shipreq.base.util.SafeStringOps._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.text.GrammarSpec.Surrounds
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.base.UiText
import Atom.AnyAtom

/**
 * Turns elements of data into user-facing plain text.
 */
object PlainText {

  object ForProject {
    type AnyCtx = ForProject[ProjectText.Context]
    type NoCtx  = ForProject[ProjectText.Context.None]

    def apply[Ctx <: ProjectText.Context](p: Project, ctx: Ctx): ForProject[Ctx] =
      new ForProject(p, ctx)

    def noCtx(p: Project): NoCtx =
      apply(p, ProjectText.Context.None)
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
      NonEmptyVector.option(_t)
  }

  private final val bullet = "* "

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class ForProject[+Ctx <: ProjectText.Context](p: Project, ctx: Ctx) extends ProjectText[Ctx, String](p, ctx) {

    override protected def _implicationList(ids: Vector[Pubid]): String =
      ids.iterator.map(pubid(_, p)).mkString(", ")

    override protected def _tagList(ids: Vector[ApplicableTagId], validity: ApplicableTagId => Validity): String =
      ids.iterator.map(p.config.tags.atag(_).key.value).mkString(" ")

    override protected def _text(text: Text.AnyOptional, live: Live): String =
      nestedText("", "", live, text)

    // Keep in sync with ProjectWidgets because it's used together for sorting/rendering in ReqTable
    override protected def deletionReasonWhenNoneGiven: String =
      ""

    // Keep in sync with ProjectWidgets because it's used together for sorting/rendering in ReqTable
    override protected def deletionReasonWhenReqTypeIsDead(rt: ReqType): String =
      UiText.ColumnNames.reqType + " " + rt.mnemonic.value + " is deleted."

    override protected def emptyText = ""

    override protected val useCaseFlowElement: UseCaseStep.Focus => String =
      useCaseStepLabel

    override protected def whenBlankButMandatory = ""

    private def codeRef(id: ReqCodeId): String = {
      import ProjectText.ReqCodeResolution, ReqCodeResolution._
      ReqCodeResolution(id, p.content.reqCodes) match {
        case ActiveCodeToReq     (c, _) => G reflinkSurround reqCode(c)
        case ActiveCodeToGroup   (c, _) => G reflinkSurround reqCode(c)
        case DeadGroup           (c, _) => G reflinkSurround reqCode(c)
        case ReqWithAltCode      (c, _) => G reflinkSurround reqCode(c)
        case ReqWithoutActiveCode(_, r) => reqRef(r)
      }
    }

    private def issue(id: CustomIssueTypeId, desc: Option[String]): String = {
      val it = p.config.customIssueType(id)
      desc.foldLeft(hashtag(it.key))(_ ~ G.issueDescSurround(_))
    }

    private def nestedText(acc: String, indent: String, live: Live, atoms: Vector[AnyAtom]): String = {
      @tailrec def go(acc: String, atoms: Vector[AnyAtom]): String =
        if (atoms.isEmpty)
          acc
        else {
          val nextAtoms = atoms.tail
          import Atom._
          val cur = atoms.head match {
            case a: Literal         # Literal        => a.value
            case _: NewLine         # BlankLine      => "\n\n" ~ indent
            case a: ContentRef      # ReqRef         => reqRef(a.value)
            case a: ContentRef      # CodeRef        => codeRef(a.value)
            case a: ContentRef      # UseCaseStepRef => useCaseStepRef(a.value)
            case a: Issue           # Issue          => issue(a.typ, a.desc.asOption.map(text(_, live, Mandatory.Not)))
            case a: PlainTextMarkup # EmailAddress   => a.value
            case a: PlainTextMarkup # Monospace      => '`' ~ a.value ~ '`'
            case a: PlainTextMarkup # TeX            => G.texSurround(a.value)
            case a: PlainTextMarkup # WebAddress     => a.value
            case a: TagRef          # TagRef         => tagRef(a.value)

            // ---------------------------------------------------------------------------------------------------------
            case a: ListMarkup      # UnorderedList  =>
              val nextIndent = indent + "  "

              val prefix: String => String =
                if (a.itemsContainMultipleLines)
                  q =>
                    if (q.isEmpty)
                      (if (acc.isEmpty) bullet else "\n\n" ~ bullet)
                    else
                      q ~ "\n\n" ~ bullet
                else
                  q =>
                    if (q.isEmpty && acc.isEmpty)
                      bullet
                    else
                      q ~ "\n" ~ bullet

              val r = a.items.foldLeft("")((q, li) => nestedText(prefix(q), nextIndent, live, li))

              if (nextAtoms.isEmpty) r else r ~ "\n\n"

            // ---------------------------------------------------------------------------------------------------------
            case a: CodeBlock # CodeBlock =>

              val firstLine: String =
                a.language match {
                  case Some(lang) => "```" ~ lang ~ '\n'
                  case None       => "```\n"
                }

              if (indent.isEmpty) {
                // top-level

                val head =
                  if (acc.isEmpty || acc.endsWith("\n\n"))
                    "" // no top-margin required
                  else if (acc.endsWith("\n"))
                    "\n" // shouldn't happen but just in case - ensure our top-margin is only one line
                  else
                    "\n\n" // add top-margin of one line

                val tail = if (nextAtoms.isEmpty) "" else "\n\n"

                head ~ firstLine ~ a.code ~ "\n```" ~ tail

              } else {
                // we're in a list

                val head =
                  if (acc == "* " || acc.endsWith("\n* ") || acc.endsWith("\n" ~ indent))
                    "" // no top-margin or indentation required
                  else if (acc.endsWith("\n"))
                    "\n" ~ indent // shouldn't happen but just in case - ensure our top-margin is only one line
                  else
                    "\n\n" ~ indent // add top-margin of one line, and indentation

                val tail = if (nextAtoms.isEmpty) "" else "\n\n" ~ indent

                head ~ firstLine ~ a.code.indent(indent) ~ "\n" ~ indent ~ "```" ~ tail
              }

          }
          // -----------------------------------------------------------------------------------------------------------

          go(acc ~ cur, nextAtoms)
        }
      go(acc, atoms)
    }

    private def reqRef(req: ReqId): String = {
      val pid = p.content.reqs.need(req).pubid
      val rt  = p.config.reqTypes.need(pid.reqTypeId)
      G.reflinkSurround(pubid(rt, pid.pos))
    }

    private def tagRef(id: ApplicableTagId): String = {
      val t = p.config.tags.atag(id)
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

    override def reqCodes(reqCodes: TraversableOnce[ReqCode.Value]): String =
      reqCodes.toIterator.map(reqCode).mkString("\n")

    override def reqCodeTree(items: Vector[ReqCodeTreeItem]): String =
      items.toIterator.map(reqCodeTreeItem).mkString("\n")

    override def reqCodeTreeItem(item: ReqCodeTreeItem): String =
      PlainText.reqCodeTreeItem(item)

    override def reqTypeShort(id: ReqTypeId): String =
      PlainText.reqTypeShort(p.config.reqTypes.need(id))

    override def reqTypeFull(id: ReqTypeId): String =
      PlainText.reqTypeFull(p.config.reqTypes.need(id))

    override def useCaseStepTextAndFlow(step: UseCaseStepFlowText.TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]],
                                        live: Live): String =
      Util.quickSB { sb =>
        sb append text(step.text, live, Mandatory.when(step.flow.forall(_.isEmpty)))
        for (d <- UseCaseStepFlowText.DefaultArrowOrder) {
          val ids = step.flow(d)
          if (ids.nonEmpty) {
            if (sb.nonEmpty) sb append ' '
            sb append UseCaseStepFlowText.AsciiArrows(d)
            for (ref <- useCaseFlowElementsById(ids).iterator) {
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
  }
}
