package shipreq.webapp.base.text

import scala.annotation.tailrec
import shipreq.base.util._
import shipreq.base.util.SafeStringOps._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G}
import shipreq.webapp.base.text.GrammarSpec.Surrounds
import shipreq.webapp.base.util.ReqCodeTreeItem
import Atom.AnyAtom

/**
 * Turns elements of data into user-facing plain text.
 */
object PlainText {

  // ScalaJS StringBuilder just uses String concatenation so fuck it.

  private implicit def surroundDisplay(s: Surrounds) = s.display

  private implicit class OptionalTextOps[T <: Text.Generic](val _t: T#OptionalText) extends AnyVal {
    @inline def asOption: Option[_t.type] =
      if (_t.isEmpty) None else Some[_t.type](_t)

    @inline def net: Option[T#NonEmptyText] =
      NonEmptyVector.option(_t)
  }

  // -------------------------------------------------------------------------------------------------------------------

  def reqCodeIndentation(is: NonEmptyVector[ReqCodeTreeItem.Indent]): String = {
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
    ReqCode.valueToStr(c, G.reqCode.nodeSeparator)

  def hashtag(key: HashRefKey): String =
    G.hashRefKey.prefix ~ key.value

  def pubid(p: Project, id: ReqId): String = {
    val pid = p.reqs.req(id).pubid
    pubid(p, pid)
  }

  def pubid(p: Project, pid: Pubid): String = {
    val rt = p.config.reqType(pid.reqTypeId)
    pubid(rt, pid.pos)
  }

  def pubid(reqType: ReqType, pos: ReqTypePos): String =
    pubid(reqType.mnemonic, pos)

  def pubid(externalPubid: ExternalPubid): String =
    pubid(externalPubid.mnemonic, externalPubid.pos)

  def pubid(mnemonic: ReqType.Mnemonic, pos: ReqTypePos): String =
    mnemonic.value ~ "-" ~ pos.value

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private final val bullet = "* "

  @inline def apply(p: Project, ctx: ProjectText.Context): ForProject =
    new ForProject(p, ctx)

  final class ForProject(p: Project, ctx: ProjectText.Context) extends ProjectText[String](p, ctx) {

    override def withCtx(newCtx: ProjectText.Context): ForProject =
      new ForProject(p, newCtx)

    def useCaseStepLabel(id: UseCaseStepId): String =
      useCaseStepLabel(p.reqs.useCases.focusStep(id))

    def useCaseStepLabel(focus: UseCaseStep.Focus): String = {
      import focus._
      val mne  = ctx match {
        case ProjectText.Context.None       => true
        case ProjectText.Context.UseCase(i) => i !=* uc.id
      }
      field.stepLabel(uc.pubid.pos, ploc, mnemonicPrefix = mne)
    }

    private def codeRef(id: ReqCodeId): String = {
      import ProjectText.ReqCodeResolution._
      ProjectText.resolveReqCode(id, p.reqCodes) match {
        case ActiveCodeToReq     (c, _) => G reflinkSurround reqCode(c)
        case ActiveCodeToGroup   (c, _) => G reflinkSurround reqCode(c)
        case DeadGroup           (c, _) => G reflinkSurround reqCode(c)
        case ReqWithAltCode      (c, _) => G reflinkSurround reqCode(c)
        case ReqWithoutActiveCode(_, r) => reqRef(r)
      }
    }

    private def reqRef(req: ReqId): String = {
      val pid = p.reqs.req(req).pubid
      val rt  = p.config.reqType(pid.reqTypeId)
      G.reflinkSurround(pubid(rt, pid.pos))
    }

    private def useCaseStepRef(id: UseCaseStepId): String =
      G.reflinkSurround(useCaseStepLabel(id))

    private def tagRef(id: ApplicableTagId): String = {
      val t = p.config.atag(id)
      hashtag(t.key)
    }

    private def issue(id: CustomIssueTypeId, desc: Option[String]): String = {
      val it = p.config.customIssueType(id)
      desc.foldLeft(hashtag(it.key))(_ ~ G.issueDescSurround(_))
    }

    private val outOfListNewline = "\n\n"

    override val format: ProjectText.FormatAtomFn[String] = {
      def nest(acc: String, newline: String, live: Live, atoms: Vector[AnyAtom]): String = {
        @tailrec def go(acc: String, atoms: Vector[AnyAtom]): String =
          if (atoms.isEmpty)
            acc
          else {
            val nextAtoms = atoms.tail
            import Atom._
            val cur = atoms.head match {
              case a: Literal         # Literal        => a.value
              case a: NewLine         # BlankLine      => newline
              case a: ReqRef          # ReqRef         => reqRef(a.value)
              case a: ReqRef          # CodeRef        => codeRef(a.value)
              case a: UseCaseStepRef  # UseCaseStepRef => useCaseStepRef(a.value)
              case a: Issue           # Issue          => issue(a.typ, a.desc.asOption map (run(live, _)))
              case a: PlainTextMarkup # WebAddress     => a.value
              case a: PlainTextMarkup # EmailAddress   => a.value
              case a: PlainTextMarkup # MathTeX        => G.mathTexSurround(a.value)
              case a: TagRef          # TagRef         => tagRef(a.value)
              case a: ListMarkup      # UnorderedList  =>
                val listNL = if (newline eq outOfListNewline) "\n  " else newline ~ "  "
                val r = a.items.foldLeft("") { (q, li) =>
                  val pre = if (q.isEmpty && acc.isEmpty) bullet else q ~ newline ~ bullet
                  nest(pre, listNL, live, li)
                }
                if (nextAtoms.isEmpty) r else r ~ newline
            }
            go(acc ~ cur, nextAtoms)
          }
        go(acc, atoms)
      }

      @inline def run(live: Live, atoms: Vector[AnyAtom]) = nest("", outOfListNewline, live, atoms)
      run
    }

    override def useCaseStep(l: Live, s: UseCaseStep[Set[UseCaseStepId]]): String =
      useCaseStepA(l, s, UseCaseStepFlowText.AsciiArrows)

    private def useCaseStepA(l: Live, s: UseCaseStep[Set[UseCaseStepId]], arrows: Direction => String): String =
      Util.quickSB { sb =>
        sb append format(l, s.text)
        for (d <- UseCaseStepFlowText.DefaultArrowOrder) {
          val ids = s.flow(d)
          if (ids.nonEmpty) {
            if (sb.nonEmpty) sb append ' '
            sb append arrows(d)
            for (ref <- useCaseFlowStepsOrdered(ids)) {
              sb append ' '
              sb append ref
            }
          }
        }
      }

    override protected def useCaseFlowStep(f: UseCaseStep.Focus): String =
      useCaseStepLabel(f)

  }
}
