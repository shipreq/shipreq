package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import shipreq.base.util.SafeStringOps._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Contextualise, Plain, _}
import shipreq.webapp.base.jsfacade.KaTeX
import shipreq.webapp.base.lib.ClientUtil.{renderSeq, renderVector, sepComma, sepSpace}
import shipreq.webapp.base.text.Atom.DisplayReqRef
import shipreq.webapp.base.text.Text.AnyOptional
import shipreq.webapp.base.text.{Grammar => G, _}
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.project.app.Style.{widgets => *}
import shipreq.webapp.client.project.app.WebWorkerClient

object ProjectWidgets {

  type AnyCtx = ProjectWidgets[ProjectText.Context]
  type NoCtx  = ProjectWidgets[ProjectText.Context.None]

  def apply[Ctx <: ProjectText.Context](project    : Project,
                                        plainText  : PlainText.ForProject[Ctx],
                                        reqDetailRC: RouterCtl[ExternalPubid],
                                        webWorker  : WebWorkerClient.Instance): ProjectWidgets[Ctx] =
    new ProjectWidgets(project, plainText, reqDetailRC, webWorker)

  implicit def subst1[F[_], C <: ProjectText.Context](pw: F[ProjectWidgets[C]]): F[ProjectWidgets.AnyCtx] =
    pw.asInstanceOf[F[AnyCtx]]

  implicit def subst2[F[_], G[_], C <: ProjectText.Context](pw: F[G[ProjectWidgets[C]]]): F[G[ProjectWidgets.AnyCtx]] =
    pw.asInstanceOf[F[G[AnyCtx]]]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val emptySpan: VdomTag =
    <.span

  val blankButMandatory: VdomTag =
    <.div(*.blankButMandatory, "blank")

  private[ProjectWidgets] object Internal {

    val deadValidity: Validity => Live => (Live, Validity) =
      Validity.memo(validityWhenDead =>
        Live.memo {
          case Live => (Live, Valid)
          case Dead => (Dead, validityWhenDead)
        }
      )

    val invalidWhenDead: Live => (Live, Validity) =
      deadValidity(Invalid)

    val stepFlowClauseBase: Direction => VdomTag = {
      val base = <.div(*.useCaseStepFlowClause)
      Direction.memo {
        case Forwards  => base("→")
        case Backwards => base("←")
      }
    }

    @inline implicit def surroundDisplay(s: GrammarSpec.Surrounds): GrammarSpec.Surround =
      s.display

    val issueDescSurroundPrefix = G.issueDescSurround.prefix.trim
    val issueDescSurroundSuffix = G.issueDescSurround.suffix.trim
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class ProjectWidgets[+Ctx <: ProjectText.Context](project      : Project,
                                                        val plainText: PlainText.ForProject[Ctx],
                                                        reqDetailRC  : RouterCtl[ExternalPubid],
                                                        webWorker    : WebWorkerClient.Instance)
    extends ProjectText[Ctx, VdomTag](project, plainText.ctx) {

  import ProjectWidgets.Internal._

  override protected def _implicationList(ids: Vector[Pubid]): VdomTag =
    PubidFormat.validWhenDead.pubids(ids)

  override protected def _tagList(ids: Vector[ApplicableTagId], validity: ApplicableTagId => Validity): VdomTag =
    renderVector(ids, sepSpace)(id => tagPlain(validity(id))(id))

  override protected def _text(text: AnyOptional, live: Live, tagValidity: ApplicableTagId => Validity): VdomTag =
    <.span(text.map(textAtom(live, tagValidity)): _*)

  // Keep in sync with PlainText because it's used together for sorting/rendering in ReqTable
  override protected def deletionReasonWhenNoneGiven: VdomTag =
    ProjectWidgets.emptySpan

  // Keep in sync with PlainText because it's used together for sorting/rendering in ReqTable
  override protected def deletionReasonWhenReqTypeIsDead(rt: ReqType): VdomTag =
    <.span(
      SpecialBuiltInField.ReqType.name + " ",
      reqTypeShort(rt.reqTypeId),
      " is deleted.")

  override protected def emptyText =
    ProjectWidgets.emptySpan

  override protected val useCaseFlowElement: UseCaseStep.Focus => VdomTag =
    Memo.by((_: UseCaseStep.Focus).id)(
      mkUseCaseStep((base, ld, label) =>
        base(
          *.useCaseStepFlowElement(ld),
          label)))

  override protected def whenBlankButMandatory =
    ProjectWidgets.blankButMandatory

  private def issue(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText, liveText: Live): VdomTag =
    NonEmptyArraySeq.option(desc) match {
      case None       => issueWithoutDesc(id)(liveText)
      case Some(desc) => issueWithDesc(id, desc, liveText)
    }

  private val issueWithoutDesc: CustomIssueTypeId => Live => VdomTag =
    Memo { id =>
      val issueType = cfg.customIssueTypes.need(id)
      Live.memo { liveText =>
        <.span(
          *.issue((liveText, issueType.live)),
          G.hashRefKey.prefix ~ issueType.key.value)
      }
    }

  private def issueWithDesc(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText, liveText: Live): VdomTag = {
    val issueType = project.config.customIssueTypes.need(id)
    <.span(
      *.issue((liveText, issueType.live)),
      G.hashRefKey.prefix ~ issueType.key.value ~ issueDescSurroundPrefix,
      text(desc, liveText, Valid.always)(*.issueDesc),
      issueDescSurroundSuffix)
  }

  private def katex(m: Atom.PlainTextMarkup#TeX): VdomTag =
    try
      <.span(*.katex, ^.dangerouslySetInnerHtml := KaTeX.renderToStringUnsafe(m.value))
    catch {
      case _: Throwable => <.span(*.katexFail, UiText.mathFailed)
    }

  private def linkOrSpan(req: Req): VdomTag =
    linkOrSpan(req, req.pubid.external(project))

  private def linkOrSpan(req: Req, ep: ExternalPubid): VdomTag =
    gctx match {
      case ProjectText.Context.Req(id) if req.id ==* id => ProjectWidgets.emptySpan
      case ProjectText.Context.None
         | _: ProjectText.Context.Req                   => reqDetailRC.link(ep)
    }

  private val ucTagValidity: ApplicableTagId => Validity =
    project.config.naTags(StaticReqType.UseCase)

  private def makeUseCaseStepTextAndFlow[C[x] <: Iterable[x], A](s: UseCaseStepFlowText.TextAndFlow[AnyOptional, C[A]],
                                                                 l: Live)
                                                                (render: C[A] => IterableOnce[VdomTag]): VdomTag = {

    val stepText = text(s.text, l, ucTagValidity, Mandatory.when(s.flow.forall(_.isEmpty)))

    def flowClause(dir: Direction): Option[VdomTag] = {
      val flowElements = s flow dir
      Option.unless(flowElements.isEmpty)(
        stepFlowClauseBase(dir)(
          TagMod.fromTraversableOnce(
            render(flowElements).iterator.map(t => t: TagMod).intersperse(sepComma))))
    }

    val flowsMaybe: Option[VdomTag] =
      UseCaseStepFlowText.DefaultArrowOrder.map(flowClause) match {
        case (None   , None   ) => None
        case (Some(f), None   ) => Some(f)
        case (None   , Some(f)) => Some(f)
        case (Some(a), Some(b)) => Some(<.div(*.useCaseStepTextAndFlow_flow, a, b))
      }

    <.div(*.useCaseStepTextAndFlow_cont(l),
      <.div(*.useCaseStepTextAndFlow_text, stepText),
      flowsMaybe)
  }

  private def mkUseCaseStep[A](r: (VdomTag, (Live, Validity), String) => A)(f: UseCaseStep.Focus): A = {
    val label = plainText.useCaseStepLabel(f)
    val title = UiText.hoverText(plainText.textWithoutMarkup(f.titleA, f.live))
    val ld = deadValidity(Invalid)(f.live)
    val base = linkOrSpan(f.uc)(^.title := title)
    r(base, ld, label)
  }

  /** Contextualised */
  private val codeRef: Live => ReqCodeId => DisplayReqRef => VdomTag =
    Live.memo { live =>
      Memo { id =>
        import ProjectText.ReqCodeResolution._

        implicit def liveWithValidity(a: Live): (Live, Validity) =
          invalidWhenDead(a)

        DisplayReqRef.memoLazy {

          // -----------------------------------------------------------------------------------------------------------
          case d@ DisplayReqRef.AsId =>
            def ref(base: VdomTag, code: ReqCode.Value, title: String): VdomTag =
              base(
                ^.title := UiText.hoverText(title),
                G.reflinkSurround(PlainText.reqCode(code)))

            def toReq(code: ReqCode.Value, r: ReqId): VdomTag = {
              val req = project.content.reqs.need(r)
              ref(
                base  = linkOrSpan(req)(*.reqRef(req live project.config.reqTypes)),
                code  = code,
                title = plainText reqTitleWithoutMarkup req)
            }

            def toGroup(code: ReqCode.Value, g: CodeGroup): VdomTag =
              ref(
                base  = <.span(*.codeGroupRef(g.live)),
                code  = code,
                title = plainText.codeGroupTitle(g))

            ProjectText.ReqCodeResolution(id, project.content.reqCodes) match {
              case ActiveCodeToReq     (c, r) => toReq(c, r)
              case ReqWithAltCode      (c, r) => toReq(c, r)
              case ActiveCodeToGroup   (c, g) => toGroup(c, g)
              case DeadGroup           (c, g) => toGroup(c, g)
              case ReqWithoutActiveCode(_, r) => reqRef(live)(d)(r)
            }

          // -----------------------------------------------------------------------------------------------------------
          case d@ DisplayReqRef.AsIdAndTitle =>
            def ref(base: VdomTag, code: ReqCode.Value, title: TagMod): VdomTag =
              base(
                G.reflinkSurround.prefix,
                PlainText.reqCode(code),
                ": ",
                title,
                G.reflinkSurround.suffix)

            def toReq(code: ReqCode.Value, r: ReqId): VdomTag = {
              val req = project.content.reqs.need(r)
              ref(
                base  = linkOrSpan(req)(*.reqRef(req live project.config.reqTypes)),
                code  = code,
                title = reqTitle(req))
            }

            def toGroup(code: ReqCode.Value, g: CodeGroup): VdomTag =
              ref(
                base  = <.span(*.codeGroupRef(g.live)),
                code  = code,
                title = codeGroupTitle(g))

            ProjectText.ReqCodeResolution(id, project.content.reqCodes) match {
              case ActiveCodeToReq     (c, r) => toReq(c, r)
              case ReqWithAltCode      (c, r) => toReq(c, r)
              case ActiveCodeToGroup   (c, g) => toGroup(c, g)
              case DeadGroup           (c, g) => toGroup(c, g)
              case ReqWithoutActiveCode(_, r) => reqRef(live)(d)(r)
            }
        }
      }
    }

  private val reqRef: Live => DisplayReqRef => ReqId => VdomTag = {
    Live.memo { liveText =>

      val pubidFmt = liveText match {
        case Live => PubidFormat.invalidWhenDeadWithCtx
        case Dead => PubidFormat.validWhenDead
      }

      DisplayReqRef.memoLazy {
        case DisplayReqRef.AsId         => pubidFmt(_)
        case DisplayReqRef.AsIdAndTitle => id => pubidFmt.withSuffix(id, false)(": ", reqTitleById(id))
      }
    }
  }

  private val tagWithoutStyleMemo: Boolean => Contextualise => ApplicableTag => VdomTag =
    Memo.bool { includeDesc =>
      Contextualise.memo { c =>

        def render(t: ApplicableTag): VdomTag = {
          var desc = ""
          if (includeDesc) {
            desc = if (t.name.compareToIgnoreCase(t.key.value) == 0) "" else t.name
            for (d <- t.desc) {
              if (desc.nonEmpty)
                desc += "\n\n"
              desc += d
            }
          }
          val keyTxt = t.key.value
          val displayTxt = c match {
            case Contextualise => G.hashRefKey.prefix ~ keyTxt
            case Plain         => keyTxt
          }
          <.span(
            TagMod.when(desc.nonEmpty)(^.title := desc),
            displayTxt)
        }

        val memo = Memo.by((_: ApplicableTag).id)(render)

        tag =>
          // TagConfig create a fake ApplicableTag with id of -1 to render new tags live before they're saved
          if (tag.id.value >= 0)
            memo(tag)
          else
            render(tag)
      }
    }

  private val h1            = <.h1(*.h1)
  private val h2            = <.h2(*.h2)
  private val h3            = <.h3(*.h3)
  private val h4            = <.h4(*.h4)
  private val h5            = <.h5(*.h5)
  private val h6            = <.h6(*.h6)
  private val bold          = <.strong
  private val italic        = <.em
  private val strikethrough = <.span(*.strikethrough)
  private val underline     = <.span(*.underline)

  private def tagWithoutStyle(c: Contextualise, t: ApplicableTag, includeDesc: Boolean): VdomTag =
    tagWithoutStyleMemo(includeDesc)(c)(t)

  private def textAtom(liveText: Live, tagValidity: ApplicableTagId => Validity): Atom.AnyAtom => TagMod = {
    import Atom._

    def wrapped(tag: VdomTag, inner: NonEmptyArraySeq[Atom.AnyAtom]) =
      tag(inner.whole.toTagMod(atom))

    def list(tag: VdomTag, a: Atom.ListMarkup # ListBase) = {
      val style = if (a.itemsContainMultipleLines) *.ulSpacious else *.ulCompact
      tag(style, a.items.whole.toTagMod(row => <.li(row toTagMod atom)))
    }

    lazy val atom: AnyAtom => TagMod = {
      case a: Literal         # Literal        => <.span(a.value)
      case a: CodeBlock       # CodeBlock      => RichCodeBlock.Props(a.detail, a.code, webWorker).render
      case a: ContentRef      # CodeRef        => codeRef(liveText)(a.id)(a.display)
      case a: ContentRef      # ReqRef         => reqRef(liveText)(a.display)(a.id)
      case a: ContentRef      # UseCaseStepRef => useCaseStepRefById(a.value)
      case a: Headings        # Heading1       => wrapped(h1, a.title)
      case a: Headings        # Heading2       => wrapped(h2, a.title)
      case a: Headings        # Heading3       => wrapped(h3, a.title)
      case a: Headings        # Heading4       => wrapped(h4, a.title)
      case a: Headings        # Heading5       => wrapped(h5, a.title)
      case a: Headings        # Heading6       => wrapped(h6, a.title)
      case a: Issue           # Issue          => issue(a.typ, a.desc, liveText)
      case a: ListMarkup      # OrderedList    => list(<.ol, a)
      case a: ListMarkup      # UnorderedList  => list(<.ul, a)
      case _: NewLine         # BlankLine      => <.div(*.blankLine)
      case a: PlainTextMarkup # Bold           => wrapped(bold, a.inner)
      case a: PlainTextMarkup # EmailAddress   => <.a(^.href := "mailto:" ~ a.value, a.value)
      case a: PlainTextMarkup # Italic         => wrapped(italic, a.inner)
      case a: PlainTextMarkup # Monospace      => <.pre(*.monospace, a.value)
      case a: PlainTextMarkup # Strikethrough  => wrapped(strikethrough, a.inner)
      case a: PlainTextMarkup # TeX            => katex(a)
      case a: PlainTextMarkup # Underline      => wrapped(underline, a.inner)
      case a: PlainTextMarkup # WebAddress     => <.a(^.href := a.value, a.value)

      case a: TagRef          # TagRef         =>
        val tag = project.config.tags.needApplicableTag(a.value)
        val valid = tagValidity(tag.id) & Invalid.when(liveText.is(Live) && tag.live.is(Dead))
        tagWithoutStyle(Contextualise, tag, includeDesc = true)(*.tagInText((tag.live, valid)))
    }

    atom
  }

  private val useCaseStepRef: UseCaseStep.Focus => VdomTag =
    Memo.by((_: UseCaseStep.Focus).id)(
      mkUseCaseStep((base, ld, label) =>
        base(
          *.useCaseStepRef(ld),
          G.reflinkSurround(label))))

  private def useCaseStepRefById(id: UseCaseStepId): VdomTag =
    useCaseStepRef(project.content.reqs.useCases.focusStep(id))

  override def pastPubids(ids: SortedSet[ExternalPubid]): VdomTag =
    renderSeq(
      ids.iterator.map(ep => <.span(*.pastPubid, PlainText.pubid(ep))),
      sepComma)

  override def reqCode(c: ReqCode.Value): VdomTag =
    <.pre(*.reqCodeFlat, PlainText reqCode c)

  override def reqCodes(reqCodes: IterableOnce[ReqCode.Value]): VdomTag =
    <.div(reqCodes toTagMod reqCode)

  override def reqCodeTree(items: Vector[ReqCodeTreeItem]): VdomTag =
    <.div(items toTagMod reqCodeTreeItem)

  private val reqCodeTreeIdentation: NonEmptyVector[ReqCodeTreeItem.Indent] => VdomTag =
    Memo(is =>
      <.pre(*.reqCodeTreeIndent, PlainText.reqCodeIndentation(is)))

  override def reqCodeTreeItem(item: ReqCodeTreeItem): VdomTag = {
    val indentation = NonEmptyVector.option(item.indent)
    var code = PlainText.reqCode(item.suffix)
    if (indentation.isDefined)
      code = G.reqCode.nodeSeparator ~ code
    <.div(
      indentation.whenDefined(reqCodeTreeIdentation),
      <.pre(*.reqCodeTreeCode, code))
  }

  override def reqTypeShort(id: ReqTypeId): VdomTag =
    _reqTypeShort(id)

  val _reqTypeShort: ReqTypeId => VdomTag =
    Memo { id =>
      renderReqType(id) { rt =>
        val title = rt.description match {
          case Some(desc) => s"${rt.name}\n\n$desc"
          case None       => rt.name
        }
        <.span(
          *.reqTypeShort(rt.live),
          ^.title := title,
          rt.mnemonic.value)
      }
    }

  override def reqTypeFull(id: ReqTypeId): VdomTag =
    _reqTypeFull(id)

  val _reqTypeFull: ReqTypeId => VdomTag =
    Memo { id =>
      renderReqType(id) { rt =>
        <.span(
          PlainText.reqTypeFull(rt),
          rt.description.whenDefined(desc => TagMod(^.title := desc, ^.cursor.help)))
      }
    }

  private def renderReqType(id: ReqTypeId)(f: ReqType => VdomTag): VdomTag =
    project.config.reqTypes.get(id) match {
      case Some(rt) => f(rt)
      case None     => <.span("?")
    }

  private def tagColour(o: Option[Colour], live: Live): TagMod =
    // Do nothing if Dead because of the *.tag style and tagLabelColour which results in .ui.label.grey for dead tags
    TagMod.when(live is Live) {
      val c = o.getOrElse(Colour.tagDefault)
      TagMod(
        ^.backgroundColor := c.value,
        ^.borderColor     := c.value,
        ^.color           := c.foreground.value,
      )
    }

  private val tagPlain: Validity => ApplicableTagId => VdomTag =
    Validity.memo { validity =>
      Memo { id =>
        val tag = project.config.tags.needApplicableTag(id)
        tagWithoutStyle(Plain, tag, includeDesc = true)(
          *.tag(((tag.live, validity), true)),
          tagColour(tag.colour, tag.live))
      }
    }

  override def useCaseStepTextAndFlow(step: UseCaseStepFlowText.TextAndFlow[AnyOptional, Set[UseCaseStepId]], live: Live): VdomTag =
    makeUseCaseStepTextAndFlow(step, live)(useCaseFlowElementsById(_).iterator())

  override def withCtx[Ctx2 <: ProjectText.Context](newCtx: Ctx2): ProjectWidgets[Ctx2] =
    if (newCtx ==* ctx)
      this.asInstanceOf[ProjectWidgets[Ctx2]]
    else
      ProjectWidgets(project, plainText withCtx newCtx, reqDetailRC, webWorker)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Public additions not part of ProjectText

  /** Simple here means:
    *
    * - title optional
    * - no cursor:help style if title
    * - no concept of Validity (Live vs Dead is still respected)
    * - no contextualisation
    */
  def tagSimple(id: ApplicableTagId, includeDesc: Boolean): VdomTag = {
    val tag = project.config.tags.needApplicableTag(id)
    tagSimple(tag, includeDesc)
  }

  def tagSimple(tag: ApplicableTag, includeDesc: Boolean): VdomTag =
    tagWithoutStyle(Plain, tag, includeDesc = includeDesc)(
      *.tag(((tag.live, Valid), false)),
      tagColour(tag.colour, tag.live))

  def useCaseStepTextAndMaybeInvalidFlow[C[x] <: Iterable[x]](s: UseCaseStepFlowText.TextAndFlow[AnyOptional, C[String \/ UseCaseStepId]],
                                                              l: Live): VdomTag = {
    /** eg. "1.p" instead of "1.0" */
    def erroneousUseCaseStepRef(s: String): VdomTag =
      <.span(*.erroneousUseCaseStepRef, s)

    makeUseCaseStepTextAndFlow(s, l)(
      _.map(_.fold(erroneousUseCaseStepRef, useCaseFlowElementById)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class PubidFormat private[PubidFormat](contextualise: Contextualise,
                                               styleFn      : Live => TagMod,
                                               titleFn      : PubidFormat.TitleFn,
                                               liveFn       : PubidFormat.LiveFn) {

    private val label: ExternalPubid => String = {
      val txt = PlainText.pubid(_: ExternalPubid)
      contextualise match {
        case Contextualise => G.reflinkSurround compose txt
        case Plain         => txt
      }
    }

    private val styleMemo = Live.memo(styleFn)

    type Out = VdomTag

    private val memo: ReqId => Out =
      Memo { reqId =>
        val req  = project.content.reqs.need(reqId)
        val ep   = req.pubid.external(project)
        val txt  = label(ep)
        val live = liveFn(req)

        linkOrSpan(req, ep)(
          styleMemo(live),
          ^.title :=? titleFn(req),
          txt)
      }

    def apply(id: ReqId): Out =
      memo(id)

    def apply(req: Req): Out =
      memo(req.id)

    def apply(pubid: Pubid): Out =
      apply(project.content.reqs.reqIdByPubid(pubid))

    def withSuffix(id: ReqId, titleAttr: Boolean)(suffix: VdomNode*): Out = {
      val req   = project.content.reqs.need(id)
      val ep    = req.pubid.external(project)
      val pubid = PlainText.pubid(ep)
      val live  = liveFn(req)

      var label = TagMod(pubid, TagMod(suffix: _*))
      contextualise match {
        case Contextualise => label = TagMod(G.reflinkSurround.prefix, label, G.reflinkSurround.suffix)
        case Plain         => ()
      }

      linkOrSpan(req, ep)(
        styleMemo(live),
        TagMod.when(titleAttr)(^.title :=? titleFn(req)),
        label)
    }

    private val sep: TagMod =
      contextualise match {
        case Contextualise => sepSpace
        case Plain         => sepComma
      }

    def pubids(v: Vector[Pubid]): VdomTag =
      renderVector(v, sep)(apply)

    def reqs(v: Vector[Req]): VdomTag =
      renderVector(v, sep)(apply)
  }

  object PubidFormat {
    type LiveFn  = Req => Live
    type TitleFn = Req => Option[String]

    private val defaultLiveFn: LiveFn =
      _.live(project.config.reqTypes)

    private val defaultTitleFn: TitleFn =
      r => Some(UiText.hoverText(plainText.reqTitle(r)))

    def apply(ctx    : Contextualise,
              styleFn: Live => TagMod,
              titleFn: TitleFn = defaultTitleFn,
              liveFn : LiveFn  = defaultLiveFn) =
      new PubidFormat(ctx, styleFn, titleFn, liveFn)

    private def reqRefFormat(ctx: Contextualise, validityWhenDead: Validity) = {
      val f = deadValidity(validityWhenDead)
      apply(ctx, l => *.reqRef(f(l)))
    }

    val validWhenDead: PubidFormat =
      reqRefFormat(Plain, Valid)

    val invalidWhenDeadWithCtx: PubidFormat =
      reqRefFormat(Contextualise, Invalid)
  }

}
