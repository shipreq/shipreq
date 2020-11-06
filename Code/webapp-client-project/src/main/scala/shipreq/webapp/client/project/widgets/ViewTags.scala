package shipreq.webapp.client.project.widgets

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.Reusable
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.mutable
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.ui.semantic.{Icon, Popup}
import shipreq.webapp.base.util.ClientUtil
import shipreq.webapp.client.project.app.Style.{tags => *}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.derivation.VirtualProjectTags.{DerivationDesc, VirtualTag}
import shipreq.webapp.member.project.text.Grammar

final class ViewTags(project: Project) {
  import ViewTags._

  private[this] val vtags     = project.virtualTags
  private[this] val tagConfig = project.config.tags

  type Out = VdomTag

  def apply(t: ApplicableTag.OrId): Dsl =
    new Dsl(t)

  final class Dsl(t: ApplicableTag.OrId) {

    private var ts: TagSettings = TagSettings.default

    def withCustomName(name: String): Dsl = {
      ts = ts.copy(customName = Some(name))
      this
    }

    def withValidity(validity: Validity): Dsl = {
      ts = ts.copy(validity = validity)
      this
    }

    def render(implicit ds: DisplaySettings): Out =
      ViewTags.this.render(t, ts)
  }

  def render(t: ApplicableTag.OrId, ts: TagSettings = TagSettings.default)(implicit ds: DisplaySettings): Out = {
    val id = t.id

    val useCache = (
      id.value >= 0 // TagConfig creates a fake ApplicableTag with id of -1 to render new tags before they're saved
      && ts.customName.isEmpty // Custom names are not cached
      && ts.customColour.isEmpty // Custom colours are not cached
    )

    if (useCache)
      cache(ds)(ts.validity)(t)
    else
      _render(t.tag(tagConfig), ds, ts)
  }

  private def hoverText(tag: ApplicableTag, ds: DisplaySettings, ts: TagSettings): String = {
    val name = ts.customName.getOrElse(tag.name)

    if (ds.hoverText == HoverText.Omit)
      ""
    else {
      var txt =
        if (name.compareToIgnoreCase(tag.key.value) == 0)
          ""
        else
          Grammar.hashRefKey.prefix + tag.key.value
      for (d <- tag.desc) {
        if (txt.nonEmpty)
          txt += "\n\n"
        txt += d
      }
      txt
    }
  }

  private def hoverTextVdom(tag: ApplicableTag, ds: DisplaySettings, ts: TagSettings): TagMod = {
    val hoverText = this.hoverText(tag, ds, ts)
    ^.title := hoverText // yes, even when hoverText is empty so that it doesn't show "double-click to edit"
  }

  private def _render(tag: ApplicableTag, ds: DisplaySettings, ts: TagSettings): Out = {
    val name          = ts.customName.getOrElse(tag.name)
    val live          = tag.live
    val tagInText     = ds.contextualise ==* Contextualise
    val styleArgs     = (live, ts.validity)
    val hoverTextVdom = this.hoverTextVdom(tag, ds, ts)

    if (tagInText) {
      // ---------------------------------------------------------------------------------------------------------------
      // Render as inline text

      <.span(
        *.tagInText(styleArgs),
        hoverTextVdom,
        Grammar.hashRefKey.prefix, name)

    } else {
      // ---------------------------------------------------------------------------------------------------------------
      // Render a little pill

      val colour: TagMod =
        // Do nothing if Dead because of the *.tag style and tagLabelColour which results in .ui.label.grey for dead tags
        TagMod.when(live is Live) {
          val c = ts.customColour.orElse(tag.colour).getOrElse(Colour.tagDefault)
          TagMod(
            ^.backgroundColor := c.value,
            ^.borderColor     := c.value,
            ^.color           := c.foreground.value,
          )
        }

      <.span(
        *.tag(styleArgs),
        colour,
        hoverTextVdom,
        name)
    }
  }

  private def _renderUnfocused(tag: ApplicableTag, ts: TagSettings): Out = {
    implicit def ds   = DisplaySettings.tag
    val name          = tag.name
    val live          = tag.live
    val styleArgs     = (live, ts.validity)
    val hoverTextVdom = this.hoverTextVdom(tag, ds, ts)

    <.span(
      *.unfocusedTag(styleArgs),
      hoverTextVdom,
      name)
  }

  private def decorateTag(vtag: VirtualTag, base: Out, foregroundIsBlack: Boolean): Out = {
    @inline def ds = DisplaySettings.tag
    @inline def ts = TagSettings.default

    val decorations =
      TagMod(
        TagMod.when(vtag.isManualInText)(tagIsFromText(foregroundIsBlack)),
        TagMod.when(vtag.isDefault)(tagIconDefault(foregroundIsBlack)),
        TagMod.when(vtag.isDerived)(tagIconDerived(foregroundIsBlack)),
        tagIconDead.when(vtag.isDead),
      )

    val decorated =
      base(decorations)

    if (vtag.isDerived) {
      val tag       = tagConfig.needApplicableTag(vtag.id)
      val hoverText = this.hoverText(tag, ds, ts)

      if (hoverText.isEmpty) {
        // No hover text - attach popup to entire tag
        <.span(
          Popup.Js.Props(
            options = popupOptions,
            base    = <.span,
            display = decorated,
            popup   = vtag.derivationDesc.whenDefined(renderDerivationDesc),
          ).render)
      } else
        // There's hover text - attach popup to the derivation icon
        base(
          Popup.Js.Props(
            options = popupOptions,
            base    = <.span,
            display = <.span(decorations),
            popup   = vtag.derivationDesc.whenDefined(renderDerivationDesc),
          ).render)

    } else if (vtag.isDefault)
      decorated(tagTitleDefault)
    else if (vtag.isManualInText)
      decorated(tagTitleFromText)
    else
      decorated
  }

  private def memoApplicableTagOrId[A](f: ApplicableTag => A): ApplicableTag.OrId => A = {
    val cache = new mutable.HashMap[ApplicableTagId, A]()
    t =>
      if (t.isId) {
        val id = t.id
        cache.getOrElseUpdate(id, {
          val tag = tagConfig.needApplicableTag(id)
          f(tag)
        })
      } else {
        val tag = t.unsafeAsTag
        cache.getOrElseUpdate(tag.id, f(tag))
      }
  }

  private[this] val cache: DisplaySettings => Validity => ApplicableTag.OrId => Out =
    Util.memoWithMapVar { ds =>
      Validity.memo { validity =>
        val ts = TagSettings(None, None, validity)
        memoApplicableTagOrId(_render(_, ds, ts))
      }
    }

  private val basic: ApplicableTagId => VdomNode =
    cache(DisplaySettings.tag)(Valid)(_)

  private val basicUnfocused: Validity => ApplicableTag.OrId => Out = {
    Validity.memo { v =>
      val ts = TagSettings.default.copy(validity = v)
      memoApplicableTagOrId(_renderUnfocused(_, ts))
    }
  }

  /** Basic here means the tag has context-specific decorations. So...
    * - no provenance icon
    * - no derivation explanation
    */
  def basicVectorById(ids: Vector[ApplicableTagId], validity: ApplicableTagId => Validity = Valid.always)
                     (implicit ds: DisplaySettings): VdomTag = {
    val c = cache(ds)
    <.div(*.tagList, ids.toTagMod { id =>
      val v = validity(id)
      c(v)(id)
    })
  }

  val forReq: FilterDead => ReqId => ForReq[Out] =
    FilterDead.memo { fd =>
      Memo { reqId =>
        _forReq(reqId, fd)
      }
    }

  private def _forReq(reqId: ReqId, fd: FilterDead): ForReq[Out] = {
    implicit def ds = DisplaySettings.tag
    val basicCache  = cache(ds)
    val vtags       = this.vtags(reqId, fd)

    def renderFocused(t: ApplicableTagId, f: TagFieldId): Out = {
      val vtag = vtags(t, f)

      lazy val foregroundIsBlack: Boolean =
        if (vtag.isDead)
          false // foreground is always white for dead tags
        else {
          val tag = tagConfig.needApplicableTag(t)
          val colour = tag.colour.getOrElse(Colour.tagDefault)
          colour.foreground eq Colour.black
        }

      decorateTag(vtag, basicCache(vtag.validity)(t), foregroundIsBlack)
    }

    def renderUnfocused(t: ApplicableTagId, f: TagFieldId): Out = {
      val vtag = vtags(t, f)
      decorateTag(vtag, basicUnfocused(vtag.validity)(t), foregroundIsBlack = true)
    }

    val focusedMemo: TagFieldId => ApplicableTagId => Out =
      Memo(f => Memo(renderFocused(_, f)))

    val unfocusedMemo: TagFieldId => ApplicableTagId => Out =
      Memo(f => Memo(renderUnfocused(_, f)))

    new ForReq[Out] {
      override def apply(f: TagFieldId): ApplicableTagId => Out =
        focusedMemo(f)

      override def unfocused(f: TagFieldId): ApplicableTagId => Out =
        unfocusedMemo(f)

      override def vector(f        : TagFieldId,
                          focused  : TagFieldId => Vector[ApplicableTagId],
                          unfocused: TagFieldId => Vector[ApplicableTagId]) =
        <.div(*.tagList,
          focused(f).toTagMod(focusedMemo(f)),
          unfocused(f).toTagMod(unfocusedMemo(f)))
    }
  }

  private def renderDerivationDesc(d: DerivationDesc): VdomNode = {

    val renderFactor: DerivationDesc.Factor => VdomNode = f =>
      <.tr(
        factorKey(f.tag.fold("No tag": VdomNode)(basic)),
        factorValues(f.reqs))

    val stepIndices = d.steps.indices

    val renderStep: Int => TagMod = i => {
      val sep = if (i == stepIndices.last) derivSpace else derivPlus
      val step = d.steps(i)
      <.li(derivEquals, ClientUtil.renderVector(step.tags.whole, sep)(basic))
    }

    <.div(
      headingFactors,
      <.table(*.derivDescFactors,
        <.tbody(
          d.factors.toTagMod(renderFactor))),
      TagMod.when(stepIndices.nonEmpty)(TagMod(
        headingDeriv,
        <.ol(*.derivDescDerivationSteps,
          stepIndices.toTagMod(renderStep)))))
  }

  val forPlainTextViewReqCache: Reusable[FilterDead => ReqId => ViewTags.ForReq[String]] =
    Reusable.byRef(this).withValue(_ => _ => plainTextForReq)

  val plainTextForReq: ForReq[String] = {
    val renderToString: ApplicableTagId => String =
      project.config.tags.needApplicableTag(_).name

    new ForReq[String] {
      override def apply(f: TagFieldId) =
        renderToString

      override def unfocused(f: TagFieldId) =
        renderToString

      override def vector(f        : TagFieldId,
                          focused  : TagFieldId => Vector[ApplicableTagId],
                          unfocused: TagFieldId => Vector[ApplicableTagId]) =
      (focused(f).iterator ++ unfocused(f).iterator).map(renderToString).mkString(" ")
    }
  }
}

object ViewTags {

  def apply(project: Project): ViewTags =
    new ViewTags(project)

  final case class DisplaySettings private[DisplaySettings](hoverText    : HoverText,
                                                            contextualise: Contextualise) {
    override val hashCode =
      hoverText.hashCode * 31 + contextualise.hashCode

    override def equals(o: Any): Boolean =
      o.isInstanceOf[AnyRef] && {
        val a = o.asInstanceOf[AnyRef]
        (a eq this) || (a.hashCode == hashCode)
      }
  }

  object DisplaySettings {
    implicit def univEq: UnivEq[DisplaySettings] = UnivEq.derive

    val inText    = apply(HoverText.Show, Contextualise)
    val tag       = apply(HoverText.Show, Plain)
    val tagNoDesc = apply(HoverText.Omit, Plain)
  }

  sealed trait HoverText

  object HoverText {
    case object Omit extends HoverText
    case object Show extends HoverText

    implicit def univEq: UnivEq[HoverText] = UnivEq.derive
  }

  final case class TagSettings(customName  : Option[String],
                               customColour: Option[Colour],
                               validity    : Validity)

  object TagSettings {
    val default = apply(None, None, Valid)
  }

  trait ForReq[A] {
    def apply(f: TagFieldId): ApplicableTagId => A

    def unfocused(f: TagFieldId): ApplicableTagId => A

    def vector(f        : TagFieldId,
               focused  : TagFieldId => Vector[ApplicableTagId],
               unfocused: TagFieldId => Vector[ApplicableTagId]): A
  }

  private val tagIconDead      = Icon.Trash.tag(*.iconDead)
  private val tagIconDefault   = Memo.bool(b => Icon.Sliders.tag(*.iconDefault(b)))
  private val tagIconDerived   = Memo.bool(b => Icon.Sitemap.tag(*.iconDerived(b)))
  private val tagIsFromText    = Memo.bool(b => <.span(*.iconText(b), "#"))
  private val tagTitleDefault  = ^.title := "Tag is here because it's a tag field default"
  private val tagTitleFromText = ^.title := "Tag is specified in this requirement's text"
  private val headingFactors   = <.h4(*.derivDescHeading, "Derivation Factors")
  private val headingDeriv     = <.h4(*.derivDescHeading, "Derivation")
  private val factorKey        = <.td(*.derivDescFactorKey)
  private val factorValues     = <.td(*.derivDescFactorValues, <.span(*.derivDescFactorDash, "—"))
  private val derivEquals      = <.span(*.derivDescDerivationStepEquals, "=")
  private val derivPlus        = <.span(*.derivDescDerivationStepPlus, "+"): TagMod
  private val derivSpace       = " ": TagMod

  private val popupOptions =
    new Popup.Js.Options {
      override val inline = true
      override val hoverable = true
      override val prefer = "opposite"
      override val position = Popup.Position.TopLeft.value
      override val delay = new Popup.Js.Options.Delay {
        override val show = 200
        override val hide = 300
      }
    }
}
