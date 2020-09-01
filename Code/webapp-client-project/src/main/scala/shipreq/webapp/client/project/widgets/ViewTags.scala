package shipreq.webapp.client.project.widgets

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.Reusable
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.mutable
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation.VirtualProjectTags.DerivationDesc
import shipreq.webapp.base.lib.ClientUtil
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.ui.semantic.{Icon, Popup}
import shipreq.webapp.client.project.app.Style.{tags => *}

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
    )

    if (useCache)
      cache(ds)(ts.validity)(t)
    else
      _render(t.tag(tagConfig), ds, ts)
  }

  private[this] val cache: DisplaySettings => Validity => ApplicableTag.OrId => Out =
    Util.memoWithMapVar { ds =>
      Validity.memo { validity =>
        val ts = TagSettings(None, validity)
        val cache = new mutable.HashMap[ApplicableTagId, Out]()
        t =>
          if (t.isId) {
            val id = t.id
            cache.getOrElseUpdate(id, {
              val tag = tagConfig.needApplicableTag(id)
              _render(tag, ds, ts)
            })
          } else {
            val tag = t.unsafeAsTag
            cache.getOrElseUpdate(tag.id, _render(tag, ds, ts))
          }
      }
    }

  private def _render(tag: ApplicableTag, ds: DisplaySettings, ts: TagSettings): Out = {
    val name      = ts.customName.getOrElse(tag.name)
    val live      = tag.live
    val tagInText = ds.contextualise ==* Contextualise
    val styleArgs = (live, ts.validity)


    val hoverText: String =
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

    val hoverTextVdom =
      ^.title := hoverText // yes, even when hoverText is empty so that it doesn't show "double-click to edit"

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
          val c = tag.colour.getOrElse(Colour.tagDefault)
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

  private val basic: ApplicableTagId => VdomNode =
    cache(DisplaySettings.tag)(Valid)(_)

  /** Basic here means the tag has context-specific decorations. So...
    * - no provenance icon
    * - no derivation explanation
    */
  def basicVectorById(ids: Vector[ApplicableTagId], validity: ApplicableTagId => Validity = Valid.always)
                     (implicit ds: DisplaySettings): VdomTag = {
    val c = cache(ds)
    <.div(*.tagList, ClientUtil.renderVector(ids, EmptyVdom) { id =>
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

    def render(t: ApplicableTagId, f: TagFieldId): Out = {
      val vtag = vtags(t, f)

      lazy val foregroundIsBlack: Boolean =
        if (vtag.isDead)
          false // foreground is always white for dead tags
        else {
          val tag = tagConfig.needApplicableTag(t)
          val colour = tag.colour.getOrElse(Colour.tagDefault)
          colour.foreground eq Colour.black
        }

      val base =
        basicCache(vtag.validity)(t)(
          TagMod.when(vtag.isManualInText)(tagIsFromText(foregroundIsBlack)),
          TagMod.when(vtag.isDefault)(tagIconDefault(foregroundIsBlack)),
          TagMod.when(vtag.isDerived)(tagIconDerived(foregroundIsBlack)),
          tagIconDead.when(vtag.isDead),
        )

      if (vtag.isDerived) {
        <.span(
          Popup.Js.Props(
            options = popupOptions,
            base    = <.span,
            display = base,
            popup   = vtag.derivationDesc.whenDefined(renderDerivationDesc),
          ).render)
      } else if (vtag.isDefault)
        base(tagTitleDefault)
      else if (vtag.isManualInText)
        base(tagTitleFromText)
      else
        base
    }

    new ForReq[Out] {
      override def apply(f: TagFieldId): ApplicableTagId => Out =
        f match {
          case TagFieldId.Custom(f) => render(_, f.asTagFieldId)
          case TagFieldId.All       => render(_, TagFieldId.All)
          case TagFieldId.Other     => render(_, TagFieldId.Other)
        }

      override def vector(ids: Vector[ApplicableTagId], render: ApplicableTagId => Out): Out =
        <.div(*.tagList, ClientUtil.renderVector(ids, EmptyVdom)(render))
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
    val renderOne: ApplicableTagId => String =
      project.config.tags.needApplicableTag(_).name

    new ForReq[String] {
      override def apply(f: TagFieldId) = renderOne
      override def vector(ids: Vector[ApplicableTagId], render: ApplicableTagId => String) =
        ids.iterator.map(render).mkString(" ")
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

  final case class TagSettings(customName: Option[String],
                               validity  : Validity)

  object TagSettings {
    val default = apply(None, Valid)
  }

  trait ForReq[A] {
    def apply(f: TagFieldId): ApplicableTagId => A
    def vector(ids: Vector[ApplicableTagId], render: ApplicableTagId => A): A
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
