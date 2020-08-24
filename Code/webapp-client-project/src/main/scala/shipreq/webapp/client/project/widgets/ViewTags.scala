package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.mutable
import scala.scalajs.js.|
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation.VirtualProjectTags
import shipreq.webapp.base.lib.ClientUtil
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.client.project.app.Style.{widgets => *}

final class ViewTags(vtags: VirtualProjectTags, tagConfig: Tags) {
  import ViewTags._

  locally(vtags) /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  type Out = VdomTag

  def apply(id: ApplicableTagId): Dsl =
    new Dsl(id, null)

  def apply(tag: ApplicableTag): Dsl =
    new Dsl(tag.id, tag)

  def apply(tag: TagOrId): Dsl =
    if ((tag: Any).isInstanceOf[ApplicableTagId])
      apply(tag.asInstanceOf[ApplicableTagId])
    else
      apply(tag.asInstanceOf[ApplicableTag])

  final class Dsl(id: ApplicableTagId, private var tagOrNull: ApplicableTag) {

    private var ts: TagSettings = TagSettings.default

    def withCustomName(name: String): Dsl = {
      ts = ts.copy(customName = Some(name))
      this
    }

    def withValidity(validity: Validity): Dsl = {
      ts = ts.copy(validity = validity)
      this
    }

    private def tag(): ApplicableTag = {
      if (tagOrNull eq null)
        tagOrNull = tagConfig.needApplicableTag(id)
      tagOrNull
    }

    def render(implicit ds: DisplaySettings): Out = {
      val useCache = (
        id.value >= 0 // TagConfig creates a fake ApplicableTag with id of -1 to render new tags before they're saved
        && ts.customName.isEmpty // Custom names are not cached
      )

      if (useCache) {
        val tag: TagOrId = if (tagOrNull ne null) tagOrNull else id
        cache(ds)(ts.validity)(tag)
      } else
        _render(tag(), ds, ts)
    }
  }

  private val cache: DisplaySettings => Validity => TagOrId => Out =
    Util.memoWithMapVar { ds =>
      Validity.memo { validity =>
        val ts = TagSettings(None, validity)
        val cache = new mutable.HashMap[ApplicableTagId, Out]()
        tagOrId =>
          (tagOrId: Any) match {
            case id: ApplicableTagId =>
              cache.getOrElseUpdate(id, {
                val tag = tagConfig.needApplicableTag(id)
                _render(tag, ds, ts)
              })
            case _ =>
              val tag = tagOrId.asInstanceOf[ApplicableTag]
              cache.getOrElseUpdate(tag.id, _render(tag, ds, ts))
          }
      }
    }

  private def _render(tag: ApplicableTag, ds: DisplaySettings, ts: TagSettings): Out = {
    val name            = ts.customName.getOrElse(tag.name)
    val live            = tag.live
    val tagInText       = ds.contextualise ==* Contextualise
    val helpIconOnHover = ds.hoverText ==* HoverText.ShowWithHelpCursor
    val styleArgs       = ((live, ts.validity), helpIconOnHover)


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
      TagMod.when(hoverText.nonEmpty)(^.title := hoverText)

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

  def vectorById(ids: Vector[ApplicableTagId],
                 separator: TagMod,
                 validity: ApplicableTagId => Validity)
                (implicit ds: DisplaySettings): VdomTag = {
    val c = cache(ds)
    ClientUtil.renderVector(ids, separator) { id =>
      val v = validity(id)
      c(v)(id)
    }
  }
}

object ViewTags {

  def apply(vtags: VirtualProjectTags, tagConfig: Tags): ViewTags =
    new ViewTags(vtags, tagConfig)

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

    val inText              = apply(HoverText.ShowWithHelpCursor, Contextualise)
    val plainWithHelpCursor = apply(HoverText.ShowWithHelpCursor, Plain)
    val plain               = apply(HoverText.Show, Plain)
    val plainNoHover        = apply(HoverText.Omit, Plain)
  }

  sealed trait HoverText

  object HoverText {
    case object Omit               extends HoverText
    case object Show               extends HoverText
    case object ShowWithHelpCursor extends HoverText

    implicit def univEq: UnivEq[HoverText] = UnivEq.derive
  }

  final case class TagSettings(customName: Option[String],
                               validity  : Validity)

  object TagSettings {
    val default = apply(None, Valid)
  }

  type TagOrId = ApplicableTag | ApplicableTagId

}
