package shipreq.webapp.member.feature.editcontrols

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react._
import shipreq.webapp.member.feature.PreviewFeature
import shipreq.webapp.member.ui._

sealed trait Font

object Font {
  case object Default extends Font
  case object Monospace extends Font

  implicit def univEq: UnivEq[Font] = UnivEq.derive
  implicit val reusability: Reusability[Font] = Reusability.derive
  val values = AdtMacros.adtValues[Font]
}

// =====================================================================================================================

sealed trait Mode

object Mode {
  case object Inline extends Mode
  case object Fullscreen extends Mode

  def derive(fullscreen: Option[OptionalFullscreen.Ctx]): Mode =
    if (fullscreen.exists(_.currentlyFullscreen))
      Mode.Fullscreen
    else
      Mode.Inline

  implicit def univEq: UnivEq[Mode] = UnivEq.derive
  implicit def reusability: Reusability[Mode] = Reusability.by_==
  def values = AdtMacros.adtValues[Mode]
}

// =====================================================================================================================

sealed trait OpenPreview

object OpenPreview {

  /** Follows the logic described in [[PreviewFeature]] to only show preview when required, and with minimal change */
  case object Minimally extends OpenPreview

  /** Same as [[Minimally]] except that it can be manually toggled on/off. */
  case object MinimallyWithControls extends OpenPreview

  /** Preview always shown. */
  case object Always extends OpenPreview

  /** Preview never shown. */
  case object Never extends OpenPreview

  /** Preview shown anytime `wantOpen` is `true`. */
  case object WhenWanted extends OpenPreview

  /** Preview shown by default, and can be manually toggled on/off. */
  case object ShowWithControls extends OpenPreview

  implicit def univEq: UnivEq[OpenPreview] = UnivEq.derive
  implicit def reusability: Reusability[OpenPreview] = Reusability.by_==
}

// =====================================================================================================================

sealed trait WhenInTransit

object WhenInTransit {
  case object ReadOnlyViewWithSpinner extends WhenInTransit
  case object DisableEditor           extends WhenInTransit

  implicit def univEq: UnivEq[WhenInTransit] = UnivEq.derive
  implicit def reusability: Reusability[WhenInTransit] = Reusability.by_==
}

// =====================================================================================================================

final case class Style(position     : PreviewFeature.Position,
                       openPreview  : OpenPreview,
                       whenInTransit: WhenInTransit)

object Style {

  lazy val default = Style(
    PreviewFeature.Position.Under,
    OpenPreview.Minimally,
    WhenInTransit.ReadOnlyViewWithSpinner,
  )

  implicit def univEq: UnivEq[Style] = UnivEq.derive
  implicit def reusability: Reusability[Style] = Reusability.derive
}
