package shipreq.webapp.base.feature.editcontrols

import shipreq.webapp.base.feature.PreviewFeature
import shipreq.webapp.base.ui._

final case class Layout(mode    : Mode,
                        preview : Option[Layout.Preview],
                        controls: Option[Layout.Controls]) {

  def position: Option[PreviewFeature.Position] =
    preview.map(_.position)

  def positionIfShown: Option[PreviewFeature.Position] =
    preview.filter(_.isShown).map(_.position)
}

object Layout {
  final case class Preview(position   : PreviewFeature.Position,
                           isShown    : Boolean,
                           collapsible: Boolean) {
    @elidable(elidable.INFO)
    override def toString = s"Preview($position, isShown = $isShown, collapsible = $collapsible)"
  }

  final case class Controls(around               : Controls.Around,
                            showControlsInitially: Boolean)
  object Controls {

    sealed trait Around
    object Around {
      case object Editor  extends Around
      case object Preview extends Around
    }
  }

  // ===================================================================================================================

  def determine(style          : Style,
                previewRW      : PreviewFeature.ReadWrite.Single,
                previewWantOpen: => Boolean,
                fullscreen     : Option[OptionalFullscreen.Ctx]): Layout = {

    import Controls.Around

    val mode = Mode.derive(fullscreen)

    mode match {
      case Mode.Inline =>
        style.openPreview match {

          case OpenPreview.Minimally =>
            val show    = previewRW.read.showPreview(previewWantOpen)
            val preview = Preview(position = style.position, isShown = show, collapsible = true)
            Layout(mode, Some(preview), None)

          case OpenPreview.MinimallyWithControls =>
            val show        = previewRW.read.showPreview(previewWantOpen)
            val position    = previewRW.read.position(style.position)
            val around      = if (show) Around.Preview else Around.Editor
            val controls    = Some(Controls(around, showControlsInitially = false))
            val preview     =
              if (previewRW.read.isManual)
                Option.when(show)(Preview(position, isShown = show, collapsible = false))
              else
                Some(Preview(position, isShown = show, collapsible = true))
            Layout(mode, preview, controls)

          case OpenPreview.ShowWithControls =>
            val show     = previewRW.read.showManuallyControlledPreview(true)
            val position = previewRW.read.position(style.position)
            val preview  = Option.when(show)(Preview(position, isShown = show, collapsible = false))
            val around   = if (show) Around.Preview else Around.Editor
            val controls = Some(Controls(around, showControlsInitially = true))
            Layout(mode, preview, controls)

          case OpenPreview.WhenWanted =>
            val show =
              previewRW.read.status match {
                case Some(_) => previewRW.read.showPreview(previewWantOpen) // using state to avoid jitter while type
                case None    => previewWantOpen
              }
            val preview = Preview(style.position, isShown = show, collapsible = true)
            Layout(mode, Some(preview), None)

          case OpenPreview.Always =>
            val preview = Preview(style.position, isShown = true, collapsible = false)
            Layout(mode, Some(preview), None)

          case OpenPreview.Never =>
            Layout(mode, None, None)
        }

      case Mode.Fullscreen =>
        val defaultShow: Boolean =
          style.openPreview match {
            case OpenPreview.Minimally
               | OpenPreview.MinimallyWithControls
               | OpenPreview.WhenWanted
               | OpenPreview.ShowWithControls
               | OpenPreview.Always                => true
            case OpenPreview.Never                 => false
          }
        val show     = previewRW.read.showManuallyControlledPreview(defaultShow)
        val position = previewRW.read.position(style.position)
        val preview  = Option.when(show)(Preview(position, isShown = show, collapsible = false))
        val around   = if (show) Around.Preview else Around.Editor
        val controls = Some(Controls(around, showControlsInitially = false))
        Layout(mode, preview, controls)
    }
  }
}
