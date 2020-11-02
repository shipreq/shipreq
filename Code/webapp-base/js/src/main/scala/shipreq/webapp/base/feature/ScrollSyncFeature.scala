package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scala.scalajs.js.timers._
import shipreq.base.util.FreeOption

/** This feature allows you to keep the vertical scroll bars of multiple DOMs in sync.
  *
  * Usage
  * =====
  *
  * 1. Create a new instance of [[ScrollSyncFeature]] and store in the parent component's backend.
  *
  * 2. Call one of the `.newPane` methods once for each DOM and store the pane in the backend.
  *
  * 3. For each DOM, use either [[ScrollSyncFeature.PaneWithRef.install()]] or
  *    [[ScrollSyncFeature.PaneWithManualDom.tagMod]] to associate the pane with its vdom.
  *
  * 4. Optionally, call [[ScrollSyncFeature.Pane.syncOthersToThis]] in the editor DOM's `onChange` event.
  */
object ScrollSyncFeature {

  def apply(): ScrollSyncFeature =
    new ScrollSyncFeature

  sealed trait Pane[N <: html.Element] {
    def dom: CallbackTo[Option[N]]
    def syncOthersToThis: Callback
  }

  trait PaneWithManualDom[N <: html.Element] extends Pane[N] {
    def tagMod: TagMod
  }

  trait PaneWithRef[N <: html.Element] extends Pane[N] {

    final val ref: Ref.Simple[N] =
      Ref[N]

    override val dom: CallbackTo[Option[N]] =
      ref.get.asCallback

    def install(v: VdomTagOf[N]): VdomTagOf[N]
  }
}

// =====================================================================================================================

final class ScrollSyncFeature {
  import ScrollSyncFeature._

  private var panes     = Vector.empty[Pane[_ <: html.Element]]
  private var scrolling = -1
  private var timeout   = FreeOption.empty[SetTimeoutHandle]

  // -------------------------------------------------------------------------------------------------------------------

  def newPaneWithRefI() =
    newPaneWithRef[html.Input]()

  def newPaneWithRefTA() =
    newPaneWithRef[html.TextArea]()

  def newPaneWithRef[N <: html.Element](): PaneWithRef[N] = {
    val id = panes.length

    val p: PaneWithRef[N] =
      new PaneWithRef[N] {

        override val syncOthersToThis: Callback =
          sync(id)

        private val tagMod: TagMod =
          ^.onScroll --> syncOthersToThis

        override def install(v: VdomTagOf[N]): VdomTagOf[N] =
          v(tagMod).withRef(ref)
      }

    panes :+= p
    p
  }

  // -------------------------------------------------------------------------------------------------------------------

  def newPane(): PaneWithManualDom[html.Element] = {
    val ref = Ref.toVdom[html.Element]
    newPane(ref.get.asCallback, ^.untypedRef := ref)
  }

  // -------------------------------------------------------------------------------------------------------------------

  def newPane[N <: html.Element](_dom: CallbackTo[Option[N]]): PaneWithManualDom[N] =
    newPane(_dom, EmptyVdom)

  def newPane[N <: html.Element](_dom: CallbackTo[Option[N]], extraTagMod: TagMod): PaneWithManualDom[N] = {
    val id = panes.length

    val p: PaneWithManualDom[N] =
      new PaneWithManualDom[N] {

        override val dom =
          _dom

        override val syncOthersToThis: Callback =
          sync(id)

        override val tagMod: TagMod =
          TagMod(
            extraTagMod,
            ^.onScroll --> syncOthersToThis,
          )

      }

    panes :+= p
    p
  }

  // -------------------------------------------------------------------------------------------------------------------

  private def sync(sourceId: Int): Callback = {

    @inline def getY1Max(dom: html.Element): Double =
      (dom.scrollHeight - dom.clientHeight).toDouble

    @inline def scrolledPct(dom: html.Element): Option[Double] = {
      val y1Max = getY1Max(dom)
      Option.when(y1Max > 0) {
        val y1 = dom.scrollTop
        y1 / y1Max
      }
    }

    Callback {
      if (scrolling == -1 || scrolling == sourceId) {

        val src = panes(sourceId)

        for {
          srcDom <- src.dom.runNow()
          srcPct <- scrolledPct(srcDom)
        } {

          // Set state so that other panes don't try to re-sync their scroll changes back
          if (timeout.nonEmpty) {
            clearTimeout(timeout.getOrNull)
            timeout = FreeOption.empty
          }
          scrolling = sourceId

          // Sync other panes
          var madeChanges = false
          var id = 0
          while (id < panes.length) {
            if (id != sourceId) {
              val p = panes(id)

              // Sync pane
              for (dom <- p.dom.runNow()) {
                val y1Max = getY1Max(dom)
                if (y1Max > 0) {
                  val newY1 = y1Max * srcPct
                  dom.scrollTop = newY1
                  madeChanges = true
                }
              }
            }
            id += 1
          }

          // Clear state when scrolling and syncing is done
          if (madeChanges)
            timeout = FreeOption(setTimeout(300) {
              scrolling = -1
            })
          else
            scrolling = -1
        }
      }
    }
  }
}
