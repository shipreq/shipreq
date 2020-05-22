package shipreq.webapp.client.project.feature

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.VdomElement
import scala.annotation.nowarn
import shipreq.webapp.base.data.{FilterDead, Project}
import shipreq.webapp.base.lib.DataReusability._

/** Usage
  * =====
  *
  * Super easy.
  *
  * 1. (Optional) Request an instance of [[SavedViewFeature.Static]] in your static props if you want to chain a bunch
  *    of `Px` instances from the saved view `Px`s.
  *
  * 2. Request an instance of [[SavedViewFeature]] in your props.
  *
  * 3. Call [[SavedViewFeature.renderSavedViewManager()]] and/or [[SavedViewFeature.renderFilterEditor()]] for managed
  * component instances.
  */
object SavedViewFeature {

  val ColumnLogic = savedview.ColumnLogic

  type ColumnPlus = shipreq.webapp.client.project.feature.savedview.ColumnPlus
  val  ColumnPlus = shipreq.webapp.client.project.feature.savedview.ColumnPlus

  type State = savedview.State
  val  State = savedview.State

  type Static = savedview.Static
  val  Static = savedview.Static

  implicit val reusability: Reusability[SavedViewFeature] = {
    // Static contains Pxs which are NOT reusable (because you can never access the previous value) however here
    // they can be ignored because Project and FilterDead have been added to SavedViewFeature.
    @nowarn("cat=unused") implicit val s: Reusability[Static] = Reusability.byRef
    Reusability.byRef || Reusability.derive
  }
}

// Note: It looks like Project and FilterDead are unused by they're used under-the-hood via Static and need to be here
// to ensure correct Reusability.
final case class SavedViewFeature(static    : SavedViewFeature.Static,
                                  state     : SavedViewFeature.State,
                                  project   : Project,
                                  filterDead: FilterDead) {

  def renderSavedViewManager(): VdomElement =
    static.renderSavedViewManager(state.async)

  def renderFilterEditor(): VdomElement =
    static.renderFilterEditor(state)
}