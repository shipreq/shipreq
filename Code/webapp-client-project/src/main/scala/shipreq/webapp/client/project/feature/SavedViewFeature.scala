package shipreq.webapp.client.project.feature

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.VdomElement
import shipreq.webapp.member.project.data.savedview.{SavedView, View}
import shipreq.webapp.member.project.data.{FilterDead, Project, ReqId}
import shipreq.webapp.member.project.util.DataReusability._

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
final case class SavedViewFeature(static            : SavedViewFeature.Static,
                                  state             : SavedViewFeature.State,
                                  project           : Project,
                                  explicitFilterDead: FilterDead) {

  // Here I'm using lazy vals EXPLICITLY BECAUSE the underlying state can change and these methods/vals are meant to
  // be stable themselves, and only used immediately during a render function.

  lazy val activeView: View =
    static.pxActiveView.value()

  lazy val savedViewId: Option[SavedView.Id] =
    static.pxSavedViewId.value()

  def filterDead: FilterDead =
    activeView.filterDead

  lazy val pxReqWhitelistIgnoringFilterDead: Option[Set[ReqId]] =
    static.pxReqWhitelistIgnoringFilterDead.value()

  lazy val reqWhitelist: Option[Set[ReqId]] =
    static.pxReqWhitelist.value()

  lazy val renderSavedViewManager: VdomElement =
    static.renderSavedViewManager(state.async)

  lazy val renderFilterEditor: VdomElement =
    static.renderFilterEditor(state)

  lazy val renderFilterDeadButton: VdomElement =
    static.renderFilterDeadButton(filterDead)

  lazy val renderSavedViewsAndFilterDeadButton: VdomElement =
    static.renderSavedViewsAndFilterDeadButton(state.async, filterDead)

  def renderSavedViewsAndFilterDeadButton(filterDeadButton: VdomElement): VdomElement =
    static.renderSavedViewsAndFilterDeadButton(state.async, filterDeadButton)
}