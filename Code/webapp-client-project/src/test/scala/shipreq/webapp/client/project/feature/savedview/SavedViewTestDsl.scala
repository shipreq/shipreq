package shipreq.webapp.client.project.feature.savedview

import japgolly.scalajs.react.test.SimEvent
import shipreq.webapp.base.data.savedview.SavedView
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.test.TestPromptJs

final case class SavedViewTestDsl[R, O, S](* : Dsl[Id, R, O, S, String])
                                          (getFilterEditorObs: O => FilterEditorObs,
                                           getSavedViewManagerObs: O => SavedViewManagerObs,
                                           getPromptJs: R => TestPromptJs) {
  private implicit class RefExt(r: R) {
    def promptJs = getPromptJs(r)
  }

  private implicit class ObsExt(o: O) {
    def filter = getFilterEditorObs(o)
    def savedViews = getSavedViewManagerObs(o)
  }

  val filterText = *.focus("Filter text").value(_.obs.filter.value)

  def enterFilter(f: String) = {
    val e = SimEvent.Change(f)
    *.action(s"enterFilter('$f')")(e simulate _.obs.filter.input)
      .addCheck(filterText.assert(f).after)
  }

  val savedViews = *.focus("Saved views").collection(_.obs.savedViews.views.map(_.name_++))

  val activeSavedView = *.focus("Active saved views").value(_.obs.savedViews.activeView.name)

  def selectView(name: String) =
    *.action(s"Select view: $name")(_.obs.savedViews.needView(name).select())

  def setDefaultView(name: String) =
    *.action(s"Set default view: $name")(_.obs.savedViews.needView(name).setAsDefault())

  def saveAndReplaceView(name: String) =
    *.action(s"Save and replace view: $name")(_.obs.savedViews.needView(SavedView.Name.unsaved.value).replace(name))

  def saveCurrentView(name: String) =
    *.action(s"Save current view as: $name") { x =>
      x.ref.promptJs.setNextResponse(name)
      x.obs.savedViews.activeView.saveAsNew()
    }

}
