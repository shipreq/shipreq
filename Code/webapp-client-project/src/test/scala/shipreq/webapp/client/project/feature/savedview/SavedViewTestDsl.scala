package shipreq.webapp.client.project.feature.savedview

import japgolly.scalajs.react.test.{SimEvent, Simulate}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.test.TestPromptJs
import shipreq.webapp.member.project.data.FilterDead
import shipreq.webapp.member.project.data.savedview.SavedView

final case class SavedViewTestDsl[R, O, S](* : Dsl[Id, R, O, S, String])
                                          (getSavedViewManagerObs: O => SavedViewManagerObs,
                                           getFilterDeadButtonObs: O => FilterDeadButtonObs,
                                           getFilterEditorObs    : O => FilterEditorObs,
                                           getPromptJs           : R => TestPromptJs) {
  private implicit class RefExt(r: R) {
    def promptJs = getPromptJs(r)
  }

  private implicit class ObsExt(o: O) {
    def filter     = getFilterEditorObs(o)
    def filterDead = getFilterDeadButtonObs(o)
    def savedViews = getSavedViewManagerObs(o)
  }

  val filterDead = *.focus("FilterDead").value(_.obs.filterDead.value)

  val filterText = *.focus("Filter text").value(_.obs.filter.value)

  def focusFilter: *.Actions =
    *.action("Focus filter") { x =>
      val dom = x.obs.filter.input
      dom.focus()
      Simulate focus dom
    }

  def enterFilter(f: String): *.Actions = {
    val e = SimEvent.Change(f)
    *.action(s"enterFilter('$f')")(e simulate _.obs.filter.input)
      .addCheck(filterText.assert(f).after)
  }

  val savedViews = *.focus("Saved views").collection(_.obs.savedViews.views.map(_.name_++))

  val activeSavedView = *.focus("Active saved views").value(_.obs.savedViews.activeView.name)

  def selectView(name: String): *.Actions =
    *.action(s"Select view: $name")(_.obs.savedViews.needView(name).select())

  def setDefaultView(name: String): *.Actions =
    *.action(s"Set default view: $name")(_.obs.savedViews.needView(name).setAsDefault())

  def saveAndReplaceView(name: String): *.Actions =
    *.action(s"Save and replace view: $name")(_.obs.savedViews.needView(SavedView.Name.unsaved.value).replace(name))

  def saveCurrentView(name: String): *.Actions =
    *.action(s"Save current view as: $name") { x =>
      x.ref.promptJs.setNextResponse(name)
      x.obs.savedViews.activeView.saveAsNew()
    }

  lazy val filterDeadToggleNoOp: *.Actions =
    *.action("filterDeadToggleNoOp")(Simulate click _.obs.filterDead.button)
      .addCheck(filterDead.assert.not.change)

  lazy val filterDeadToggle: *.Actions =
    *.action("filterDeadToggle")(Simulate click _.obs.filterDead.button)
      .addCheck(filterDead.assert.change)

  def setFilterDead(fd: FilterDead): *.Actions =
    filterDeadToggle.unless(_.obs.filterDead.value ==* fd).rename(s"setFilterDead($fd)")

}
