package shipreq.webapp.client.project.feature.savedview

import japgolly.scalajs.react.test.Simulate
import org.scalajs.dom
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style

final class SavedViewManagerObs($: DomZipperJs) {
  import SavedViewManagerObs._

  val views = $.collect0n(".item.ui.dropdown").map(new View(_))

  val List(activeView) = views.iterator.filter(_.isActive).toList

  def needView(name: String): View =
    views.find(_.name == name).getOrElse(sys.error(s"Saved views [${views.map(_.name).mkString(" | ")}] found but not [$name]"))
}

object SavedViewManagerObs {
  def needIn($: DomZipperJs): SavedViewManagerObs =
    new SavedViewManagerObs($(s"[${ViewManager.devMarker.attrName}]"))

  final class View($: DomZipperJs) {
    val isDefault = $.exists("i.icon.star.yellow")
    val isActive = $.classes.contains(Style.savedViews.activeItem.className.value)
    val actions = $.collect0n(".item").map(new Action(_))

    val name = $.domAsHtml.childNodes.iterator.collectFirst { case t: dom.Text => t.textContent }.get
    val name_++ = (if (isActive) "> " else "") + (if (isDefault) "* " else "") + name

    def select(): Unit =
      Simulate click $.domAsHtml

    def needAction(name: String): Action =
      actions.find(_.name == name).getOrElse(sys.error(s"Saved view [${this.name}] has actions [${actions.map(_.name).mkString(" | ")}] but not [$name]"))

    def saveAsNew(): Unit =
      needAction("Save as new...").click()

    def setAsDefault(): Unit =
      needAction("Set as default").click()

    def replace(name: String): Unit =
      needAction(s"Replace $name").click()
  }

  final class Action($: DomZipperJs) {
    val name = $.innerText
    def click(): Unit = Simulate.click($.domAsHtml)
  }
}