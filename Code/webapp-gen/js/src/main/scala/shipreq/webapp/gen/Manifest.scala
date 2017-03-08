package shipreq.webapp.gen

import japgolly.scalajs.react.vdom.Implicits._
import shipreq.webapp.client.project.app.root.LoadingPage

object Manifest {

  sealed trait Entry {
    type A
    val gen: Generator[A]
  }

  sealed abstract class Aux[I](g: Generator[I]) extends Entry {
    override type A = I
    override val gen = g
  }

  case object ProjectSpaLoader extends Aux(
    Generator("ProjectSpaLoader", Data.projectSpaLoaderData) { case (u, p) => LoadingPage.Props(u, p).render })

  val All: List[Entry] =
    List(ProjectSpaLoader)
}
