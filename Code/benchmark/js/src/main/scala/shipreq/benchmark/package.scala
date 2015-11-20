package shipreq

import japgolly.scalajs.benchmark._
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import shipreq.webapp.base.data.Project

package object benchmark {

  /**
    * Builder for a benchmark that accepts a project.
    */
  val projectBM = _setup[Unit, Project](_ =>
    data.project_100)

  // TODO Add ↓ to scalajs-benchmark

  def _setup[A, B](prepare: A => B): _BuilderX[A, B] =
    new _BuilderX[A, B](prepare)

  trait _Builder[A, B] {
    def apply(name: String)(f: B => Any): Benchmark[A]
  }

  class _BuilderX[A, B](val prepare: A => B) extends _Builder[A, B] {
    override def apply(name: String)(f: B => Any): Benchmark[A] =
      new Benchmark(name, Setup { a =>
        val b = prepare(a)
        () => f(b)
      })
    def map[C](f: B => C): _BuilderX[A, C] =
      new _BuilderX(f compose prepare)
  }

  import gui._, MenuComp._
  implicit def _autoLiftGuiSuite(s: GuiSuite[_]): MenuItems =
    MenuSuite(UrlFrag from s.name, s)

  def BASEURL = BaseUrl(dom.window.location.href.takeWhile(_ != '?'))
}
