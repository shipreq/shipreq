package shipreq.webapp.client.test

import scalaz.Equal
import org.scalajs.dom.html
import scalajs.js.{UndefOr, undefined}
import shipreq.webapp.base.test.WebappTestUtil._
import Sizzle.{DOM, Result}

object DomZipper {
  def apply(dom: Result): DomZipper =
    apply(null, dom)

  def apply(desc: String, dom: Result): DomZipper =
    apply(desc, 1, dom, 0)

  def apply(expectedCount: Int, dom: Result, selectIndex: Int): DomZipper =
    apply(null, expectedCount, dom, selectIndex)

  def apply(desc: String, expectedCount: Int, dom: Result, selectIndex: Int): DomZipper = {
    utest.assert(selectIndex < expectedCount)
    assertCount(Option(desc) getOrElse "DomZipper.apply", expectedCount, dom, undefined)
    val n = dom(selectIndex)
    new DomZipper(n)
  }

  def assertCount(desc: String, expectedCount: Int, dom: Result, root: UndefOr[DOM]): Unit = {
    def showDom(inner: Boolean)(d: DOM) = {
      val html = if (inner) d.innerHTML else d.outerHTML
      "\n" + removeReactIds(html).take(160)
    }
    def detail =
      if (dom.isEmpty)
        root.fold("")(showDom(true))
      else
        dom.map(showDom(false))
    assertEq(desc + detail, dom.length, expectedCount)
  }

  def first(desc: String, dom: Result): DomZipper = {
    if (dom.isEmpty)
      fail(desc + ": empty")
    new DomZipper(dom.head)
  }

  def removeReactIds(html: String): String =
    html.replaceAll(""" data-reactid=".*?"""", "")

  implicit val equality: Equal[DomZipper] =
    Equal.equal((a, b) => a.get isSameNode b.get)
}

class DomZipper(root: DOM) {
  override def toString =
    s"DomZipper(${DomZipper removeReactIds root.outerHTML take 160})"

  def getAll(sel: String): Result =
    Sizzle(sel, root)

  def getAll(expectedCount: Int, sel: String): Result = {
    val r = Sizzle(sel, root)
    DomZipper.assertCount(sel, expectedCount, r, root)
    r
  }

  def option(sel: String): Option[DomZipper] = {
    val r = Sizzle(sel, root)
    if (r.isEmpty)
      None
    else
      Some(DomZipper(sel, r))
  }

  def apply(cssSel: String): DomZipper =
    apply(1, cssSel, 0)

  def apply(expectedCount: Int, cssSel: String, selectIndex: Int): DomZipper = {
    utest.assert(selectIndex < expectedCount)
    val n = getAll(expectedCount, cssSel)(selectIndex)
    new DomZipper(n)
  }

  def collect[A](sel: String, f: DOM => A): Vector[A] =
    getAll(sel).foldLeft(Vector.empty[A])(_ :+ f(_))

  def collectD[A](sel: String, f: DomZipper => A): Vector[A] =
    collect(sel, d => f(new DomZipper(d)))

  def collectInnerHTML[A](sel: String): Vector[String] =
    collect(sel, _.innerHTML)

  def collectInnerText[A](sel: String): Vector[String] =
    collect(sel, _.textContent)

  def get: DOM =
    root

  def as[D <: DOM]: D =
    root.asInstanceOf[D]

  def outerHTML: String = root.outerHTML
  def innerHTML: String = root.innerHTML
  def innerText: String = root.textContent

  def selectedOptionText: String = {
    val s = as[html.Select]
    s.options(s.selectedIndex).innerHTML
  }

  def inputChecked: Boolean =
    as[html.Input].checked

//    def collect2[A, B](selA: String, a: DOM => A)(selB: String, b: DOM => B): Vector[(A, B)] = {
//      val as = collect(selA, a)
//      val bs = collect(selB, b)
//      assertEq(as.length, bs.length)
//      as zip bs
//    }
}
