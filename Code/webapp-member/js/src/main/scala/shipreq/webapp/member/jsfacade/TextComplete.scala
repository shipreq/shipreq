package shipreq.webapp.member.jsfacade

import org.scalajs.dom.html
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.{Function1 => -->, Function2 => JsFn2, Function3 => JsFn3, RegExp, UndefOr, |}
import shipreq.webapp.member.jsfacade.TextComplete._

@js.native
@JSGlobal("TextComplete")
@nowarn
final class TextComplete(val editor: Editor, options: Options = js.native) extends js.Any {

  def register(ss: js.Array[Strategy[_]]): this.type = js.native

  def hide(): this.type = js.native

  def on(event: String, fn: js.Function0[Unit]): this.type = js.native

  def trigger(text: String): this.type = js.native

  def destroy(destroyEditor: Boolean = true): this.type = js.native

  val _events: _Events = js.native

  val dropdown: Dropdown = js.native
}

object TextComplete {

  @js.native
  @nowarn
  sealed trait Editor extends js.Object {
    /** @param options code: ("UP" | "DOWN") */
    def emitMoveEvent(options: js.Object): Unit = js.native
    def emitEnterEvent(): Unit = js.native
    def emitEscEvent(): Unit = js.native
  }

  @js.native
  @JSGlobal("TextCompleteTA")
  @nowarn
  final class TextArea(element: html.TextArea) extends Editor

  @js.native
  sealed trait Dropdown extends js.Object {
    def deactivate(): this.type = js.native
  }

  @js.native
  sealed trait _Events extends js.Object {
    val select: js.UndefOr[js.Object]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  @js.native
  trait MatchData extends js.Array[String] {
    var index: js.UndefOr[Int]
  }
  object MatchData {
    def empty(): MatchData =
      new js.Array[String].asInstanceOf[MatchData]

    def apply(as: IterableOnce[String], index: js.UndefOr[Int]): MatchData = {
      val md = empty()
      as.iterator.foreach(md.push(_))
      md.index = index
      md
    }
  }

  @js.native
  trait Strategy[A] extends js.Object {
    var `match` : Strategy.Match
    var search  : Strategy.Search  [A]
    var replace : Strategy.Replace [A]
    var cache   : Strategy.Cache
    var context : Strategy.Context
    var template: Strategy.Template[A]
    var index   : Strategy.Index
    var id      : Strategy.Id
  }

  object Strategy {

    /** Used by Replace to represent replacement values before & after the cursor */
    type StringPair   = js.Array[String]
    type Term         = js.UndefOr[String]
    type Match        = RegExp | (String --> MatchData) | Null
    type Search  [ A] = JsFn3[Term, (js.Array[A] --> Unit), MatchData, Unit]
    type Replace [-A] = A --> (StringPair | String | Null)
    type Cache        = UndefOr[Boolean]
    type Context      = UndefOr[String --> UndefOr[String]]
    type Template[-A] = UndefOr[JsFn2[A, String, String]]
    type Index        = UndefOr[Int]
    type Id           = UndefOr[String]

    object Search {
      def apply[A](f: String => IterableOnce[A]): Search[A] =
        (term, cb, _) => {
          val as = new js.Array[A]
          val t = term.getOrElse("")
          f(t).iterator.foreach(as.push(_))
          cb(as)
        }
    }

    object Replace {
      def apply[A](f: A => (StringPair | String | Null)): Replace[A] = f

      def pair[A](f: A => (String, String)): Replace[A] =
        apply[A] { a =>
          val x = f(a)
          js.Array(x._1, x._2)
        }
    }

    @inline def builder = Step1

    object Step1 {
      def regex(pattern: String, flags: String = "", index: Index = ()): Step2 = regexp(new RegExp(pattern, flags), index)
      def regexp(r: RegExp,                          index: Index = ()): Step2 = new Step2(r, index)
      def apply(f: String => MatchData,              index: Index = ()): Step2 = new Step2(f: String --> MatchData, index)
    }

    final class Step2(`match`: Match, index: Index) {
      def search  [A](f: String => IterableOnce[A]): Step3a[A] = new Step3a(`match`, index, Search(f))
      def replace [A](f: A => String)              : Step3b[A] = new Step3b[A](`match`, index, Replace apply f)
      def replace2[A](f: A => (String, String))    : Step3b[A] = new Step3b[A](`match`, index, Replace pair f)
    }

    final class Step3a[A](`match`: Match, index: Index, search: Search[A]) {
      private def ready(replace: Replace[A]): Ready[A] = new Ready[A](`match`, index, search, replace, _ => ())
      def replace (f: A => String)          : Ready[A] = ready(Replace apply f)
      def replace2(f: A => (String, String)): Ready[A] = ready(Replace pair f)
    }

    final class Step3b[A](`match`: Match, index: Index, replace: Replace[A]) {
      private def ready(search: Search[A]): Ready[A] = new Ready(`match`, index, search, replace, _ => ())
      def search(f: String => IterableOnce[A]): Ready[A] = ready(Search(f))
    }

    final class Ready[A](`match`: Match,
                         index  : Index,
                         search : Search[A],
                         replace: Replace[A],
                         other  : Strategy[A] => Unit) {

      private def add(f: Strategy[A] => Unit): Ready[A] =
        new Ready(`match`, index, search, replace,
          s => { other(s); f(s) })

      def cache(c: Boolean): Ready[A] =
        add(_.cache = c)

      def context(f: String => UndefOr[String]): Ready[A] =
        add(_.context = f: String --> UndefOr[String])

      def template(f: (A, String) => String): Ready[A] =
        add(_.template = f: JsFn2[A, String, String])

      def id(id: String): Ready[A] =
        add(_.id = id)

      def result(): Strategy[A] = {
        val b = (new js.Object).asInstanceOf[Strategy[A]]
        b.`match` = `match`
        b.index   = index
        b.search  = search
        b.replace = replace
        other(b)
        b
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  @js.native
  sealed trait Options extends js.Object
  /*
  dropdown: {
    maxCount: Infinity
  }
   */
}
