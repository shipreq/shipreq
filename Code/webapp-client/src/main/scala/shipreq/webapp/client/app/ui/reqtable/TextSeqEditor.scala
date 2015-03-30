package shipreq.webapp.client.app.ui.reqtable

import java.util.regex.Pattern
import japgolly.scalacss.ScalaCssReact._
import japgolly.scalajs.jquery.TextComplete
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.console
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom.raw.HTMLInputElement
import scalajs.js

import scalaz.{\/, -\/, \/-, Tags}
import scalaz.effect.IO
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.stream._
import scalaz.syntax.foldable._

import shipreq.base.util.ScalaExt._
import shipreq.base.util.Rx
import shipreq.base.util.effect.IoUtils
import shipreq.webapp.base.UiText
import shipreq.webapp.client.app.ui.Style

object TextSeqEditor {
  type S = String
  type ParseResult[+O] = Option[String] \/ O

  type AutoComplete = Rx[TextComplete.Strategies]

  final case class Format(normAll: EndoFn[String], sep: Pattern, normEach: EndoFn[String], ignore: String => Boolean) {
    def apply(input: String): Stream[String] =
      (input |> normAll |> sep.split).toStream map normEach filterNot ignore
  }

  val hashtagSeqFormat = {
    val each = "^# *".r
    Format(_.trim, "[# ,]+".r.pattern, each.replaceFirstIn(_, ""), _.isEmpty)
  }

  val leftNone: ParseResult[Nothing] =
    -\/(None)
}

import TextSeqEditor._

final class TextSeqEditor[A](fmt: Format) {

  case class Props(state       : S,
                   stateUpdate : S => IO[Unit],
                   abort       : IO[Unit],
                   parse       : S => ParseResult[A],
                   commit      : Vector[A] => IO[Unit],
                   autoComplete: AutoComplete)

  val component =
    ReactComponentB[Props]("TextSeqEditor")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .componentDidMount { $ =>
        val n = $.getDOMNode().asInstanceOf[HTMLInputElement]
        n.focus()
        n.select()

        // TODO Should update autoComplete if needed on props change
        val strategies = $.props.autoComplete.value()
        if (strategies.nonEmpty) {
          val nn = js.Dynamic.global.$(n)
          TextComplete(nn, strategies)
          TextComplete.onSelect(nn) {
            $.props.stateUpdate(n.value).unsafePerformIO()
          }
        }
      }
      .domType[HTMLInputElement]
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val cancelOnEscape: ReactKeyboardEventH => IO[Unit] =
      e => e.key match {
        case KeyValue.Escape =>
//          val t = e.target
//          val st = ST.callback(e.preventDefaultIO >> e.stopPropagationIO, IO(t.blur()))
//          f(st)
          $.props.abort
        case _ =>
          IoUtils.nop
      }

    val onChange: ReactEventI => IO[Unit] =
      e => $.props.stateUpdate(e.target.value)

//    def onBlur: IO[Unit] = {
//    }

    def render: ReactElement = {
      val p = $.props

      val parseResult =
        fmt(p.state)
          .map(p.parse(_).bimap(Tags.First.apply, _ :: Nil))
          .suml

      <.input(
        Style.reqtable.cellEditor(parseResult.isLeft),
        ^.`type` := "text",
        ^.value := p.state,
        ^.onChange ~~> onChange,
        // ^.onBlur    ~~> $.props.,
        ^.onKeyDown ~~> cancelOnEscape)
    }
  }
}

// =====================================================================================================================

object TagEditor {
  import shipreq.webapp.base.data._

  type A = ApplicableTag.Id

  type Lookup = Map[String, A]

  final val editor = new TextSeqEditor[A](hashtagSeqFormat)

  def lookupX(project: Rx[Project]): Rx[Lookup] =
    project.map(
      _.tags.data.vstream(_.tag)
        .filterT[ApplicableTag]
        .map(_.tmap2(_.key.value, _.id))
        .toMap
    )

  def apply(initial : Vector[A],
            project : Project,
            lookup  : Rx[Lookup],
            setState: Option[Cell.State] => IO[Unit]): CellEditor = {

    val autoComplete: AutoComplete =
      lookup.map { l =>
        val ks = l.keys.toStream.sorted
        // TODO Need to merge parts of Validators, this, Presentation, and data.methods in data.xxx
        val ch = """[A-Za-z0-9\._=\-]"""
        val searchFn = TextComplete.search(term => ks.filter(_ contains term), false)
        val s = TextComplete.Strategy(s"\\b($ch+)$$")
          .search(searchFn)
          .replace(_ + " ")
          .index(1)
        js.Array(s)
      }

    val abort: IO[Unit] =
      setState(None)

    val commit: Vector[A] => IO[Unit] =
      s => IO{ println("Send to ze server: " + s) } // TODO

    lazy val update: S => IO[Unit] =
      s => setState(Some(editor(s)))

    def editor(s: S) =
      CellEditor(lookup, autoComplete, s, update, abort, commit)

    val init: S =
      initial.map { a =>
        val m = project.atag(a).map(_.key.value)
        UiText.mustA(m)
      } mkString " "

    editor(init)
  }

  case class CellEditor(lookup      : Rx[Lookup],
                        autoComplete: AutoComplete,
                        state       : S,
                        stateUpdate: S => IO[Unit],
                        abort       : IO[Unit],
                        commit      : Vector[A] => IO[Unit]) extends Cell.Editing {

    def parse(s: S): ParseResult[A] =
      lookup.value().get(s) match {
        case Some(id) => \/-(id)
        case None     => leftNone
      }

    val p = editor.Props(state, stateUpdate, abort, parse, commit, autoComplete)

    override def render =
      editor.component(p)
  }
}