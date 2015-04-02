package shipreq.webapp.client.app.ui.reqtable

import java.util.regex.Pattern
import japgolly.scalacss.ScalaCssReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.console
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom.raw.HTMLInputElement
import scalajs.js
import scalaz.{\/, -\/, \/-, Tags}
import scalaz.effect.IO
import scalaz.std.vector._
import scalaz.std.option._
import scalaz.std.stream._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import shapeless.syntax.singleton._

import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Must, UnivEq, Rx}
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.webapp.base.{Grammar, UiText}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.Presentation
import shipreq.webapp.client.lib.ui.UI

object TextSeqEditor {
  type S = String
  type ParseResult[+O] = Option[String] \/ O

  type AutoComplete = Rx[TC.Strategies]

  final case class Format(normAll: EndoFn[String], sep: Pattern, normEach: EndoFn[String], ignore: String => Boolean) {
    def apply(input: String): Stream[String] =
      (input |> normAll |> sep.split).toStream map normEach filterNot ignore
  }

  val pubidSeqFormat =
    Format(_.trim, "[ ,]+".r.pattern, _.replace("-", "").toUpperCase, _.isEmpty)

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
          TC(nn, strategies)
          TC.onSelect(nn) {
            $.props.stateUpdate(n.value).unsafePerformIO()
          }
        }
      }
      .domType[HTMLInputElement]
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val cancelOnEscape = UI.keyDispatch(_.key) {
      case KeyValue.Escape => $.props.abort
    }

    val onChange: ReactEventI => IO[Unit] =
      e => $.props.stateUpdate(e.target.value)

    def render: ReactElement = {
      val p = $.props

      val parseResult =
        fmt(p.state)
          .map(p.parse(_).bimap(Tags.First.apply, Vector.empty :+ _))
          .suml

      def onKeyPress = UI.keyDispatch(_.key) {
        case KeyValue.Enter => parseResult.fold(_ => js.undefined, p.commit)
      }

      <.input(
        *.cellEditor(parseResult.isLeft),
        ^.`type`      := "text",
        ^.value       := p.state,
        ^.onChange   ~~> onChange,
        ^.onKeyDown  ~~> cancelOnEscape,
        ^.onKeyPress ~~> onKeyPress)
    }
  }

  def cellState(p: Props): Cell.Editing =
    Cell.Editing(component(p))
}

// =====================================================================================================================
// TODO Hide dead tags & maintain across edits (unless show deleted is on)

object TagEditor {
  import shipreq.webapp.base.data._

  type A = ApplicableTag.Id

  type Lookup = Map[String, A] // TODO ¿ case class Lookup(legal: Map[String, A], suggest: Set[String]) ?

  final val editor = new TextSeqEditor[A](hashtagSeqFormat)

  def lookupForNoCol(p: Rx[Project]): Rx[Lookup] =
    lookupRx(p, _.tagsNotUsedInColumns)

  def lookupForCol(p: Rx[Project], f: CustomField.Tag.Id): Rx[Lookup] =
    lookupRx(p, _.tagsForColumn(f))

  def lookupRx(project: Rx[Project], f: TagColumnDistribution => Must[Set[ApplicableTag]]): Rx[Lookup] =
    project.map(p =>
      mustResolve(f(p.tagColumnDistribution))(UnivEq.emptySet)
        .toStream
        .map(_.tmap2(_.key.value, _.id))
        .toMap)

  def apply(initial : Vector[A],
            project : Project,
            lookup  : Rx[Lookup],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    val init: S =
      initial.map { a =>
        val m = project.atag(a).map(_.key.value)
        UiText.mustA(m)
      } mkString " "

    val autoComplete: AutoComplete =
      lookup.map(l =>
        TC.Strategies(
          TC.Strategy(s"\\b(${Grammar.hashRefKeyChars.+})$$")
            .search(TC.caseInsensitiveContains(l.keys.toStream.sorted))
            .replace(_ + " ")
            .index(1)
        ))

    def parse(s: S): ParseResult[A] =
      lookup.value().get(s) match {
        case Some(id) => \/-(id)
        case None     => leftNone
      }

    val abort: IO[Unit] =
      setState(None)

    val commit: Vector[A] => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

    lazy val update: S => IO[Unit] =
      s => setState(Some(newState(s)))

    def newState(state: S) =
      editor cellState editor.Props(state, update, abort, parse, commit, autoComplete)

    newState(init)
  }
}


// =====================================================================================================================
// TODO Hide dead reqs & maintain across edits (unless show deleted is on)
// TODO ImplicationEditor needs validation

object ImplicationEditor {
  import shipreq.webapp.base.data._
  import DataImplicits._

  type A = Req.Id

  case class LookupV(desc: String, req: Req, display: String) {
    val descNorm = pubidSeqFormat.normEach(desc)
  }

  type Lookup = Map[String, LookupV]

  final val editor = new TextSeqEditor[A](pubidSeqFormat)

  def lookupAll(project: Rx[Project], reqDescFn: Rx[Req => String]): Rx[Lookup] =
    for {
      p       <- project
      reqDesc <- reqDescFn
    } yield {
      val reqs = p.reqs.data.reqs.values.toStream
      val m = Must.foldMapM[Req, Stream, (String, LookupV)](reqs)(r =>
        Presentation.pubid(r.pubid)(p).map { pubidTxt =>
          val key = pubidSeqFormat.normEach(pubidTxt)
          (key, LookupV(reqDesc(r), r, pubidTxt))
        }
      )
      mustResolve(m)(Stream.empty).toMap
    }

  def lookupForCol(project: Rx[Project], lookup: Rx[Lookup], fid: CustomField.Implication.Id): Rx[Lookup] =
    for {
      p <- project
      l <- lookup
    } yield {
      val m = p.customField(fid).map(f => l.filter(t => t._2.req.reqTypeId ≟ f.reqTypeId))
      mustResolve(m)(Map.empty)
    }

  def apply(initial : Vector[Pubid],
            project : Project,
            lookup  : Rx[Lookup],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    val init: S =
      initial.map(pid =>
        UiText mustA Presentation.pubid(pid)(project)
      ) mkString " "

    val autoComplete: AutoComplete =
      lookup.map { l =>
        val sorted = l.toStream.sortBy(_._2.tmap2(_.req.pubid.pos.value, _.display))

        def li(v: LookupV): ReactElement =
          *.reqAutoComplete('req)(r => _('desc)(d =>
            <.div(
              <.div(r, v.display),
              <.div(d, v.desc))
          ))

        TC.Strategies(
          TC.Strategy(s"\\b(\\S+)$$")
            .search[LookupV](term => {
              val n = pubidSeqFormat.normEach(term)
              // TODO [low] Search algorithm won't scale well
              sorted.filter(x => x._1.contains(n) || x._2.descNorm.contains(n)).map(_._2)
            })
            .replace(_.display + " ")
            .index(1)
            .template(v => React.renderToStaticMarkup(li(v)))
        )
      }

    def parse(s: S): ParseResult[A] =
      lookup.value().get(s) match {
        case Some(v) => \/-(v.req.id)
        case None    => leftNone
      }

    val abort: IO[Unit] =
      setState(None)

    val commit: Vector[A] => IO[Unit] =
    // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

    lazy val update: S => IO[Unit] =
      s => setState(Some(newState(s)))

    def newState(state: S) =
      editor cellState editor.Props(state, update, abort, parse, commit, autoComplete)

    newState(init)
  }
}