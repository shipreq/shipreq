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
import scalaz.syntax.either._
import scalaz.syntax.equal._
import scalaz.syntax.foldable._
import shapeless.syntax.singleton._

import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Must, UnivEq, Rx}
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.webapp.base.UiText
import shipreq.webapp.base.text.{Grammar, Presentation}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.ui.UI

object TextSeqEditor {
  type S = String
  type ParseRejection  = Option[String]
  type ParseResult[+O] = ParseRejection \/ O
  type Parser[+O]      = () => S => ParseResult[O]

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

final class TextSeqEditor[A](name: String, val fmt: Format) {

  case class Props(state       : S,
                   stateUpdate : S => IO[Unit],
                   abort       : IO[Unit],
                   parser      : Parser[A],
                   commit      : Vector[A] => IO[Unit],
                   autoComplete: AutoComplete)

  val textEditorRef = Ref[HTMLInputElement]("i")

  val component =
    ReactComponentB[Props](name)
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .componentDidMount { $ =>
        val n = textEditorRef($).get.getDOMNode()
        n.focus()
        n.select()

        // TODO Should update autoComplete if needed on props change
        val strategies = $.props.autoComplete.value()
        UI.textComplete(n, strategies, $.props.stateUpdate)
      }
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val cancelOnEscape = UI.keyDispatch(_.key) {
      case KeyValue.Escape => $.props.abort
    }

    val onChange: ReactEventI => IO[Unit] =
      e => $.props.stateUpdate(e.target.value)

    def render: ReactElement = {
      val p = $.props

      val parse = p.parser()
      val parseResult =
        fmt(p.state)
          .map(parse(_).bimap(Tags.First.apply, Vector.empty :+ _))
          .suml

      def onKeyPress = UI.keyDispatch(_.key) {
        case KeyValue.Enter => parseResult.fold(_ => js.undefined, p.commit)
      }

      <.div(
        <.input(
          ^.ref := textEditorRef,
          *.cellEditor(parseResult.isLeft),
          ^.`type`      := "text",
          ^.value       := p.state,
          ^.onChange   ~~> onChange,
          ^.onKeyDown  ~~> cancelOnEscape,
          ^.onKeyPress ~~> onKeyPress),
        parseResult.swap.toOption.flatMap(Tags.First.unwrap).map(err =>
          <.div(*.cellEditorErrMsg, err)
        ))
    }
  }

  def cellState(p: Props): Cell.Editing =
    Cell.Editing(component(p))
}

// =====================================================================================================================
// TODO Hide dead tags & maintain across edits (unless show deleted is on)

object TagEditor {
  import shipreq.webapp.base.data._
  import Grammar.{hashRefKey => G}

  type A = ApplicableTag.Id

  type Lookup = Map[String, A]

  final val editor = new TextSeqEditor[A]("TagEditor", hashtagSeqFormat)

  def lookupForNoCol(p: Project): Must[Lookup] =
    lookupG(p, _.tagsNotUsedInColumns)

  def lookupForCol(p: Project, f: CustomField.Tag.Id): Must[Lookup] =
    lookupG(p, _.tagsForColumn(f))

  def lookupG(p: Project, f: TagColumnDistribution => Must[Set[ApplicableTag]]): Must[Lookup] =
    f(p.tagColumnDistribution).map(
      _.toStream
      .map(_.tmap2(_.key.value, _.id))
      .toMap
    )

  def apply(initial : Vector[A],
            project : Project,
            lookupM : Rx[Must[Lookup]],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    def init: S =
      initial.map { a =>
        val m = project.atag(a).map(_.key.value)
        UiText.mustA(m)
      } mkString " "

    val lookup = lookupM.map(mustResolve(_)(UnivEq.emptyMap))

    val autoComplete: AutoComplete =
      lookup.map(l =>
        TC.Strategies(
          TC.Strategy(s"\\b(${G.firstChar.one}${G.allChars.*})$$")
            .search(TC.caseInsensitiveContains(l.keys.toStream.sorted))
            .replace(_ + " ")
            .index(1)
        ))

    val parser: Parser[A] =
      () => s =>
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
      editor cellState editor.Props(state, update, abort, parser, commit, autoComplete)

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

  final val editor = new TextSeqEditor[A]("ImplicationEditor", pubidSeqFormat)

  @inline def norm = editor.fmt.normEach

  case class LookupV(desc: String, req: Req, display: String) {
    val descNorm = norm(desc)
  }

  case class Lookup(legal: Map[String, LookupV], illegal: Map[String, ParseRejection]) {
    def outlaw(rej: ParseRejection, f: Req => Boolean): Lookup = {
      var legal2   = legal
      var illegal2 = illegal
      for ((k, v) <- legal)
        if (f(v.req)) {
          illegal2 = illegal2.updated(k, rej)
          legal2 -= k
        }
      Lookup(legal2, illegal2)
    }
  }

  def lookupAll(p: Project, reqDesc: Req => String): Must[Lookup] = {
    val reqs = p.reqs.data.reqs.values.toStream
    val m = Must.foldMapM[Req, Stream, (String, LookupV)](reqs)(r =>
      Presentation.pubid(r.pubid)(p).map { pubidTxt =>
        val key = norm(pubidTxt)
        (key, LookupV(reqDesc(r), r, pubidTxt))
      }
    )
    m.map(legal => Lookup(legal.toMap, UnivEq.emptyMap))
  }

  def lookupForCol(p: Project, l: Lookup, fid: CustomField.Implication.Id): Must[Lookup] =
    p.customField(fid).map(f =>
      l.outlaw(None, _.reqTypeId ≠ f.reqTypeId))

  def declFwd(c: Column.ImplicationSrc.type): Boolean = false
  def declFwd(c: Column.ImplicationTgt.type): Boolean = true
  def declFwd(c: CustomField.Implication.Id): Boolean = false

  /**
   * @param declFwd If true, the user edits what this subject implies (ie. subject → edit-specified).
   *                If false, then it's what implies this subject     (ie. subject ← edit-specified).
   */
  def lookupForSubject(p: Project, l: Lookup, subject: Req.Id, declFwd: Boolean): Lookup = {
    val (a, b) = if (declFwd) (p.implicationTgtToSrcTC, p.implicationSrcToTgtTC)
                 else         (p.implicationSrcToTgtTC, p.implicationTgtToSrcTC)
    l.outlaw(
      Some("That would cause a cycle in your implication graph."),
      r => a(subject).contains(r.id) || b(r.id).contains(subject))
  }

  def apply(initial : Vector[Pubid],
            project : Rx[Project],
            lookupM : Rx[Must[Lookup]],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    def init: S = {
      val p = project.value()
      initial.map(pid =>
        UiText mustA Presentation.pubid(pid)(p)
      ) mkString " "
    }

    val lookup = lookupM.map(mustResolve(_)(Lookup(UnivEq.emptyMap, UnivEq.emptyMap)))

    val autoComplete: AutoComplete =
      lookup.map { l =>
        val sorted = l.legal.toStream.sortBy(_._2.tmap2(_.req.pubid.pos.value, _.display))

        def li(v: LookupV): ReactElement =
          *.reqAutoComplete('req)(r => _('desc)(d =>
            <.div(
              <.div(r, v.display),
              <.div(d, v.desc))
          ))

        TC.Strategies(
          TC.Strategy(s"\\b(\\S+)$$")
            .search[LookupV](term => {
              val n = norm(term)
              // TODO [pri=low] Search algorithm won't scale well
              sorted.filter(x => x._1.contains(n) || x._2.descNorm.contains(n)).map(_._2)
            })
            .replace(_.display + " ")
            .index(1)
            .template(v => React.renderToStaticMarkup(li(v)))
        )
      }

    val parser: Parser[A] = () => {
      val l = lookup.value()
      s =>
        l.legal.get(s).map(_.req.id.right) orElse
          l.illegal.get(s).map(-\/.apply) getOrElse
            leftNone
    }

    val abort: IO[Unit] =
      setState(None)

    val commit: Vector[A] => IO[Unit] =
    // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
      s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

    lazy val update: S => IO[Unit] =
      s => setState(Some(newState(s)))

    def newState(state: S) =
      editor cellState editor.Props(state, update, abort, parser, commit, autoComplete)

    newState(init)
  }
}