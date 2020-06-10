package shipreq.webapp.base.text

import japgolly.univeq.UnivEq
import org.parboiled2._
import scalaz.{-\/, Applicative, Functor, Monoid, \/, \/-}
import shipreq.base.util.{Backwards, Direction, Forwards}
import shipreq.webapp.base.data.derivation.UseCaseStepLabelLookup
import shipreq.webapp.base.data.{ReqTypePos, UseCaseStepId}
import shipreq.webapp.base.util.ParsingUtil

/**
 * Use case step titles have textual representations of step-flow, eg: "Try again if failed. --> 1.0.3".
 *
 * This deals with the flow portion.
 */
object UseCaseStepFlowText {

  sealed abstract class Elem[+T, +S] {
    final def bimap[F[_], TT, SS](f: T => F[TT], g: S => F[SS])(implicit F: Applicative[F]): F[Elem[TT, SS]] =
      this match {
        case Elem.Text(text) => F.map(f(text))(Elem.Text(_))
        case Elem.Step(step) => F.map(g(step))(Elem.Step(_))
        case a: Elem.Arrow   => F.point(a)
      }

    final def mapT[F[_], TT, SS >: S](f: T => F[TT])(implicit F: Applicative[F]): F[Elem[TT, SS]] =
      bimap[F, TT, SS](f, F.point(_))

    final def mapS[F[_], TT >: T, SS](f: S => F[SS])(implicit F: Applicative[F]): F[Elem[TT, SS]] =
      bimap[F, TT, SS](F.point(_), f)
  }

  object Elem {
    sealed abstract class Flow[+S] extends Elem[Nothing, S]

    case class Text[+T](text: T)     extends Elem[T, Nothing]
    case class Step[+S](step: S)     extends Flow[S]
    case class Arrow(dir: Direction) extends Flow[Nothing]

    implicit def univEq[T: UnivEq, S: UnivEq]: UnivEq[Elem[T, S]] = UnivEq.force
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val AsciiArrows: Direction => String = {
    case Forwards  => "-->"
    case Backwards => "<--"
  }

  object DefaultArrowOrder {
    val _1 = Backwards
    val _2 = Forwards

    def map[A](f: Direction => A): (A, A) =
      (f(_1), f(_2))

    def foreach[A](f: Direction => A): Unit = {
      f(_1)
      f(_2)
    }
  }

  /**
    * @return `Text` is never an empty string.
    *         `Text`s are never consecutive.
    *         `Step`s are always preceded by an `Arrow` or another `Step`.
    *         `Step`s never contain whitespace.
    *         Property tests enforce the above0.
    */
  def parse(input: String): Seq[Elem[String, String]] =
    new TextAndFlowParser(input).main.run()(Parser.DeliveryScheme.Throw)

  private final class TextAndFlowParser(val input: ParserInput) extends ParsingUtil {
    import ParsingUtil._

    private def arrowF: Rule1[Forwards.type] =
      rule('-' ~ ch('-').+ ~ '>' ~ push(Forwards))

    private def arrowB: Rule1[Backwards.type] =
      rule("<-" ~ ch('-').+ ~ push(Backwards))

    private def arrow: Rule1[Elem.Arrow] =
      rule(
        !lastCharIs(PunctuationOrSymbol) ~
        (arrowF | arrowB) ~
        (&(ch('.')) | !PunctuationOrSymbol)
        ~> Elem.Arrow)

    private def stepSeperator: Rule0 =
      rule(SLWS | ',')

    private def step: Rule1[Elem.Step[String]] =
      rule(capture(oneOrMore(!(ch(EOI) | stepSeperator | arrow) ~ ANY)) ~> (Elem.Step(_: String)))

    private def flowClause: Rule1[Seq[Elem.Flow[String]]] =
      rule(
        arrow ~ stepSeperator.* ~ zeroOrMore(step).separatedBy(stepSeperator.+) ~ stepSeperator.*
        ~> ((a: Elem.Arrow, t: Seq[Elem.Flow[String]]) => a +: t)
      )

    type E = Elem[String, String]
    type ES = Seq[E]

    private def line: Rule1[ES] =
      rule(!EOI ~ capture((!EOI ~ !arrow ~ ANY).*) ~ flowClause.* ~> flattenLine)

    private val flattenLine: (String, Seq[Seq[Elem.Flow[String]]]) => ES =
      (t, f) => {
        var r: ES = f.flatten
        if (t.nonEmpty)
          r = Elem.Text(t) +: r
        r
      }

    def main: Rule1[ES] =
      rule(line.* ~ EOI ~> ((_: Seq[ES]).flatten))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def parseStep(lookup: UseCaseStepLabelLookup, currentUseCase: Option[ReqTypePos])(input: String): String \/ UseCaseStepId = {
    import UseCaseStepLabelLookup.Failure
    new StepParser(lookup, currentUseCase, input).useCaseStepLabelAttempt.run()(Parser.DeliveryScheme.Either) match {
      case Right(id@ \/-(_))                => id
      case Right(-\/(a: Failure.Ambiguous)) => -\/(a.errMsg(input))
      case Right(-\/(Failure.NotFound))     => -\/("Invalid step: " + input)
      case Left(_)                          => -\/("Invalid step: " + input)
    }
  }

  private final class StepParser(override val useCaseStepLabelLookup: UseCaseStepLabelLookup,
                                 override val currentUseCase: Option[ReqTypePos],
                                 override val input: ParserInput) extends Parsers.UseCaseStepLabel {
    override def OWS = rule(SLWS.*)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class TextAndFlow[+T, +S](text: T, flow: Direction.Values[S]) {

    def fold[A](f: T => A)(g: (A, S) => A): A =
      g(g(f(text), flow(Forwards)), flow(Backwards))

    def bimap[TT, SS](f: T => TT, g: S => SS): TextAndFlow[TT, SS] =
      TextAndFlow(f(text), flow.map(g))

    def bimapD[TT, SS](f: T => TT, g: Direction => S => SS): TextAndFlow[TT, SS] =
      TextAndFlow(f(text), Direction.Values(d => g(d)(flow(d))))

//    def compose[T2, S2, X, Y](that: TextAndFlow[T2, S2])
//                             (f: (T, T2) => X, g: (S, S2) => Y): TextAndFlow[X, Y] =
//      TextAndFlow(
//        f(this.text, that.text),
//        d => g(this flow d, that flow d))

    def composeF[F[_], T2, S2, X, Y](that: F[TextAndFlow[T2, S2]])
                                    (f: (T, F[T2]) => X, g: (S, F[S2]) => Y)
                                    (implicit F: Functor[F]): TextAndFlow[X, Y] =
      TextAndFlow(
        f(this.text, F.map(that)(_.text)),
        Direction.Values(d => g(this flow d, F.map(that)(_ flow d))))

//    def zip[T2, S2, X, Y](that: TextAndFlow[T2, S2]): TextAndFlow[(T, T2), (S, S2)] =
//      compose(that)((_, _), (_, _))
  }

  object TextAndFlow {
    implicit def univEq[T: UnivEq, S: UnivEq]: UnivEq[TextAndFlow[T, S]] = UnivEq.derive
  }

  def separateTextAndFlow[T, S](es: IterableOnce[Elem[T, S]])(implicit M: Monoid[T]): TextAndFlow[T, Vector[S]] = {
    var t = M.zero
    var fwd = Vector.empty[S]
    var bck = Vector.empty[S]
    var dir: Direction = null
    es.iterator foreach {
      case Elem.Text(text) => t = M.append(t, text); dir = null
      case Elem.Arrow(d)   => dir = d
      case Elem.Step(step) => dir match {
        case Forwards  => fwd :+= step
        case Backwards => bck :+= step
      }
    }
    TextAndFlow(t, Direction.Values {
      case Forwards  => fwd
      case Backwards => bck
    })
  }

}
