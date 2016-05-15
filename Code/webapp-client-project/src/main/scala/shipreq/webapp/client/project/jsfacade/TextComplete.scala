package shipreq.webapp.client.project.jsfacade

import scalajs.js.{Function0 => JFn0, Function1 => JFn1, Function2 => JFn2, Function3 => JFn3, _}
import scalajs.js.{Any => JAny, Array => JArray, _}
import org.scalajs.dom.Event

object TextComplete {

  final val eventSelect = "textComplete:select"
  final val eventShow   = "textComplete:show"
  final val eventHide   = "textComplete:hide"

  type MatchType = RegExp // TODO | JFn1[String, RegExp]
  type MatchFn = JFn1[String, RegExp]
  @native sealed trait SearchFn[A] extends JAny
  type ReplaceFn[A] = JFn2[A, Event, JAny]

  /**
   * @tparam A The type of data returned by the `search` function.
   */
  @native
  sealed trait StrategyA[A] extends Object {
    var `match`: MatchType    = native
    var search : SearchFn[A]  = native
    var replace: ReplaceFn[A] = native

    var index     : UndefOr[Int]                     = native
    var template  : UndefOr[JFn2[A, String, String]] = native
    var cache     : UndefOr[Boolean]                 = native
    var context   : UndefOr[JFn1[A, JAny]]           = native // returns bool | string | regex | () → regex
    var idProperty: UndefOr[String]                  = native
  }

  @native
  sealed trait Callback[A] extends JAny {
    def apply(result: JArray[A], stillSearching: Boolean = false): Unit = native
  }

  def Options(): Options = (new Object).asInstanceOf[Options]

  @native
  sealed trait Options extends Object {
    var appendTo:  JAny          = native  // $('body')
    var height:    UndefOr[Int]  = native  // undefined
    var maxCount:  Int           = native  // 10
    var placement: String        = native  // ''
    var header:    UndefOr[JAny] = native  // undefined. T = string | () → string
    var footer:    UndefOr[JAny] = native  // undefined. T = string | () → string
    var zIndex:    String        = native  // '100'
    var debounce:  UndefOr[Int]  = native  // undefined. T = milliseconds
    //var adapter:   UndefOr[JAny] = native  // undefined
    var className: String        = native  // ''
  }

  type Strategy = StrategyA[_]

  def search2[A](f: (String, Callback[A]) => Unit): SearchFn[A] =
    (f: JFn2[String, Callback[A], Unit]).asInstanceOf[SearchFn[A]]

  def search3[A](f: (String, Callback[A], JArray[String]) => Unit): SearchFn[A] =
    (f: JFn3[String, Callback[A], JArray[String], Unit]).asInstanceOf[SearchFn[A]]

  def replace1[A](f: (A, Event) => String): ReplaceFn[A] =
    (f: JFn2[A, Event, String]).asInstanceOf[ReplaceFn[A]]

  def replace2[A](f: (A, Event) => (String, String)): ReplaceFn[A] = {
    val f2 = (a: A, e: Event) => {
      val r = f(a, e)
      JArray(r._1, r._2)
    }
    (f2: JFn2[A, Event, JArray[String]]).asInstanceOf[ReplaceFn[A]]
  }

  object Strategy {
    @inline private implicit class DictionaryExt(val self: Dictionary[JAny]) extends AnyVal {
      @inline def updated(key: String, value: JAny): Dictionary[JAny] = {
        self.update(key, value)
        self
      }
    }

    def pattern(pattern: String, flags: String = "", index: UndefOr[Int] = undefined): B1 =
      regexp(new RegExp(pattern, flags), index)

    def regexp(r: RegExp, index: UndefOr[Int] = undefined): B1 = {
      val d = Dictionary.empty[JAny].updated("match", r: MatchType)
      index.foreach(d.update("index", _))
      new B1(d)
    }

    def apply(f: String => RegExp, index: UndefOr[Int] = undefined): B1 = {
      val d = Dictionary.empty[JAny].updated("match", f: MatchFn)
      index.foreach(d.update("index", _))
      new B1(d)
    }

    final class B1(val o: Dictionary[JAny]) extends AnyVal {
      def apply  [A](f: SearchFn[A])                                  : B2[A] = new B2[A](o.updated("search", f))
      def search2[A](f: (String, Callback[A])                 => Unit): B2[A] = apply(TextComplete search2 f)
      def search3[A](f: (String, Callback[A], JArray[String]) => Unit): B2[A] = apply(TextComplete search3 f)
      def search [A](f: String => Seq[A])                             : B2[A] = search2((t, c) => c(JArray(f(t): _*)))
    }

    final class B2[A](val o: Dictionary[JAny]) extends AnyVal {
      def apply    (f: ReplaceFn[A])                  : B3[A] = new B3(o.updated("replace", f))
      def replace  (f: A => String)                   : B3[A] = replaceE((a, _) => f(a))
      def replaceE (f: (A, Event) => String)          : B3[A] = apply(TextComplete replace1 f)
      def replace2 (f: A => (String, String))         : B3[A] = replaceE2((a, _) => f(a))
      def replaceE2(f: (A, Event) => (String, String)): B3[A] = apply(TextComplete replace2 f)
    }

    final class B3[A](val o: Dictionary[JAny]) extends AnyVal {
      def update(key: String, value: JAny): B3[A] = {
        o.update(key, value)
        this
      }

      def index     (i: Int                  ): B3[A] = update("index",      i)
      def cache     (i: Boolean              ): B3[A] = update("cache",      i)
      def template  (i: (A, String) => String): B3[A] = update("template",   i: JFn2[A, String, String])
      def contextB  (i: A => Boolean         ): B3[A] = update("context",    i: JFn1[A, Boolean])
      def contextS  (i: A => String          ): B3[A] = update("context",    i: JFn1[A, String])
      def contextR  (i: A => RegExp          ): B3[A] = update("context",    i: JFn1[A, RegExp])
      def idProperty(i: String               ): B3[A] = update("idProperty", i)

      @inline def result: StrategyA[A] =
        o.asInstanceOf[StrategyA[A]]
    }

    @inline implicit def autoResultFromB3[A](b: B3[A]): StrategyA[A] = b.result
  }

  type Strategies = JArray[Strategy]

  @inline implicit def autoSingletonStrategy[A](s: StrategyA[A]): Strategies = {
    val a: Strategies = new JArray(1)
    a(0) = s
    a
  }

  @inline def Strategies(ss: Strategy*): Strategies =
    JArray(ss: _*)

  type JQuerySel = Dynamic

//  def apply(target: JQuerySel, strategy: Strategy[_]): JQuerySel =
//    apply(target, JArray(strategy))
//
//  def apply(target: JQuerySel, strategy: Strategy[_], options: UndefOr[Options]): JQuerySel =
//    apply(target, JArray(strategy), options)

  def apply(target: JQuerySel, strategies: Strategies): JQuerySel =
    target.textcomplete(strategies)

  def apply(target: JQuerySel, strategies: Strategies, options: UndefOr[Options]): JQuerySel =
    target.textcomplete(strategies, options)

  /** If you want to "stop autocompleting". */
  def destroy(target: JQuerySel): JQuerySel =
    target.textcomplete("destroy")

  /** Fired with the selected value when a dropdown is selected. */
  def onSelect(target: JQuerySel, f: (Event, String, Strategy) => Unit): JQuerySel =
    target.on(Dynamic.literal(eventSelect -> (f: JFn3[Event, String, Strategy, Unit])))

  /** Fired with the selected value when a dropdown is selected. */
  def onSelect(target: JQuerySel, f: (Event, String) => Unit): JQuerySel =
    target.on(Dynamic.literal(eventSelect -> (f: JFn2[Event, String, Unit])))

  /** Fired with the selected value when a dropdown is selected. */
  def onSelect(target: JQuerySel, f: (Event) => Unit): JQuerySel =
    target.on(Dynamic.literal(eventSelect -> (f: JFn1[Event, Unit])))

  /** Fired with the selected value when a dropdown is selected. */
  def onSelect(target: JQuerySel)(f: => Unit): JQuerySel =
    target.on(Dynamic.literal(eventSelect -> ((() => f): JFn0[Unit])))

  /** Fired when a dropdown is shown. */
  def onShow(target: JQuerySel, f: Event => Unit): JQuerySel =
    target.on(Dynamic.literal(eventShow -> (f: JFn1[Event, Unit])))

  /** Fired when a dropdown is shown. */
  def onShow(target: JQuerySel)(f: => Unit): JQuerySel =
    target.on(Dynamic.literal(eventShow -> ((() => f): JFn0[Unit])))

  /** Fired when a dropdown is hidden. */
  def onHide(target: JQuerySel, f: Event => Unit): JQuerySel =
    target.on(Dynamic.literal(eventHide -> (f: JFn1[Event, Unit])))

  /** Fired when a dropdown is hidden. */
  def onHide(target: JQuerySel)(f: => Unit): JQuerySel =
    target.on(Dynamic.literal(eventHide -> ((() => f): JFn0[Unit])))

  // ===================================================================================================================
  // Additional niceties

  type Query[A] = String => Stream[A]

  /**
   * Prevents auto-complete when the search term is empty.
   * Prevents showing all options without criteria.
   *
   * Note that you can prevent this in your `match` regex.
   */
  def ignoreEmptyTerm[A](f: Query[A]): Query[A] =
    term =>
      if (term.isEmpty)
        Stream.empty
      else
        f(term)

  /**
   * Prevents auto-complete when the only result just what the user already has typed.
   */
  def ignorePerfectMatch[A](query: Query[A])(perfectMatch: (String, A) => Boolean): Query[A] =
    term => {
      val r = query(term)
      if (r.lengthCompare(1) == 0 && perfectMatch(term, r.head))
        Stream.empty
      else
        r
    }

  def ignorePerfectMatchStr(query: Query[String]): Query[String] =
    ignorePerfectMatch(query)(_ == _)

  /**
   * Normalises term and options before comparison.
   *
   * @param options Pre-sorted options.
   */
  def normalisedStringQuery[A](norm: String => String, cmp: (String, String) => Boolean, options: Stream[String]): Query[String] = {
    val os = options.map(s => (norm(s), s))
    term => {
      val t2 = norm(term)
      os.filter(o => cmp(o._1, t2)).map(_._2)
    }
  }

  /**
   * Matches options containing the search string, where case is ignored.
   *
   * @param options Pre-sorted options.
   */
  def caseInsensitiveContains(options: Stream[String]): Query[String] =
    ignorePerfectMatchStr(
      normalisedStringQuery(_.toLowerCase, _ contains _, options))

  /**
   * Matches options containing the search string, where case is ignored.
   *
   * @param options Pre-sorted options.
   */
  def caseInsensitiveStartsWith(options: Stream[String]): Query[String] =
    ignorePerfectMatchStr(
      normalisedStringQuery(_.toLowerCase, _ startsWith _, options))
}
