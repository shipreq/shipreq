package japgolly.scalajs.jquery

import scalajs.js.{Function0 => JFn0, Function1 => JFn1, Function2 => JFn2, Function3 => JFn3, _}
import scalajs.js.{Any => JAny, Array => JArray, _}
import org.scalajs.dom.Event

object TextComplete {

  final val eventSelect = "textComplete:select"
  final val eventShow   = "textComplete:show"
  final val eventHide   = "textComplete:hide"

//  type Matcher   = RegExp // matchRegExpOrFunc
//  type SearchFn  = JFn3[String, Callback, JArray[String], Unit]
//  type ReplaceFn = JFn1[String, JAny]

  type Matcher   = RegExp // matchRegExpOrFunc
  sealed trait SearchFn extends JAny
  type ReplaceFn = JFn1[String, JAny]

  type Strategies = JArray[Strategy]

  sealed trait Strategy extends Object {
    var `match`: RegExp     = native
    var search : SearchFn   = native
    var replace: ReplaceFn  = native

    var index     : UndefOr[Int]                  = native
    var template  : UndefOr[JFn1[String, String]] = native
    var cache     : UndefOr[Boolean]              = native
    var context   : UndefOr[JFn1[String, JAny]]   = native
    var idProperty: UndefOr[String]               = native
  }
  
  sealed trait Callback extends JAny {
    def apply(result: JArray[String], stillSearching: Boolean = false): Unit = native
  }

  trait StrOrFunc extends JAny

  def Options: Options = (new Object).asInstanceOf[Options]
  sealed trait Options extends Object {
    var appendTo:  JAny               = native  // $('body')
    var height:    UndefOr[Int]       = native  // undefined
    var maxCount:  Int                = native  // 10
    var placement: String             = native  // ''
    var header:    UndefOr[StrOrFunc] = native  // undefined
    var footer:    UndefOr[StrOrFunc] = native  // undefined
    var zIndex:    String             = native  // '100'
    var debounce:  UndefOr[Int]       = native  // undefined. (milliseconds)
    var adapter:   UndefOr[JAny]      = native  // undefined
    var className: String             = native  // ''
  }

  @inline private implicit class DictionaryExt(val self: Dictionary[JAny]) extends AnyVal {
    @inline def updated(key: String, value: JAny): Dictionary[JAny] = {
      self.update(key, value)
      self
    }
  }

  def search(f: (String, Callback) => Unit): SearchFn =
    (f: JFn2[String, Callback, Unit]).asInstanceOf[SearchFn]

  def search(f: (String, Callback, JArray[String]) => Unit): SearchFn =
    (f: JFn3[String, Callback, JArray[String], Unit]).asInstanceOf[SearchFn]

  def replace(f: String => String): ReplaceFn =
    (f: JFn1[String, String]).asInstanceOf[ReplaceFn]

  def replace2(f: String => (String, String)): ReplaceFn = {
    val f2 = (i: String) => {
      val r = f(i)
      JArray(r._1, r._2)
    }
    (f2: JFn1[String, JArray[String]]).asInstanceOf[ReplaceFn]
  }

  object Strategy {
    def apply(r: RegExp)                          : B1 = new B1(Dictionary.empty[JAny].updated("match", r))
    def apply(pattern: String, flags: String = ""): B1 = apply(new RegExp(pattern, flags))

    final class B1(val o: Dictionary[JAny]) extends AnyVal {
      def search(f: SearchFn                                  ): B2 = new B2(o.updated("search", f))
      def search(f: (String, Callback)                 => Unit): B2 = search(TextComplete search f)
      def search(f: (String, Callback, JArray[String]) => Unit): B2 = search(TextComplete search f)
    }

    final class B2(val o: Dictionary[JAny]) extends AnyVal {
      def replaceF(f: ReplaceFn)                 : B3 = new B3(o.updated("replace", f))
      def replace (f: String => String)          : B3 = replaceF(TextComplete replace f)
      def replace2(f: String => (String, String)): B3 = replaceF(TextComplete replace2 f)
    }

    final class B3(val o: Dictionary[JAny]) extends AnyVal {
      def update(key: String, value: JAny): B3 = {
        o.update(key, value)
        this
      }

      def index     (i: Int              ): B3 = update("index",      i)
      def cache     (i: Boolean          ): B3 = update("cache",      i)
      def template  (i: String => String ): B3 = update("template",   i: JFn1[String, String])
      def contextB  (i: String => Boolean): B3 = update("context",    i: JFn1[String, Boolean])
      def contextS  (i: String => String ): B3 = update("context",    i: JFn1[String, String])
      def idProperty(i: String           ): B3 = update("idProperty", i)

      @inline def result: Strategy =
        o.asInstanceOf[Strategy]
    }

    @inline implicit def autoResultFromB3(b: B3): Strategy = b.result
  }

  type JQuerySel = Dynamic

  def apply(target: JQuerySel, strategy: Strategy): JQuerySel =
    apply(target, JArray(strategy))

  def apply(target: JQuerySel, strategy: Strategy, options: UndefOr[Options]): JQuerySel =
    apply(target, JArray(strategy), options)

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

  def ignoreUnhelpful(f: String => Stream[String], allowEmptyTerm: Boolean): String => JArray[String] =
    term =>
      if (!allowEmptyTerm && term.isEmpty)
        new JArray(0)
      else {
        val r = f(term)
        if (r.lengthCompare(1) == 0 && r.head == term)
          new JArray(0)
        else
          JArray(r: _*)
      }

  def search(f: String => Stream[String], allowEmptyTerm: Boolean): SearchFn = {
    val g = ignoreUnhelpful(f, allowEmptyTerm)
    search((term, c) => c(g(term)))
  }

}
