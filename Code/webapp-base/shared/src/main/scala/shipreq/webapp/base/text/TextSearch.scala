package shipreq.webapp.base.text

import scala.collection.immutable.IntMap
import scalaz.Need
import shipreq.base.util.{FilterFn, FilterFn2, IMap}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import TextSearch.{apply => _, _}

object TextSearch {

  def apply(project: Project,  plainText: PlainText.ForProject): TextSearch =
    new TextSearch(project, plainText)

  // ===================================================================================================================

  type Normaliser = String => Normalised

  object Normaliser {
    val whitespace = "\\s+".r

    val ignoreCaseSingleSpaces: Normaliser = s => {
      val a = whitespace.replaceAllIn(s, " ").toCharArray
      mkLowerCase(a)
      new Normalised(a)
    }

    val ignoreCaseNoWhitespace: Normaliser = s => {
      val a = whitespace.replaceAllIn(s, "").toCharArray
      mkLowerCase(a)
      new Normalised(a)
    }

    def mkLowerCase(a: Array[Char]): Unit = {
      var i = a.length
      while (i > 0) {
        i -= 1
        val c = a(i)
        // Simple make lowercase
        if (c >= 'A' && c <= 'Z')
          a(i) = (c + 32).toChar
      }
    }
  }

  final class Normalised(val data: Array[Char]) extends AnyVal {
    @inline def length        : Int  = data.length
    @inline def charAt(i: Int): Char = data(i)
  }

  final val CharArrayCutoff = 127

  final class CharToInt(default: Int) {
    private[this] val array = Array.fill(CharArrayCutoff)(default)
    private[this] var map   = IntMap.empty[Int]

    def apply(ch: Char): Int = {
      val i = ch.toInt
      if (i < CharArrayCutoff)
        array(i)
      else
        map.getOrElse(i, default)
    }

    def update(ch: Char, v: Int): Unit = {
      val i = ch.toInt
      if (i < CharArrayCutoff)
        array(i) = v
      else
        map = map.updated(i, v)
    }
  }

//  /**
//   * Knuth-Morris-Pratt DFA substring search.
//   *
//   * "Algorithms, Fourth Edition", pg. 768.
//   */
//  final class KMP(pat: Normalised) {
//    private val M = pat.length
//
//    private val emptyV = IntMap.empty[Int]
//    private val data = Array.fill(M)(emptyV)
//    private def get(ch: Char, pos: Int): Int =
//      data(pos).getOrElse(ch, 0)
//
//    @inline private def init(): Unit = {
//      def set(ch: Char, pos: Int)(v: Int): Unit = {
//        val m = data(pos)
//        data(pos) = m.updated(ch, v)
//      }
//      set(pat.charAt(0), 0)(1)
//      var x = 0
//      var j = 1
//      while (j < M) {
//        data(j) = data(x)
//        val ch = pat.charAt(j)
//        set(ch, j)(j + 1)
//        x = get(ch, x)
//        j += 1
//      }
//    }
//    init()
//
//    def search(txt: Normalised): Boolean = {
//      val N = txt.length
//      var i = 0
//      var j = 0
//      while (i < N && j < M) {
//        j = get(txt charAt i, j)
//        i += 1
//      }
//      j == M
//    }
//  }

  /**
   * Boyer-Moore-Horspool.
   *
   * It is a simplification of the Boyer–Moore string search algorithm which is related to the Knuth–Morris–Pratt algorithm.
   * The algorithm trades space for time in order to obtain an average-case complexity of O(N) on random text,
   * although it has O(MN) in the worst case, where the length of the pattern is M and the length of the search string is N.
   */
  final class BoyerMooreHorspool(pat: Normalised) {
    private val m = pat.length
    private val `m - 1` = m - 1

    private val badCharShift = new CharToInt(m)
    @inline private def init(): Unit = {
      var i = 0
      while (i < `m - 1`) {
        badCharShift(pat charAt i) = `m - 1` - i
        i += 1
      }
    }
    init()

    def search(txt: Normalised): Boolean = {
      val n = txt.length
      if (n > 0) {
        val stop = n - m
        var start = 0
        while (start <= stop) {
          var index = `m - 1`
          while (txt.charAt(start + index) == pat.charAt(index)) {
            if (index == 0)
              return true
            index -= 1
          }
          start += badCharShift(txt.charAt(start + `m - 1`))
        }
      }
      false
    }
  }

  // ===================================================================================================================

  @inline private implicit def autoNeedValue[A](n: Need[A]): A = n.value

  type IndexEntryFilter = FilterFn2[IndexEntryR, IndexEntryG]
  @inline private def IEF(a: IndexEntryR => Boolean, b: IndexEntryG => Boolean): IndexEntryFilter = FilterFn2(a, b)
  import FilterFn.`n/a`

  private type SearchFn = BoyerMooreHorspool => IndexEntryFilter

  private val searchAll: SearchFn =
    a => FilterFn2(e => a.search(e.title) || a.search(e.textFields), _.title |> a.search)

  private val searchTitles: SearchFn =
    a => FilterFn2(_.title |> a.search, _.title |> a.search)

  // Indexes

  case class IndexEntryG(group: ReqCodeGroup, title: Normalised)
  case class IndexEntryR(req: Req, title: Normalised, textFields: Need[Normalised])

  final class Index private[TextSearch](norm    : Normaliser,
                                        indexR  : IMap[ReqId,     IndexEntryR],
                                        indexG  : IMap[ReqCodeId, IndexEntryG],
                                        filter  : Option[IndexEntryFilter],
                                        searchFn: SearchFn) {

    private def newFilter(f: IndexEntryFilter): IndexEntryFilter =
      filter.fold(f)(_ && f)

    private def withNewFilter(f: IndexEntryFilter): Index =
      new Index(norm, indexR, indexG, Some(newFilter(f)), searchFn)

    def filterReq(f: Req => Boolean): Index =
      withNewFilter(IEF(_.req |> f, `n/a`))

    def filterReqsIds(ids: Set[ReqId]): Index =
      filterReq(ids contains _.id)

    def titlesOnly: Index =
      new Index(norm, indexR, indexG, filter, searchTitles)

    private def search[A](substr: String, matchEverything: => A)(s: IndexEntryFilter => A): A =
      if (substr.isEmpty)
        matchEverything
      else {
        val algo = new BoyerMooreHorspool(norm(substr))
        val f = newFilter(searchFn(algo))
        s(f)
      }

    def searchFilter(substr: String): FilterFn2[ReqId, ReqCodeId] =
      search(substr, FilterFn2[ReqId, ReqCodeId](`n/a`, `n/a`))(f =>
        FilterFn2[ReqId, ReqCodeId](
          indexR.get(_) exists f.a,
          indexG.get(_) exists f.b))

    def searchAll(substr: String): Stream[Req] = {
      // whitespace.split(substr).filter(_.nonEmpty)
      def all = indexR.values.toStream
      search(substr, all)(all filter _.a).map(_.req)
    }
  }

}

final class TextSearch(project: Project,  plainText: PlainText.ForProject) {

  private def index(norm: Normaliser): Index = {

    def indexValuesR: Iterator[IndexEntryR] = {
      def each(r: Req): IndexEntryR = {
        val title      = norm(plainText reqTitle r)
        val textFields = Need(norm(
          project.config.liveCustomTextFields.foldLeft("")((q, f) =>
            plainText.customTextField(f.id)(r).fold(q)(q + "\n" + _))
        ))
        IndexEntryR(r, title, textFields)
      }
      project.reqs.reqIterator map each
    }

    def indexValuesG: Iterator[IndexEntryG] = {
      def each(g: ReqCodeGroup): IndexEntryG = {
        val title = norm(plainText reqCodeGroupTitle g)
        IndexEntryG(g, title)
      }
      project.reqCodes.groups.iterator map each
    }

    val indexR = IMap.empty[ReqId,     IndexEntryR](_.req.id)   ++ indexValuesR
    val indexG = IMap.empty[ReqCodeId, IndexEntryG](_.group.id) ++ indexValuesG

    new Index(norm, indexR, indexG, None, searchAll)
  }

  val ignoreCaseSingleSpaces = index(Normaliser.ignoreCaseSingleSpaces)
  val ignoreCaseNoWhitespace = index(Normaliser.ignoreCaseNoWhitespace)
}
