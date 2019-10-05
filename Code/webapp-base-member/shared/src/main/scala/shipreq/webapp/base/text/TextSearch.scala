package shipreq.webapp.base.text

import scala.collection.immutable.IntMap
import scalaz.Need
import shipreq.base.util.{IMap, OptionalBoolFn}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._

object TextSearch {

  def apply(project: Project,  plainText: PlainText.ForProject.NoCtx): TextSearch =
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

  final case class IndexEntryFilter(req        : OptionalBoolFn[IndexEntryR],
                                    codeGroup  : OptionalBoolFn[IndexEntryG],
                                    manualIssue: OptionalBoolFn[IndexEntryM],
                                   ) {

    def &&(that: IndexEntryFilter): IndexEntryFilter =
      IndexEntryFilter(
        req && that.req,
        codeGroup && that.codeGroup,
        manualIssue && that.manualIssue,
      )
  }

  private def IEF(req        : IndexEntryR => Boolean,
                  codeGroup  : IndexEntryG => Boolean = null,
                  manualIssue: IndexEntryM => Boolean = null): IndexEntryFilter =
    IndexEntryFilter(
      OptionalBoolFn(Option(req)),
      OptionalBoolFn(Option(codeGroup)),
      OptionalBoolFn(Option(manualIssue)),
    )

  private type SearchFn = BoyerMooreHorspool => IndexEntryFilter

  private val searchAll: SearchFn =
    a => IEF(
      e => a.search(e.title) || a.search(e.textFields),
      _.title |> a.search,
      _.title |> a.search,
    )

  private val searchTitles: SearchFn =
    a => IEF(
      _.title |> a.search,
      _.title |> a.search,
      _.title |> a.search,
    )

  // Indexes

  final case class IndexEntryG(group: CodeGroup, title: Normalised)
  final case class IndexEntryR(req: Req, title: Normalised, textFields: Need[Normalised])
  final case class IndexEntryM(issue: ManualIssue, title: Normalised)

  final class Index private[TextSearch](norm    : Normaliser,
                                        indexR  : IMap[ReqId,         IndexEntryR],
                                        indexG  : IMap[ReqCodeId,     IndexEntryG],
                                        indexM  : IMap[ManualIssueId, IndexEntryM],
                                        filter  : Option[IndexEntryFilter],
                                        searchFn: SearchFn) {

    private def newFilter(f: IndexEntryFilter): IndexEntryFilter =
      filter.fold(f)(_ && f)

    private def withNewFilter(f: IndexEntryFilter): Index =
      new Index(norm, indexR, indexG, indexM, Some(newFilter(f)), searchFn)

    def filterReq(f: Req => Boolean): Index =
      withNewFilter(IEF(_.req |> f))

    def filterReqsIds(ids: Set[ReqId]): Index =
      filterReq(ids contains _.id)

    def titlesOnly: Index =
      new Index(norm, indexR, indexG, indexM, filter, searchTitles)

    private def search[A](substr: String, s: IndexEntryFilter => A)(matchEverything: => A): A =
      if (substr.isEmpty)
        matchEverything
      else {
        val algo = new BoyerMooreHorspool(norm(substr))
        val f = newFilter(searchFn(algo))
        s(f)
      }

    case class SearchFilter(req        : OptionalBoolFn[ReqId],
                            codeGroup  : OptionalBoolFn[ReqCodeId],
                            manualIssue: OptionalBoolFn[ManualIssueId])
    object SearchFilter {
      val empty = apply(OptionalBoolFn.empty, OptionalBoolFn.empty, OptionalBoolFn.empty)
    }

    def searchFilter(substr: String): SearchFilter =
      search(substr, i => SearchFilter(
        req         = i.req        .map[ReqId]        (f => indexR.get(_) exists f),
        codeGroup   = i.codeGroup  .map[ReqCodeId]    (f => indexG.get(_) exists f),
        manualIssue = i.manualIssue.map[ManualIssueId](f => indexM.get(_) exists f),
      ))(SearchFilter.empty)

    def searchAll(substr: String): Stream[Req] = {
      // whitespace.split(substr).filter(_.nonEmpty)
      def all = indexR.values.toStream
      search(substr, all filter _.req.toFn)(all).map(_.req)
    }
  }

}

final class TextSearch(project: Project,  plainText: PlainText.ForProject.NoCtx) {
  import TextSearch.{apply => _, _}

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
      project.content.reqs.reqIterator map each
    }

    def indexValuesG: Iterator[IndexEntryG] = {
      def each(g: CodeGroup): IndexEntryG = {
        val title = norm(plainText codeGroupTitle g)
        IndexEntryG(g, title)
      }
      project.content.reqCodes.groups.iterator map each
    }

    def indexValuesM: Iterator[IndexEntryM] = {
      def each(i: ManualIssue): IndexEntryM = {
        val title = norm(plainText.manualIssue(i.text))
        IndexEntryM(i, title)
      }
      project.manualIssues.imap.valuesIterator map each
    }

    val indexR = IMap.empty[ReqId,         IndexEntryR](_.req.id)   ++ indexValuesR
    val indexG = IMap.empty[ReqCodeId,     IndexEntryG](_.group.id) ++ indexValuesG
    val indexM = IMap.empty[ManualIssueId, IndexEntryM](_.issue.id) ++ indexValuesM

    new Index(norm, indexR, indexG, indexM, None, searchAll)
  }

  val ignoreCaseSingleSpaces = index(Normaliser.ignoreCaseSingleSpaces)
  val ignoreCaseNoWhitespace = index(Normaliser.ignoreCaseNoWhitespace)
}
