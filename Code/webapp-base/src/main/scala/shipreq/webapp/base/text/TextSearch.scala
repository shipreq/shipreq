package shipreq.webapp.base.text

import scala.collection.immutable.IntMap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import TextSearch.{apply => _, _}

object TextSearch {

  def apply(project: Project,  plainText: PlainText.ForProject): TextSearch =
    new TextSearch(project, plainText)

  type Normaliser = String => Normalised

  val whitespace = "\\s+".r

  val defaultNormaliser: Normaliser =
    s => {
      val a = whitespace.replaceAllIn(s, " ").toCharArray
      var i = a.length
      while (i > 0) {
        i -= 1
        val c = a(i)
        // Simple make lowercase
        if (c >= 'A' && c <= 'Z')
          a(i) = (c + 32).toChar
      }
      new Normalised(a)
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
        var start = 0
        while(n - start >= m) {
          var index = `m - 1`
          var ch = txt.charAt(start + index)
          while (ch == pat.charAt(index)) {
            if (index == 0)
              return true
            index -= 1
            ch = txt.charAt(start + index)
          }
          start += badCharShift(ch)
        }
      }
      false
    }
  }
}

final class TextSearch(project: Project,  plainText: PlainText.ForProject) {

  type Index = Stream[(Normalised, Req)]

  private def mkIndex(norm: Normaliser): Index = {
    val each: Req => Normalised = r => {
      val title = plainText.reqTitle(r)
      val str =
        project.customTextFields.foldLeft(title)((q, f) =>
          plainText.customTextField(f)(r.id).fold(q)(q + " " + _))
      norm(str)
    }
    project.reqs.data.reqs.vstream(_ mapStrengthL each)
  }

  private val index = mkIndex(defaultNormaliser)

  def search(substr: String): Stream[Req] = {
    // Don't parse search string. Later accept a search AST.
    //whitespace.split(substr).filter(_.nonEmpty)

    val matches =
      if (substr.isEmpty)
        index
      else {
        val algo = new BoyerMooreHorspool(defaultNormaliser(substr))
        index.filter(t => algo.search(t._1))
      }

    matches.map(_._2)
  }
}