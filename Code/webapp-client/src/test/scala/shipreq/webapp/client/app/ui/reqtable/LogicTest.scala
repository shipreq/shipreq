package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.util.Multimap
import scalaz.{\/, \/-, -\/, Equal}
import scalaz.std.AllInstances._
import scalaz.syntax.id._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.client.test.ClientTestSettings._
import SortMethod.{AscHalf, ConsiderBlanks, AscThenBlanks, BlanksThenAsc, Asc}

object LogicTest extends TestSuite {

//  private sealed trait ExpType[A] {
//    def visible(vs: ViewSettings): Boolean
//    def extract(e: Expansion): List[A]
//  }
//  private case object ExpCodes extends ExpType[ReqCode] {
//    override def visible(vs: ViewSettings) = vs isVisible Column.Code
//    override def extract(e: Expansion) = e.reqCodes
//  }
//
//  private val exptypes = List(ExpCodes)

  private def codes(r: Row): List[ReqCode] = r match {
    case g: GenericReqRow => g.exp.reqCodes
  }

  def firstCodePerRow(r: Row): String =
    codes(r) match {
      case Nil    => ""
      case h :: _ => h.txt
    }

  case class LogicTests(vs: ViewSettings, p: Project) {
    val E = EvalOver(this)

    val gathered    = Logic.gather(vs, p)
    val gatheredG   = gathered.filterT[GenericReqRow]
    val rowReqCodes = gathered.flatMap(codes(_).toStream)
    val rowGReqIds  = gatheredG.map(_.req.id).toSet
    val srcGReqIds  = p.reqs.data.reqs.keys.filterT[GenericReq.Id].toSet
    val srcReqCodes = p.reqCodes.data.codeSet

    // -----------------------------------------------------------------------------------------------------------------
    // Gathering

    def noEmptyAndNonEmptyReqCodesMixed = {
      val data: Stream[List[List[ReqCode]]] =
        Multimap.empty[Req.Id, List, List[ReqCode]]
          .addPairs(gatheredG.map(r => (r.req.id, r.exp.reqCodes)): _*)
          .m.values.toStream
      E.forall(data)(l =>
        E.test("Either all empty or all non-empty", !(l.exists(_.isEmpty) && l.exists(_.nonEmpty))))
    }

    // TODO doesn't check expanded implications
    // expansions per expandable A
    //   - list A = req.A
    //   - if req.A then no rows without As

    // NOTE: ReqCodes *can* be duplicated. Imagine sorting by MF > Code.
    def gather =
      ( E.distinct("Rows", gathered)
      ∧ E.allPresent("each generic req id has a row", srcGReqIds, rowGReqIds)
      ∧ E.allPresent("all req codes are displayed", srcReqCodes, rowReqCodes)
      ∧ noEmptyAndNonEmptyReqCodesMixed
      ) rename "Logic.gather"

    // -----------------------------------------------------------------------------------------------------------------
    // Sorting

    def anySort = {
      val sorted   = Logic.sort(vs.order, p, gathered)
      val reversed = Logic.sort(vs.order.reverse, p, gathered)
      ( E.equal("sort.sort = sort", Logic.sort(vs.order, p, sorted.toStream), sorted)
      ∧ E.equal("sort(criteria.reverse) = reverse(sort(cri))", reversed, sorted.reverse)
      ) rename "Any SortCriteria"
    }

    def sortCriICB(c: SortCriterion.InconclusiveCB): SortCriteria =
      this.vs.order.copy(init = Vector(c))

    def gatherOn(c: Column.SortInconclusive, sc: SortCriteria): Stream[Row] =
      if (vs isVisible c) gathered else Logic.gather(ViewSettings(Vector(c), sc), p)

    /** @return error \/ (blank, non-blank) */
    def separateBlanks[A](expectBlanksFirst: Boolean, as: List[A])(isBlank: A => Boolean): String \/ (List[A], List[A]) = as match {
      case Nil =>
        \/-(Nil, Nil)
      case h :: t =>
        val firstBlockBlank = isBlank(h)
        val b1Cond: A => Boolean = if (firstBlockBlank) isBlank else !isBlank(_)
        val block1 = h :: t.takeWhile(b1Cond)
        val block2 = as drop block1.length
        val (b,nb) = if (firstBlockBlank) (block1, block2) else (block2, block1)
        def show = {
          val (s1,s2) = (block1,block2).mapEach(_.map(a => if (isBlank(a)) "." else "#").mkString(""))
          s"[$s1|$s2]"
        }
        def fail(e: String) = -\/(s"$e: $show")
        if (block2.isEmpty)
          if (firstBlockBlank) \/-(block1, Nil) else \/-(Nil, block1)
        else if (block2 exists b1Cond)
          fail("Blank and non-blanks not separated")
        else if (expectBlanksFirst != firstBlockBlank)
          fail(s"Blocks in wrong order")
        else
          \/-(b, nb)
    }

    def E_bnbBlocks[A](name: String, expectBlanksFirst: Boolean, as: List[A])(isBlank: A => Boolean, f: (List[A], List[A]) => EvalL): EvalL =
      E.either(s"$name make separate blank/non-blank blocks", separateBlanks(expectBlanksFirst, as)(isBlank))(f.tupled)

    def E_sorted[A: Ordering: Equal](name: String, as: List[A]): EvalL =
      E.equal(name + " are sorted", as, as.sorted)

    def sortByPubid: EvalL = {
      def extract(pid: Pubid): (String, Int) = (p.reqType(pid.reqTypeId).fold(sys.error, _.mnemonic.value), pid.pos.value)
      val sc     = SortCriteria(Vector.empty, SortCriterion.Conclusive(Column.PubId, Asc))
      val sorted = Logic.sort(sc, p, gathered)
      val pubids = sorted.map { case r: GenericReqRow => extract(r.req.pubId)}
      E_sorted("Pubids", pubids)
    }

    def sortByRecCode(sm: ConsiderBlanks with AscHalf, blanksFirst: Boolean): EvalL = {
      val sc         = sortCriICB(SortCriterion.InconclusiveCB(Column.Code, sm))
      val input      = gatherOn(Column.Code, sc)
      val sorted     = Logic.sort(sc, p, input)
      val data       = sorted map firstCodePerRow
      val name       = s"ReqCodes ($sm)"
      val intra      = sorted.toStream.map(codes).filter{case _ :: _ :: _ => true; case _ => false}.map(_.map(_.txt))
      def eachRow    = E.forall(intra)(E_sorted(s"Codes within a single row are sorted.", _))
      def wholeTable = E_bnbBlocks(name, blanksFirst, data)(_.isEmpty, (_, nb) => E_sorted(name, nb))
      (wholeTable ∧ eachRow) rename name
    }

    def sortCB(t: (ConsiderBlanks with AscHalf, Boolean) => EvalL): EvalL =
       t(BlanksThenAsc, true) ∧ t(AscThenBlanks, false)

    def sorting =
      ( sortByPubid
      ∧ sortCB(sortByRecCode)
      //∧ sortCB(__) // TODO Sort by Desc
      //∧ sortCB(__) // TODO Sort by CustomTextField
      //∧ sortCB(__) // TODO Sort by ...
      ∧ anySort
      ) rename "Logic.sort"

    // -----------------------------------------------------------------------------------------------------------------
    def all = gather ∧ sorting
  }

  def gen: Gen[LogicTests] =
    for {
      p  <- RandomData.project
      vs <- ReqTableTest.rndViewSettings(p)
    } yield
      LogicTests(vs, p)

  override def tests = TestSuite {
    gen.mustSatisfyE(_.all) //(implicitly[Settings].setSeed(0).setDebug.setSampleSize(200))
  }
}
