package shipreq.webapp.base.data.deletion

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.nonempty.NonEmptySet
import nyaya.gen._
import nyaya.prop._
import scalaz.std.set.setInstance
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.TestOptics
import shipreq.webapp.base.test.WebappTestUtil._

/**
  *
  * @param mode Mode being tested
  * @param projectFree Any random project
  * @param projectBasic A project with max 1 imp/req, and everything is mode.from
  */
final class DeleRestProps(mode: Mode,
                          projectFree: Project,
                          projectBasic: Project) {
  import DeleRestProps._

  private def invariants(T: Results) = {
    import T._

//    println("="*120)
//    println(s"Inputs = $input")
//    println(s"Auto   = $auto")
//    println(s"Extra  = $extra")
//    println("Before:")
//    println(p.prettyPrintImplicationGraph)
//    println()

    def allLive =
      E.forall(all)(id =>
        E.equal(s"$id.live", p.content.reqs.need(id).live(p.config.reqTypes), mode.from))

    def extraSubsetOfOptional =
      E.whitelist("extra ⊆ optional", result.optionalReqIds, extra)

    def extraImpliedBySomethingVisible =
      E.forall(extra)(id =>
        E.test(s"$id is implied by something visible",
          p.content.implications.backwards(id).exists(all.contains)))

    def extraTransitivelyImpliedByInputs =
      E.forall(extra)(id =>
        E.test(s"$id is implied by something visible",
          p.implicationTgtToSrcTC(id).exists(parent => input.contains(parent))))

    def ifAutosActioned: EvalL =
      mode.perform(autoAndInput).map { event =>
        val p2 = applyEventSuccessfully(p, event)

        // deleted have no live imps (live imps would make deletion necessity ambiguous)
        // restored have no dead imps (dead imps would make restoration necessity ambiguous)
        E.forall(auto)(id =>
          E.test(s"Everything that implies $id is now ${mode.to}",
            p2.content.implications.backwards(id).forall(parent =>
              p2.content.reqs.need(parent).live(p2.config.reqTypes) ==* mode.to)))

      }.getOrElse(E.pass).rename("ifAutosActioned")

    def ifAllActioned: EvalL =
      mode.perform(all).map { event =>
        val p2 = applyEventSuccessfully(p, event)

//        println("="*120)
//        println("After All: " + event)
//        println(p2.prettyPrintImplicationGraph)
//        println()

        // TODO restored have dead imps (is this always true? children of this case might not hold)

        // inputs have no dead imps
        E.forall(input.whole)(id =>
          E.test(s"All transitive implications of input $id are now ${mode.to}",
            p2.implicationSrcToTgtTC(id).forall(child =>
              p2.content.reqs.need(child).live(p2.config.reqTypes) ==* mode.to)))

      }.getOrElse(E.pass).rename("ifAllActioned")

    (allLive
      & extraSubsetOfOptional
      & extraImpliedBySomethingVisible
      & extraTransitivelyImpliedByInputs
      & ifAutosActioned
      & ifAllActioned
      ) rename "invariants"
  }

  private def free(input: NonEmptySet[ReqId]) =
    invariants(new Results(mode, input, projectFree))
      .rename(s"free(${input.whole.map(_.value).mkString(",")})")

  private def basic(_input: NonEmptySet[ReqId]) = {
    val T = new Results(mode, _input, projectBasic)
    import T._

    def noneOff =
      E.test("Off should be empty", off.isEmpty)

    def autoIsEverythingImplied =
      E.equal("auto = TC", auto,
        input.iterator.map(p.implicationSrcToTgtTC(_)).reduce(_ ++ _) -- input.whole)

      // TODO Pending: Restore
//    def performReverse = {
//      val apply   = mode.perform(result.initialReqs).get
//      val p2      = applyEventSuccessfully(p, apply)
//      val t2      = new Results(!mode, input, p2)
//      val reverse = (!mode).perform(t2.result.initialReqs).get
//      val p3      = applyEventSuccessfully(p2, reverse)
//      def norm(p: Project) = p.content.copy(deletionReasons = DeletionReasons.empty)
//
//      println("=" * 120)
//      println("Apply: " + apply)
//      println("Reverse: " + reverse)
//      println(p3.prettyPrintImplicationGraph)
//      println()
//
//      E.equal("apply.reverse = id", norm(p3), norm(p))
//      E.equal("apply.reverse = id", true, false)
//    }

    (invariants(T) & noneOff & autoIsEverythingImplied ) // TODO & performReverse)
      .rename(s"basic(${input.whole.map(_.value).mkString(",")})")
  }

  def allProps =
    testWithInputs(projectBasic, _ => true, basic).rename("projectBasic") &
      testWithInputs(projectFree, _.live(projectFree.config.reqTypes) is mode.from, free).rename("projectFree")
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object DeleRestProps {

  final class Results(mode: Mode, val input: NonEmptySet[ReqId], val p: Project) {
    val E = EvalOver(input)

    val result: DeletionLogic.Data =
      DeletionLogic.Data.forReqs(p, input)

    val auto: Set[ReqId] =
      result.initialReqs -- input.whole

    val off: Set[ReqId] =
      result.deletableReqs.iterator.map(_.req.id).filterNot(auto.contains).toSet -- input.whole

    val extra: Set[ReqId] =
      auto ++ off

    val all: Set[ReqId] =
      extra ++ input.whole

    val autoAndInput: Set[ReqId] =
      auto ++ input.whole
  }

  def chooseInputs(p: Project, f: Req => Boolean): List[NonEmptySet[ReqId]] = {
    val ids     = p.content.reqs.reqIterator.filter(f).map(_.id).toList
    val idCount = ids.size
    val singles = ids.take(20).map(NonEmptySet one _)
    val pairs   = ids.combinations(2).map(NonEmptySet force _.toSet).take(6).toList // TODO THIS IS REALLY SLOW
    var l = singles ::: pairs
    if (idCount > 4)
      l :::= ids.combinations(idCount >> 1).map(NonEmptySet force _.toSet).take(6).toList // TODO THIS IS REALLY SLOW
    l
  }

  def testWithInputs(p: Project, f: Req => Boolean, t: NonEmptySet[ReqId] => EvalL): EvalL =
    EvalOver(p).forall(chooseInputs(p, f))(t)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class RandomData(mode: Mode) {
    private val * = shipreq.webapp.base.RandomData

    /** A project with max 1 imp/req, and everything is mode.from */
    val genProjectBasic: Gen[Project] =
      for {
        reqtypes1 ← *.customReqTypes
        reqtypes2 = ReqTypes.empty.custom ++ reqtypes1.valuesIterator.map(_.copy(live = Live))
        config    = ProjectConfig.empty.copy(reqTypes = ReqTypes(reqtypes2))
        reqCount  ← Gen.chooseInt(40)
        ucCount   ← Gen.chooseSize map (_ >> 1)
        reqs1     ← *.reqsWithoutText(config, reqCount, ucCount)
        reqs2     = (TestOptics.grsLive.set(mode.from) compose TestOptics.ucsLive.set(mode.from))(reqs1)
        imps1     ← *.reqFieldDataImplications(reqs2.idIterator.toSet)
        imps2     = imps1.forwards.m.mapValuesNow(_.take(1))
        imps3     = Implications.BiDir(Implications.UniDir(imps2).reverse) // reverse ensures take(1) is on parent side
        content   = ProjectContent.empty.copy(reqs = reqs2, implications = imps3)
        project   = Project.empty.copy(config = config, content = content)
      } yield IdCeilings.supply(ic => project.copy(idCeilings = ic))

    val genProjectFree: Gen[Project] =
      *.project

    val genProps: Gen[DeleRestProps] =
      for {
        basic <- genProjectBasic
        free  <- genProjectFree
      } yield new DeleRestProps(mode, projectFree = free, projectBasic = basic)
  }
}
