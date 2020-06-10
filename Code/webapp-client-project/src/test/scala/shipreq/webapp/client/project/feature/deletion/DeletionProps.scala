package shipreq.webapp.client.project.feature.deletion

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.gen._
import nyaya.prop._
import scalaz.std.set.setInstance
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.event.Event.{ContentRestore, ReqsDelete}
import shipreq.webapp.base.test.TestOptics
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.Text

/**
  * @param mode Mode being tested
  * @param projectFree Any random project
  * @param projectBasic A project with max 1 imp/req, and everything is mode.from
  */
final class DeletionProps(mode: DeleteOrRestore,
                          projectFree: Project,
                          projectBasic: Project) {
  import DeletionProps._

  private def invariants(T: Results) = {
    import T._

//    println("="*120)
//    println(s"Inputs = $input")
//    println(s"Auto   = $auto")
//    println(s"Off    = $off")
//    println("Before:")
//    println(p.prettyPrintImplicationGraph)
//    println()

    def allLive =
      E.forall(all)(id =>
        E.equal(s"$id.live", p.content.reqs.need(id).live(p.config.reqTypes), mode.fromState))
        .rename(s"All actionable reqs should be ${mode.fromState}")

    def extraSubsetOfOptional =
      E.whitelist("extra ⊆ optional", result.optionalReqIds, extra)

    def extraImpliedBySomethingVisible =
      E.forall(extra)(id =>
        E.test(s"$id should be implied by something visible",
          p.content.implications.backwards(id).exists(all.contains)))
        .rename("All extra reqs should be implied by something visible")

    def extraTransitivelyImpliedByInputs =
      E.forall(extra)(id =>
        E.test(s"$id should be transitively implied by inputs",
          impTgtToSrcTC(id).exists(parent => input.contains(parent))))
        .rename("All extra reqs should be transitively implied by inputs")

    def ifAutosActioned: EvalL =
      perform(mode, autoAndInput).map { event =>
        val p2 = applyEventSuccessfully(p, event)

        // deleted have no live imps (live imps would make deletion necessity ambiguous)
        // restored have no dead imps (dead imps would make restoration necessity ambiguous)
        E.forall(auto)(id =>
          E.test(s"Everything that implies $id is now ${mode.toState}",
            p2.content.implications.backwards(id).forall(parent =>
              p2.content.reqs.need(parent).live(p2.config.reqTypes) ==* mode.toState)))

      }.getOrElse(E.pass).rename("ifAutosActioned")

    def ifAllActioned: EvalL =
      perform(mode, all).map { event =>
        val p2 = applyEventSuccessfully(p, event)

//        println("=" * 120)
//        println(s"Inputs = $input")
//        println(s"Auto   = $auto")
//        println(s"Off    = $off")
//        println(Util.sideBySideStrings(p.prettyPrintImplicationGraph, p2.prettyPrintImplicationGraph))
//        println()

        // Currently, Delete/Restore logic doesn't go through already deleted/restored stuff to find transitive eligible
        // candidates. This would require showing users a bunch of child reqs that are already in the target state and
        // can't be selected. Rather than complicate the UX, for now the delete form only shows deletable stuff.
        //
        // Example:
        // ================
        // Implication tree
        // ================
        // 41629!
        // . 29264
        // . . 21800
        // . . 26788
        // . . . 21800
        // . . . 30427!
        // ================
        // This only presents 41629 as a candidate for restoration, even though transitively 30427 could be considered
        // a candidate for restoration too.

        val impSrcToTgtTC2 =
          p2.content.implications.transitiveClosure(
            Forwards,
            p2.content.reqs.idIterator,
            id => p2.content.reqs.need(id).allowLiveChange(p2.config.reqTypes) match {
              case Allow =>
                def isInput = input.contains(id)
                def wasAlreadyInTargetState = p.content.reqs.need(id).live(p.config.reqTypes) ==* mode.toState
                if (!isInput && wasAlreadyInTargetState)
                  TransitiveClosure.Filter.Exclude
                else
                  TransitiveClosure.Filter.Follow
              case Deny =>
                TransitiveClosure.Filter.Exclude
            })

        E.forall(input.whole)(id =>
          E.equal(s"All transitive implications of input $id are now ${mode.toState}",
            impSrcToTgtTC2(id).filter(p2.content.reqs.need(_).live(p2.config.reqTypes) !=* mode.toState),
            Set.empty[ReqId]))

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
      E.equal("auto = TC", auto, input.iterator.map(impSrcToTgtTC(_)).reduce(_ ++ _) -- input.whole)

    def performReverse = {
      val apply   = perform(mode, result.initialReqs).get
      val p2      = applyEventSuccessfully(p, apply)
      val t2      = new Results(!mode, input, p2)
      val reverse = perform(!mode, t2.result.initialReqs).get
      val p3      = applyEventSuccessfully(p2, reverse)
      def norm(p: Project) = p.content.copy(deletionReasons = DeletionReasons.empty)

//      println("=" * 120)
//      println("Apply: " + apply)
//      println("Reverse: " + reverse)
//      println(p3.prettyPrintImplicationGraph)
//      println()

      E.equal("apply.reverse = id", norm(p3), norm(p))
    }

    (invariants(T) & noneOff & autoIsEverythingImplied & performReverse)
      .rename(s"basic(${input.whole.map(_.value).mkString(",")})")
  }

  private def eligible(p: Project): Req => Boolean =
    r => r.live(p.config.reqTypes).is(mode.fromState) && r.allowLiveChange(p.config.reqTypes).is(Allow)

  def allProps =
    testWithInputs(projectBasic, _ => true, basic).rename("projectBasic") &
      testWithInputs(projectFree, eligible(projectFree), free).rename("projectFree")
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object DeletionProps {
  final class Results(mode: DeleteOrRestore, val input: NonEmptySet[ReqId], val p: Project) {
    val E = EvalOver(input)

    val result: DeletionRestorationLogic.Data =
      DeletionRestorationLogic.forReqs(mode, p, input)

    val auto: Set[ReqId] =
      result.initialReqs -- input.whole

    val off: Set[ReqId] =
      result.actionableReqs.iterator.map(_.req.id).filterNot(auto.contains).toSet -- input.whole

    val extra: Set[ReqId] =
      auto ++ off

    val all: Set[ReqId] =
      extra ++ input.whole

    val autoAndInput: Set[ReqId] =
      auto ++ input.whole

    lazy val impSrcToTgtTC =
      changableImpTC(p, Forwards)

    val impTgtToSrcTC =
      changableImpTC(p, Backwards)
  }

  def perform(mode: DeleteOrRestore, reqIds: Set[ReqId]): Option[Event] =
    mode match {
      case Delete  => NonEmptySet.option(reqIds).map(ReqsDelete(_, Set.empty, Text.empty))
      case Restore => Some(ContentRestore(reqIds, Set.empty))
    }

  def changableImpTC(p: Project, dir: Direction) =
    p.content.implications.transitiveClosure(
      dir,
      p.content.reqs.idIterator(),
      p.content.reqs.need(_).allowLiveChange(p.config.reqTypes) match {
        case Allow => TransitiveClosure.Filter.Follow
        case Deny  => TransitiveClosure.Filter.Exclude
      })


  def chooseInputs(p: Project, f: Req => Boolean): List[NonEmptySet[ReqId]] = {
    val ids     = p.content.reqs.reqIterator().filter(f).map(_.id).toList
    val idCount = ids.size
    val singles = ids.take(20).map(NonEmptySet one _)
    val pairs   = ids.combinations(2).map(NonEmptySet force _.toSet).take(6).toList
    var l = singles ::: pairs
    if (idCount > 4)
      l :::= ids.combinations(idCount >> 1).map(NonEmptySet force _.toSet).take(6).toList
    l
  }

  def testWithInputs(p: Project, f: Req => Boolean, t: NonEmptySet[ReqId] => EvalL): EvalL =
    EvalOver(p).forall(chooseInputs(p, f))(t)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class RandomData(mode: DeleteOrRestore) {
    private val * = shipreq.webapp.base.RandomData

    /** A project with max 1 imp/req, and everything is mode.from */
    val genProjectBasic: Gen[Project] =
      for {
        reqtypes1 <- *.customReqTypes
        reqtypes2 = ReqTypes.empty.custom ++ reqtypes1.valuesIterator.map(_.copy(live = Live))
        config    = ProjectConfig.empty.copy(reqTypes = ReqTypes(reqtypes2))
        reqCount  <- Gen.chooseInt(40)
        ucCount   <- Gen.chooseSize map (_ >> 1)
        reqs1     <- *.reqsWithoutText(config, reqCount, ucCount)
        reqs2     = (TestOptics.grsLive.set(mode.fromState) compose TestOptics.ucsLive.set(mode.fromState))(reqs1)
        imps1     <- *.reqFieldDataImplications(reqs2.idIterator().toSet)
        imps2     = imps1.forwards.m.mapValuesNow(_.take(1))
        imps3     = Implications.BiDir(Implications.UniDir(imps2).reverse) // reverse ensures take(1) is on parent side
        content   = ProjectContent.empty.copy(reqs = reqs2, implications = imps3)
        project   = Project.empty.copy(config = config, content = content)
      } yield IdCeilings.supply(ic => project.copy(idCeilings = ic))

    val genProjectFree: Gen[Project] =
      *.project

    val genProps: Gen[DeletionProps] =
      for {
        basic <- genProjectBasic
        free  <- genProjectFree
      } yield new DeletionProps(mode, projectFree = free, projectBasic = basic)
  }
}
