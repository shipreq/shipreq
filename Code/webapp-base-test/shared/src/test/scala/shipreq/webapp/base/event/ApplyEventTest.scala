package shipreq.webapp.base.event

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scalaz.{-\/, \/-}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplyEvent.LogicVer
import shipreq.webapp.base.hash._
import shipreq.webapp.base.hash.HashTestUtil.hashSchemes
import shipreq.webapp.base.text.Text
import shipreq.base.util.{NonEmptyVector, univEqOps}

object ApplyEventTest extends TestSuite {

  /*
//  case class State(reqCodes: Int, events: Vector[Event])

  case class State(reqCodes: Int)

  object RandomEventStream extends StateGen.Fix[State] {

    val initialState = State(0, Vector.empty)

    // different scopes
    // different hashScheme
    // different logicVer

    def blah(s: State): Gen[State] = {
      val rcn = s.reqCodes + 1
      val rcv = NonEmptyVector one ReqCode.Node(rcn.toString)
      val e = CreateReqCodeGroup(ReqCodeId(rcn), ReqCodeGroupGD.Code(rcv))
      val s2 = s.copy(reqCodes = rcn, events = s.events :+ e)
      Gen pure s2
    }

    //    genS()


  }
*/

  /*
  def changePoints(changes: Int, seqLength: Int): Gen[Vector[Int]] =
    if (changes == 0)
      Gen pure Vector.empty
    else if (changes < 0)
      sys.error(s"Can't have a negative number of changes ($changes).")
    else if (seqLength < (changes + 1))
      sys.error(s"For $changes changes, a minimum seqLength of ${changes + 1} is required.")
    else {
      val tmp = seqLength - 1 - changes
      Gen { ctx =>
        val v = Vector.newBuilder[Int]
        var prev = 0
        for (change <- 1 to changes) {
          prev = Gen.chooseInt(prev + 1, tmp + change) run ctx
          v += prev
        }
        v.result()
      }
    }

  def changePoints2(maxChanges: Int, seqLength: Int): Gen[Vector[Int]] =
    if (maxChanges < 1 || seqLength < 1)
      Gen pure Vector.empty
    else {
      val max = seqLength - 1
      Gen { ctx =>
        val v = Vector.newBuilder[Int]
        var prev = 0
        for (change <- 1 to maxChanges) {
          prev = Gen.chooseInt(prev, max) run ctx
          v += prev
        }
        v.result()
      }
    }

  class ChangePoints(changes: Vector[Int]) {

    def forChange(changeIndex: Int): Int =
      changes(changeIndex)

    def atPos(pos: Int): Int = {
      require(pos >= 0, "pos must be ≥ 0")
      @tailrec def go(change: Int): Int =
        if (change < 0)
      {
        val start = changes(change)
        if (start)
      }
      go(0)
    }
  }
  */

  val OldLogicVer = LogicVer(33)
  val LogicVers = OldLogicVer +: LogicVer.all

  def verifyEvent(p1: Project, e1: Event) = {

    val p2 = ApplyEvent.untrusted.apply1(e1)(p1) match {
      case \/-(p) => p
      case -\/(x) => fail(s"Init failed: $x")
    }

    val hrs = HashRec.changes(p1, p2)
    assert(hrs.nonEmpty)
    val ve = VerifiedEvent(e1, hrs)

    (p2, ve)
  }

  def assertApplicationFailure(vef: VerifiedEvent, p1: Project): Unit =
    ApplyEvent.untrusted.applyVerified(List(vef))(p1) match {
      case \/-(p) => fail(s"applyVerified passed when it shouldn't have.")
      case -\/(e) => ()
    }

//  val corruptProject: Project => Project = {
//    val drt = NonEmptyVector one Text.DeletionReason.Literal("OMG")
//    Project.deletionReasons.modify(d => d.copy(reasons = d.reasons :+ drt))
//  }

  override def tests = TestSuite {
    'applyVerified {

      val p1 = Project.empty
      val e1 = DeleteStaticField(StaticField.StepGraph)
      val (p2, ve) = verifyEvent(p1, e1)

      'pass {
        ApplyEvent.untrusted.applyVerified(List(ve))(p1) match {
          case \/-(p) => assertEq(p, p2)
          case -\/(e) => fail(s"applyVerified failed: $e")
        }
      }

      'fail {
        val vef = ve.copy(hashRecs = ve.hashRecs.map(r => HashRec(r.scope, r.logicVer, r.scheme)(r.hash.map(_ + 1))))
        assertApplicationFailure(vef, p1)
      }

      'checkUnspecifiedScopes {
        val (_, ve) = verifyEvent(Project.empty, ApplyTemplate(ProjectTemplate.Default))
        val vef = ve.copy(hashRecs = ve.hashRecs.drop(1))
        assertApplicationFailure(vef, Project.empty)
      }
    }

    'stuff {

      def log(s: => String): Unit = println(s)
      //def log(s: => String): Unit = ()

      val genLogicVerSeq   = Gen.orderedSeq(LogicVers  .whole, 0, dropElems = true, emptyResult = false)
      val genHashSchemeSeq = Gen.orderedSeq(hashSchemes.whole, 0, dropElems = true, emptyResult = false)

      def gen(initProject: Project = Project.empty) =
        Gen { ctx =>
//          println("Gen starting.")
//          try {

          var lvs = genLogicVerSeq run ctx
          var hss = genHashSchemeSeq run ctx
//          val lvCount = lvs.length
//          val hsCount = hss.length

          var p = initProject
          var stats = EventStats.empty
          var ves = Vector.empty[VerifiedEvent]

          var lv = LogicVer('z')
          var hs: HashScheme = null
          def advanceLogicVer(): Unit = {lv = lvs.head; lvs = lvs.tail}
          def advanceHashScheme(): Unit = {hs = hss.head; hss = hss.tail}
          advanceLogicVer()
          advanceHashScheme()

          // TODO Remove
//          lv = LogicVer.Current
//          lvs = Vector.empty

          def addEvent(): Unit = {

            val ((s2, p2), e) = GenSuccEvent(p).applicableEventS(stats)(EventStats.observeFn) run ctx

            var hr = HashRec.__changes(HashRec.defaultHashScopes, lv, hs, p, p2)

            // Old logic means hashes that are obsolete and no longer match
            if (lv !=* LogicVer.Current)
              hr = hr.map(r => HashRec(r.scope, r.logicVer, r.scheme)(r.hash.map(_ ^ 0xffff0000)))

            ves :+= VerifiedEvent(e, hr)
            stats = s2
            p = p2
          }

          while (lvs.nonEmpty || hss.nonEmpty) {
            if (ctx.nextBit())
              addEvent()
            else {
              if (lvs.nonEmpty && ctx.nextBit()) advanceLogicVer()
              if (hss.nonEmpty && ctx.nextBit()) advanceHashScheme()
            }
          }

          for (_ <- 0 until ctx.nextInt3())
            addEvent()

          if (ves.isEmpty)
            addEvent()

//          println(s"${ves.size} @ $lvCount/$hsCount")
//          println(stats.report)
//          println()
//          log(
//            (s"Generated: ${ves.length} events." +:
//              ves.map(v => s"  - ${EventStats name v.event} - ${v.hashRecs.map(r => s"L${r.logicVer.value.toInt} H${r.scheme.id.value.toInt}").mkString("[",", ", "]")}"))
//              .mkString("\n") + "\n")

          // TODO Merge and show stats

          ves
//          }finally{
//            println("Gen finished.")
//          }
      }

      val g = gen()

      def mkProp(AE: ApplyEvent) = Prop.atom[VerifiedEvents](AE.trust.toString, ves => {
//        println("Prop running.")

        def getP2 = AE(ves.map(_.event))(Project.empty).toOption

        def printHashValues(): Unit =
          for (p2 <- getP2) {
            println("=" * 100)
            println(s"Hash values (${AE.trust})")
            val scopes    = ves.iterator.flatMap(_.hashRecs.iterator.map(_.scope)).toSet
            val schemes   = ves.iterator.flatMap(_.hashRecs.iterator.map(_.scheme)).toSet
            val logicVers = ves.iterator.flatMap(_.hashRecs.iterator.map(_.logicVer)).toSet
            for {
              scope  <- scopes
              scheme <- schemes
              logic  <- logicVers
            } {
              def h(p: Project) = HashScope.hash(scope, scheme.value, p)
              println(s"  > $scope $logic $scheme = ${h(Project.empty)} → ${h(p2)}")
            }
            println()
          }

        val r = AE.applyVerified(ves)(Project.empty)
        if (r.isLeft)
          for (p2 <- getP2)
            log {
              //          printHashValues()
              //AE.applyVerified2(ves)(Project.empty)
              def pv(v: VerifiedEvent) = v.hashRecs.map(_.inspect(p2)).mkString(", ")
              def pvs(vs: VerifiedEvents) = (s"${vs.length} events." +: vs.map(v => s"  - ${EventStats name v.event} - ${pv(v)}")).mkString("\n")
              "Generated: " + pvs(ves) + "\n"
            }

//        println("Prop finished.")
        r.swap.toOption
      })

//      val prop = (mkProp(ApplyEvent.untrusted) & mkProp(ApplyEvent.trusted)).rename("Verified event application")
      val prop = mkProp(ApplyEvent.trusted)

//      g.mustSatisfy(prop)(defaultPropSettings)
      //g.mustSatisfy(prop)(defaultPropSettings.setSeed(84).setSampleSize(10).setDebug)
//      g.bugHunt(689, 1000, printFailure = false)(prop)
      g.bugHunt(seeds = 1000)(prop)(defaultPropSettings)

      // TODO should also wipe some hashrecs to demonstrate manual intervention

    }
  }
}
