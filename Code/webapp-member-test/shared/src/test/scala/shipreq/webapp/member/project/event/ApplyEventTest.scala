package shipreq.webapp.member.project.event

import java.time.Instant
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.test.WebappTestUtil._
import utest._

object ApplyEventTest extends TestSuite {

  def verifyEvent(p1: Project, e1: Event) = {
    val p2 = ApplyEvent.untrusted.partialApplyUnverified(e1)(p1) match {
      case \/-(p) => p
      case -\/(x) => fail(s"Init failed: $x")
    }

    val ve = VerifiedEvent(EventOrd.first, e1, Instant.now())

    (p2, ve)
  }

  def assertApplicationFailure(vef: VerifiedEvent, p1: Project): Unit =
    ApplyEvent.untrusted(vef)(p1) match {
      case \/-(_) => fail(s"applyVerified passed when it shouldn't have.")
      case -\/(_) => ()
    }

  object Data1 {
    val p1 = Project.empty
    val e1 = FieldStaticRemove(StaticField.StepGraph)
    val (p2, ve) = verifyEvent(p1, e1)
  }

  /*
  def simulateStream(): Unit = {
    val genLogicVerSeq   = Gen.orderedSeq(LogicVers  .whole, 0, dropElems = true, emptyResult = false)
    val genHashSchemeSeq = Gen.orderedSeq(hashSchemes.whole, 0, dropElems = true, emptyResult = false)

    var totalStats = EventStats.empty
    val totalStatsLock = new AnyRef

    val gen: Gen[Vector[VerifiedEvent]] =
      Gen { ctx =>
        var lvs   = genLogicVerSeq run ctx
        var hss   = genHashSchemeSeq run ctx
        var p     = Project.empty
        var stats = EventStats.empty
        var ves   = Vector.empty[VerifiedEvent]

        var lv = LogicVer('z')
        var hs: HashScheme = null
        def advanceLogicVer(): Unit = {
          lv = lvs.head; lvs = lvs.tail
        }
        def advanceHashScheme(): Unit = {
          hs = hss.head; hss = hss.tail
        }
        advanceLogicVer()
        advanceHashScheme()

        def addEvent(): Unit = {
          val ((s2, p2), e) = ApplicableEventGen(p).applicableEventS(stats)(EventStats.observeFn) run ctx
          var hr = HashRec.__changes(HashRec.defaultHashScopes, lv, hs, p, p2)

          // Old logic means hashes that are obsolete and no longer match
          if (lv !=* LogicVer.Current)
            hr = hr.map(r => HashRec(r.scope, r.logicVer, r.scheme)(r.hash.map(_ ^ 0xffff0000)))

          // Simulate hashes being cleared manually in the DB
          if (ctx.nextBit() && ctx.nextBit() && ctx.nextBit() && ctx.nextBit())
            hr = hr.map(r => HashRec(r.scope, r.logicVer, r.scheme)(None))

          ves :+= VerifiedEvent(e, hr)
          stats = s2
          p = p2
        }

        // Now the generation begins...

        while (lvs.nonEmpty || hss.nonEmpty) {

          if (ctx.nextBit())
            addEvent()
          else {
            if (lvs.nonEmpty && ctx.nextBit()) advanceLogicVer()
            if (hss.nonEmpty && ctx.nextBit()) advanceHashScheme()
          }
        }

        for (_ <- 0 until ctx.nextInt4())
          addEvent()

        if (ves.isEmpty)
          addEvent()

        totalStatsLock.synchronized {
          totalStats += stats
        }

        ves
      }

    def mkProp(AE: ApplyEvent) = Prop.atom[Vector[VerifiedEvent]](AE.trust.toString,
      ves => {
        def getP2 = AE(ves.map(_.event))(Project.empty).toOption

//        def printHashValues(): Unit =
//          for (p2 <- getP2) {
//            println("=" * 100)
//            println(s"Hash values (${AE.trust})")
//            val scopes = ves.iterator.flatMap(_.hashRecs.iterator.map(_.scope)).toSet
//            val schemes = ves.iterator.flatMap(_.hashRecs.iterator.map(_.scheme)).toSet
//            val logicVers = ves.iterator.flatMap(_.hashRecs.iterator.map(_.logicVer)).toSet
//            for {
//              scope <- scopes
//              scheme <- schemes
//              logic <- logicVers
//            } {
//              def h(p: Project) = HashScope.hash(scope, scheme.value, p)
//              println(s"  > $scope $logic $scheme = ${h(Project.empty)} → ${h(p2)}")
//            }
//            println()
//          }

        AE(ves)(Project.empty) match {
          case \/-(_) => None
          case -\/(failure) =>
            for (p2 <- getP2)
              println {
                // printHashValues()
                //AE.applyVerified2(ves)(Project.empty)
                def inspect(h: HashRec, p: Project): String = {
                  import h._
                  hash.fold("pass") { e =>
                    val a = recalc(p)
                    val op = if (e ==* a) "=" else "=!="
                    s"[$logicVer $scheme $scope $e  $op  $a]"
                  }
                }
                def pv(v: VerifiedEvent) = v.hashRecs.map(inspect(_, p2)).mkString(", ")
                def pvs(vs: Vector[VerifiedEvent]) = (s"${vs.length} events." +: vs.map(v => s"  - ${EventStats name v.event} - ${pv(v)}")).mkString("\n")
                "Generated: " + pvs(ves) + "\n"
              }
            Some(failure)
        }
      })

    val prop = (mkProp(ApplyEvent.untrusted) & mkProp(ApplyEvent.trusted)).rename("Verified event application")

    gen.mustSatisfy(prop) //(defaultPropSettings.setSampleSize(10000))

    totalStatsLock.synchronized {
      println(totalStats.report)
    }
  }
  */

  override def tests = Tests {

    "applyVerified" - {
      "pass" - {
        import Project.Equality.IgnoringHistory._
        import Data1._
        ApplyEvent.untrusted(ve)(p1) match {
          case \/-(p) => assertEq(p, p2)
          case -\/(e) => fail(s"applyVerified failed: $e")
        }
      }

      // 'prop - simulateStream()
    }
  }
}
