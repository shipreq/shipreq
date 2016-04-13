package shipreq.webapp.server.db

import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import utest._
import shipreq.base.util._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes.AutoNES._
import shipreq.webapp.base.text.Text
import shipreq.webapp.server.test.TestDB
import EventDao.EventSeq

object DaoTest2 extends TestSuite {

  val tldb = TestDB.threadLocalResHP()

  def eventPropTest(): Unit = {
    // implicit val settings = DefaultSettings.propSettings.setSampleSize(10).setGenSize(10).setDebug.setSingleThreaded.setSeed(0)
    // implicit val settings = DefaultSettings.propSettings.setSampleSize(20000).setGenSize(4).setDebug
    implicit val settings = DefaultSettings.propSettings.setSampleSize(320).setGenSize(16)

    val seqCounter = new AtomicInteger()

    val prop = Prop.equal[(ActiveEvent, HashRec.Collection)]("load . save = id")(
      i => {
        val seq = EventSeq(seqCounter.incrementAndGet())
        implicit val (s, db, projectId) = tldb.get()
        val dao = db.dao
        dao.createEvent(projectId, seq, i._1, i._2)
        val loaded =
          db.debugSelectOnError(s"select * from event e, event_hash eh where e.project_id=eh.project_id and e.seq=eh.seq and e.seq = ${seq.value}") {
            dao.findAllEvents(projectId).filter(_._1 == seq).map(r => r._2.event match {
              case ae: ActiveEvent => (ae, r._2.hashRecs)
              case e               => sys error s"Not an ActiveEvent: $e"
            })
          }
        loaded
      },
      Vector1(_))

    val rnd = RandomData.events.activeEvent *** RandomData.events.hashRecs

    tldb around prop.mustBeSatisfiedBy(rnd)
  }

  private val q3 = "\"\"\""
  def demo[E <: ActiveEvent](gen: Gen[E]) = {
    import EventDbCodecs.eventCodecRegistry
    val es = gen.samplesSized(3).take(10)
    println("_"*120)
    for (e <- es) {
      val c = eventCodecRegistry.writer(e)
      val d = c._2.write(e)
      val json = Option(d._3).fold("null")(q3 + _ + q3)
      println(
        s"""
           |testRW(${e.toString.replaceAll(",(?! )", ", ")},
           |  ${c._1}, ${d._2}, '${d._1.getValue}', $json)
         """.stripMargin)
    }
    println()
  }

  def testRW(e: ActiveEvent, typeId: Short, dataId: Integer, dataTypeId: Char, data: String): Unit = {
    import EventDbCodecs.eventCodecRegistry

    val c = eventCodecRegistry.writer(e)
    val (aDataTypeId, aDataId, aData) = c._2.write(e)
    assertEq(s"TypeId for $e", c._1, typeId)
    assertEq(s"DataId for $e", (aDataId, aDataTypeId.getValue), (dataId, dataTypeId.toString))
    assertEq(s"Data for $e", aData, data)

    val aEvent = c._2.read(aDataTypeId.getValue.head.toByte, aDataId, aData)
    assertEq(aEvent, e)
  }

  import shipreq.webapp.base.test.UnsafeTypes._

  override def tests = TestSuite {
    'event {
      'prop - eventPropTest()

      // These tests serve two purposes:
      // 1) Ensure data is stored as expected (efficient & human-readable).
      // 2) Ensure each Event's format never changes once established.
      'data {

        'addStaticField {
          testRW(AddStaticField(StaticField.ExceptionStepTree), 40, -2, 's', null)
          testRW(AddStaticField(StaticField.NormalAltStepTree), 40, -1, 's', null)
          testRW(AddStaticField(StaticField.StepGraph        ), 40, -3, 's', null)
        }

        'applyTemplate {
          testRW(ApplyTemplate(ProjectTemplate.Default), 0, null, ' ', """1""")
        }

        //'createApplicableTag    - demo(RandomData.events.createApplicableTag   )

        //'createCustomImpField   - demo(RandomData.events.createCustomImpField  )

        //'createCustomIssueType  - demo(RandomData.events.createCustomIssueType )

        //'createCustomReqType    - demo(RandomData.events.createCustomReqType   )

        //'createCustomTagField   - demo(RandomData.events.createCustomTagField  )

        //'createCustomTextField  - demo(RandomData.events.createCustomTextField )

        'createGenericReq {
          import CreateGenericReqGD._
          testRW(CreateGenericReq(123, 449099973, Tags(NonEmptySet(ApplicableTagId(166426196)))),
            230, 123, 'g', """{"T":449099973,"#":166426196}""")

          testRW(CreateGenericReq(123, 1469773577, ImpTgts(NonEmptySet(GenericReqId(2074289209)))),
            230, 123, 'g', """{"T":1469773577,"<":2074289209}""")

          testRW(CreateGenericReq(123, 1469773577, ImpSrcs(NonEmptySet(GenericReqId(2074289209)))),
            230, 123, 'g', """{"T":1469773577,">":2074289209}""")

          testRW(CreateGenericReq(123, 1469773577, ReqCodes(ReqCode.IdAndValue(7, "yay"))),
            230, 123, 'g', """{"T":1469773577,"c":{"yay":7}}""")
        }

        //'createReqCodeGroup     - demo(RandomData.events.createReqCodeGroup    )

        //'createTagGroup         - demo(RandomData.events.createTagGroup        )

        //'deleteCustomField      - demo(RandomData.events.deleteCustomField     )

        //'deleteCustomIssueType  - demo(RandomData.events.deleteCustomIssueType )

        //'deleteCustomReqType    - demo(RandomData.events.deleteCustomReqType   )

        //'deleteReqCodeGroup     - demo(RandomData.events.deleteReqCodeGroup    )

        //'deleteReq              - demo(RandomData.events.deleteReq             )

        //'deleteStaticField      - demo(RandomData.events.deleteStaticField     )

        //'deleteTag              - demo(RandomData.events.deleteTag             )

        'patchImplicationSrc {
          testRW(PatchImplicationSrc(1234, nesd[ReqId](34, 45)(67, 89)),
            201, 1234, 'g', """{"-":[34,45],"+":[67,89]}""")

          testRW(PatchImplicationSrc(1234, nesd[ReqId]()(2128131835)),
            201, 1234, 'g', """{"+":2128131835}""")

          testRW(PatchImplicationSrc(1234, nesd[ReqId](1086529477)()),
            201, 1234, 'g', """{"-":1086529477}""")
        }

        'patchImplicationTgt {
          testRW(PatchImplicationTgt(1234, nesd[ReqId](34, 45)(67, 89)),
            202, 1234, 'g', """{"-":[34,45],"+":[67,89]}""")

          testRW(PatchImplicationTgt(1234, nesd[ReqId]()(2128131835)),
            202, 1234, 'g', """{"+":2128131835}""")

          testRW(PatchImplicationTgt(1234, nesd[ReqId](1086529477)()),
            202, 1234, 'g', """{"-":1086529477}""")
        }

        'patchReqCodes {
          val mm = UnivEq.emptySetMultimap[ReqCode.Value, ReqCodeId]
          testRW(PatchReqCodes(666, Set(3), Set(1095731751, 1055755379), mm),
            203, 666, 'g', """{"-":3,"^":[1095731751,1055755379]}""")

          testRW(PatchReqCodes(667, Set(), Set(), mm.add("lnls", 1168583026).addvs("m", Set(976426332, 522011847))),
            203, 667, 'g', """{"+":{"m":[522011847,976426332],"lnls":1168583026}}""")
        }

        'patchReqTags {
          testRW(PatchReqTags(1234, nesd[ApplicableTagId](34, 45)(67, 89)),
            204, 1234, 'g', """{"-":[34,45],"+":[67,89]}""")

          testRW(PatchReqTags(1234, nesd[ApplicableTagId]()(2128131835)),
            204, 1234, 'g', """{"+":2128131835}""")

          testRW(PatchReqTags(1234, nesd[ApplicableTagId](1086529477)()),
            204, 1234, 'g', """{"-":1086529477}""")
        }

        //'repositionField        - demo(RandomData.events.repositionField       )

        'setCustomTextField {
          testRW(SetCustomTextField(2345, CustomField.Text.Id(123), Text.CustomTextField.demo(9, 8, 7, 6)),
          205, 2345, 'g',
            """
              │{"f":123,"t":[
              │"Atom demonstration.",
              │0,
              │"Here we go:",
              │{"*":[
              │["Req: ",{"r":9}],
              │["Code: ",{"c":8}],
              │["Tag: ",{"t":7}],
              │["Issue(∅): ",{"i":6}],
              │["Issue(∃): ",{"i":6,"?":["Need to finish ",{"r":9}," and ",{"c":8}]}],
              │["Issue(∃): ",{"i":6,"?":["Ask ",{"@":"bob@gmail.com"}," about ",{"=":"e=mc^2"}]}],
              │[],
              │["Math: ",{"=":"f(x) = {x+1 \\over x - 1} + 9\\pi^2"}],
              │["Email: ",{"@":"blah@google.com"}],
              │["Web: ",{"/":"https://shipreq.com"}]
              │]}
              │]}
            """.stripMargin('│').replace("\n", "").trim
          )
        }

        //'setGenericReqTitle     - demo(RandomData.events.setGenericReqTitle    )

        //'setGenericReqType      - demo(RandomData.events.setGenericReqType     )

        //'updateApplicableTag    - demo(RandomData.events.updateApplicableTag   )

        //'updateCustomImpField   - demo(RandomData.events.updateCustomImpField  )

        //'updateCustomIssueType  - demo(RandomData.events.updateCustomIssueType )

        //'updateCustomReqType    - demo(RandomData.events.updateCustomReqType   )

        //'updateCustomTagField   - demo(RandomData.events.updateCustomTagField  )

        //'updateCustomTextField  - demo(RandomData.events.updateCustomTextField )

        //'updateReqCodeGroup     - demo(RandomData.events.updateReqCodeGroup    )

        //'updateTagGroup         - demo(RandomData.events.updateTagGroup        )

        //'addUseCaseStep        - demo(RandomData.events.addUseCaseStep       )
        //'shiftUseCaseStepLeft  - demo(RandomData.events.shiftUseCaseStepLeft )
        //'shiftUseCaseStepRight - demo(RandomData.events.shiftUseCaseStepRight)
        //'deleteUseCaseStep     - demo(RandomData.events.deleteUseCaseStep    )
        //'setUseCaseStepText    - demo(RandomData.events.setUseCaseStepText   )
        //'setUseCaseTitle       - demo(RandomData.events.setUseCaseTitle      )
        //'createUseCase         - demo(RandomData.events.createUseCase        )

      }
    }
  }
}
