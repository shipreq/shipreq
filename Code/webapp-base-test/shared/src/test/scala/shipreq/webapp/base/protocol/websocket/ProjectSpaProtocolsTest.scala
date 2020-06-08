package shipreq.webapp.base.protocol.websocket

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.microlibs.scalaz_ext.ScalazMacros
import java.time.Instant
import scalaz.{-\/, Equal, \/, \/-}
import sourcecode.Line
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.event.EventEquality._
import shipreq.webapp.base.sort.SortMethod._
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil.verifiedEventsFromJson
import shipreq.webapp.base.text.Text
import utest._

/** Tests the stability of Project SPA protocols.
  *
  * Generate test code by uncommenting out GenerateUnitTest lines in ProjectSpaLogic.scala
  */
object ProjectSpaProtocolsTest extends TestSuite {
  import EventOrd.Latest
  import ProjectSpaProtocols._
  import WsReqRes._
  import WebSocketShared.ReqId

  private val webSocket = WebSocket("fake_project_id")
  private val codecCS   = WebSocketShared.protocolCS(webSocket.req.codec).codec

  private implicit def univEqWsReq: UnivEq[WsReqRes.AndReq] = UnivEq.force
  protected implicit val equalProjectAndOrd: Equal[ProjectAndOrd] = ScalazMacros.deriveEqual
  private implicit val equalInitAppData: Equal[InitAppData] = ScalazMacros.deriveEqual

  private def assertRequest(bin: BinaryData, expect: codecCS.Data)(implicit l: Line) =
    assertDecodeOk(codecCS)(bin, expect)

  private def assertResponse(res: WsReqRes)(bin: BinaryData, expect: WebSocket.Push \/ (ReqId, res.ResponseType))(implicit l: Line, eq: Equal[res.ResponseType]) = {
    val codecSC = WebSocketShared.protocolSC(_ => res.protocolRes)(webSocket.push.codec).codec
    assertDecodeVia(codecSC)(bin, \/-(expect))(
      _.map { case (reqId, protocolAndValue) => (reqId, protocolAndValue.value.asInstanceOf[res.ResponseType]) },
      _.map { case (reqId, v) => (reqId, res.protocolRes.andValue(v)) }
    )
  }

  override def tests = Tests {

    // =================================================================================================================
    "InitApp" - {
      "req" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D0100010038295653")
          val expect = (ReqId(1),InitApp.AndReq(()))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        import WsReqRes.InitApp.protocolRes.codec

        "v1.0" - {

          // TODO test success

          "error" - {
            val bin    = BinaryData.fromHex("0100010A6F6D66672066697265213B301988")
            val expect = -\/(ErrorMsg("omfg fire!"))
            assertDecodeOk(codec)(bin, expect)
          }
        }
      }
    }

    // =================================================================================================================
    "Reconnect" - {
      "req" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D010008010282E238295653")
          val expect = (ReqId(8),Reconnect.AndReq(Some(Latest(738))))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        val bin    = BinaryData.fromHex("010010010000091C6585")
        val expect = \/-((ReqId(8),VerifiedEvent.Seq.empty))
        assertResponse(Reconnect)(bin, expect)
      }
    }

    // =================================================================================================================
    "Sync" - {
      "req" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D01007B0203070B0338295653")
          val expect = (ReqId(123), WsReqRes.Sync.AndReq(NonEmptySet(3, 7, 11)))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("010081FE0100")
          val expect = \/-((ReqId(255), ()))
          assertResponse(Sync)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "UpdateConfig" - {
      import UpdateConfigCmd._
      "req" - {

        "v1.1" - {
          "CustomIssueTypeUpdate" - {
            val bin    = BinaryData.fromHex("5945B41D01011203070003024B0750454E44494E47440208796F20796F20796F38295653")
            val expect = (ReqId(18),UpdateConfig.AndReq(CustomIssueTypeUpdate(CustomIssueTypeId(3),CustomIssueTypeGD(HashRefKey("PENDING"),Some("yo yo yo")))))
            assertRequest(bin, expect)
          }

          "ApplicableTagCreate" - {
            val ^ = ApplicableTagGD
            val values = ^.nev(
              ^.ValueForApplicableReqTypes(ApplicableReqTypes.blacklist(3)),
              ^.ValueForChildren(Vector(6.AT, 444.TG)),
              ^.ValueForColour(Colour("#f32").get),
              ^.ValueForDesc("zzz"),
              ^.ValueForKey("qwe"),
              ^.ValueForParents(Map(1.TG -> None, 2.AT -> Some(3.TG))),
            )
            val expect = (ReqId(8), UpdateConfig.AndReq(ApplicableTagCreate(values)))
            val bin = BinaryData.fromHex("5945B41D010108030F0663020423663332500267000101610002026700036402037A7A7A726E016300034302610006670081BC230371776538295653")
            assertRequest(bin, expect)
          }

          "ApplicableTagUpdate" - {
            val ^ = ApplicableTagGD
            val values = ^.nev(
              ^.ValueForApplicableReqTypes(ApplicableReqTypes.empty),
              ^.ValueForColour(None),
              ^.ValueForDesc(None),
              ^.ValueForParents(Map.empty),
            )
            val expect = (ReqId(8), UpdateConfig.AndReq(ApplicableTagUpdate(81.AT, values)))
            val bin = BinaryData.fromHex("5945B41D0101080310005104726163016401500038295653")
            assertRequest(bin, expect)
          }

          "TagGroupCreate" - {
            val ^ = TagGroupGD
            val values = ^.nev(
              ^.ValueForChildren(Vector(6.AT, 444.TG)),
              ^.ValueForExclusivity(Exclusive),
              ^.ValueForDesc("zzz"),
              ^.ValueForName("qwe"),
              ^.ValueForParents(Map(1.TG -> None, 2.AT -> Some(3.TG))),
            )
            val expect = (ReqId(8), UpdateConfig.AndReq(TagGroupCreate(values)))
            val bin = BinaryData.fromHex("5945B41D0101080312054E037177654D01500267000101610002026700034402037A7A7A4302610006670081BC38295653")
            assertRequest(bin, expect)
          }

          "TagGroupUpdate" - {
            val ^ = TagGroupGD
            val values = ^.nev(
              ^.ValueForChildren(Vector.empty),
              ^.ValueForExclusivity(NonExclusive),
            )
            val expect = (ReqId(8), UpdateConfig.AndReq(TagGroupUpdate(52.TG, values)))
            val bin = BinaryData.fromHex("5945B41D010108031300340243004D0038295653")
            assertRequest(bin, expect)
          }

          "TagSetLiveChildrenOrder" - {
            val bin = BinaryData.fromHex("5945B41D01010803150009020003001138295653")
            val expect = (ReqId(8), UpdateConfig.AndReq(TagSetLiveChildrenOrder(9.TG, Vector(3.AT, 17.AT))))
            assertRequest(bin, expect)
          }

          "TagDelete" - {
            val bin = BinaryData.fromHex("5945B41D010108031167000438295653")
            val expect = (ReqId(8), UpdateConfig.AndReq(TagDelete(4.TG)))
            assertRequest(bin, expect)
          }

          "TagRestore" - {
            val bin = BinaryData.fromHex("5945B41D010108031461000338295653")
            val expect = (ReqId(8), UpdateConfig.AndReq(TagRestore(3.AT)))
            assertRequest(bin, expect)
          }

          "CustomFieldCreateImp" - {
            val bin = BinaryData.fromHex("5945B41D01010203166300030263002102630020020138295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldCreateImp(3, FieldReqTypeRules.optional.mandatory(33, 32))))
            assertRequest(bin, expect)
          }

          "CustomFieldCreateTag" - {
            val bin = BinaryData.fromHex("5945B41D0101020317002F0363000403000E63000703900475010038295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldCreateTag(47.TG, FieldReqTypeRules.notApplicable.defaultTo(14.AT)(4, 7).optional(StaticReqType.UseCase))))
            assertRequest(bin, expect)
          }

          "CustomFieldCreateText" - {
            val bin = BinaryData.fromHex("5945B41D010102031804706F6F7002630080B100630001000238295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldCreateText("poop", FieldReqTypeRules.mandatory.notApplicable(177, 1))))
            assertRequest(bin, expect)
          }

          "CustomFieldUpdateImp" - {
            val bin = BinaryData.fromHex("5945B41D0101020301000C015201630003020138295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldUpdateImp(12.CFImp, CustomImpFieldGD(FieldReqTypeRules.optional.mandatory(3)))))
            assertRequest(bin, expect)
          }

          "CustomFieldUpdateTag" - {
            val bin = BinaryData.fromHex("5945B41D0101020302000B01520363000403000E630007039004750390040038295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldUpdateTag(11.CFTag, CustomTagFieldGD(FieldReqTypeRules.notApplicable.defaultTo(14.AT)(4, 7, StaticReqType.UseCase)))))
            assertRequest(bin, expect)
          }

          "CustomFieldUpdateText" - {
            val bin = BinaryData.fromHex("5945B41D0101020303000A024E03617364520175010238295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldUpdateText(10.CFText, CustomTextFieldGD("asd", FieldReqTypeRules.mandatory.optional(StaticReqType.UseCase)))))
            assertRequest(bin, expect)
          }

          "CustomFieldDelete" - {
            val bin = BinaryData.fromHex("5945B41D010102031978000C38295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldDelete(12.CFText)))
            assertRequest(bin, expect)
          }

          "CustomFieldRestore" - {
            val bin = BinaryData.fromHex("5945B41D010102031A69000938295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(CustomFieldRestore(9.CFImp)))
            assertRequest(bin, expect)
          }

          "StaticFieldAdd" - {
            val bin = BinaryData.fromHex("5945B41D010102031B6738295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(StaticFieldAdd(StaticField.StepGraph)))
            assertRequest(bin, expect)
          }

          "StaticFieldRemove" - {
            val bin = BinaryData.fromHex("5945B41D010102031C6938295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(StaticFieldRemove(StaticField.ImplicationGraph)))
            assertRequest(bin, expect)
          }

          "FieldUpdateOrder" - {
            val bin = BinaryData.fromHex("5945B41D010102030E740007024E38295653")
            val expect = (ReqId(2), UpdateConfig.AndReq(FieldUpdateOrder(7.CFTag, Some(StaticField.NormalAltStepTree))))
            assertRequest(bin, expect)
          }
        }
      }

      "resp" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("0100240100000182F209000301440208796F20796F20796FE0CBA98D5D40BFD7327786DA86")
          val expect = \/-((ReqId(18),\/-(verifiedEventsFromJson("""{"#":754,"event":{"CustomIssueTypeUpdate":{"id":3,"values":[{"desc":"yo yo yo"}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
          assertResponse(UpdateConfig)(bin, expect)
        }
        "v1.1" - {
          val bin    = BinaryData.fromHex("0100808801010001093700030363020423663830726F02756300056402027878E0CBA98D5D40BFD7327786DA86")
          val expect = \/-((ReqId(68),\/-(verifiedEventsFromJson("""{"#":9,"event":{"ApplicableTagUpdate:2":{"id":3,"values":[{"colour":"#f80"},{"reqTypes":{"only":["uc",5]}},{"desc":"xx"}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
          assertResponse(UpdateConfig)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "CreateContent" - {
      "req" - {
        import CreateContentCmd._

        "v1.0" - {
          import Text.GenericReqTitle.{apply => mk, _}
          val bin    = BinaryData.fromHex("5945B41D01000604670000000167000E0001010009026C07556D6D2E2E2E206900010038295653")
          val expect = (ReqId(6),CreateContent.AndReq(CreateGenericReq(
            Set(),
            Map(),
            Direction.Values {
              case Forwards => Set()
              case Backwards => Set(GenericReqId(14))
            },
            CustomReqTypeId(1),
            Set(ApplicableTagId(9)),
            mk(Literal("Umm... "), Issue(CustomIssueTypeId(1),Text.InlineIssueDesc.empty)))))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("01000C0100000182E61900200001033E0167000E2301000954026C07556D6D2E2E2E2069000100E0CBA98D5D40BFD7327786DA86")
          val expect = \/-((ReqId(6),\/-(verifiedEventsFromJson("""{"#":742,"event":{"GenericReqCreate":{"reqId":32,"reqTypeId":1,"values":[{"impSrcs":[{"gr":14}]},{"tags":[9]},{"title":[{"lit":"Umm... "},{"issue":{"desc":[],"type":1}}]}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
          assertResponse(CreateContent)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "UpdateContent" - {
      "req" - {
        import UpdateContentCmd._

        "v1.0" - {
          import Text.GenericReqTitle.{apply => mk, _}
          val bin    = BinaryData.fromHex("5945B41D010002050C0006016C185068A40361727274202D20686520657869737473206E6F772138295653")
          val expect = (ReqId(2),UpdateContent.AndReq(SetGenericReqTitle(GenericReqId(6),mk(Literal("Phäarrt - he exists now!")))))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("0100040100000182E31A0006016C185068A40361727274202D20686520657869737473206E6F7721E0CBA98D5D40BFD7327786DA86")
          val expect = \/-((ReqId(2),\/-(verifiedEventsFromJson("""{"#":739,"event":{"GenericReqTitleSet":{"id":6,"value":[{"lit":"Phäarrt - he exists now!"}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
          assertResponse(UpdateContent)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "ProjectNameSet" - {
      "req" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D010004060B4D722E20436F6E74656E7438295653")
          val expect = (ReqId(4),ProjectNameSet.AndReq("Mr. Content"))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("0100080100000182E51F0B4D722E20436F6E74656E74E0CBA98D5D40BFD7327786DA86")
          val expect = \/-((ReqId(4),\/-(verifiedEventsFromJson("""{"#":741,"event":{"ProjectNameSet":"Mr. Content"}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
          assertResponse(ProjectNameSet)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "UpdateSavedViews" - {
      import SavedViewCmd._
      import shipreq.webapp.base.data.savedview._
      import SavedView._
      import SavedViewGD.{nev, ValueForName}

      "req" - {
        "v1.0" - {
          "rename" - {
            val bin = BinaryData.fromHex("5945B41D010008077516014E0949737375655465737438295653")
            val expect = (ReqId(8), UpdateSavedViews.AndReq(Update(Id(22), nev(ValueForName(Name("IssueTest"))))))
            assertRequest(bin, expect)
          }
          "create" - {
            val bin    = BinaryData.fromHex("5945B41D01010907630564656C6D65D5F5B607010904070601740004030101740003030001690007017800010000000001024C010100819C38295653")
            val expect = (ReqId(9), UpdateSavedViews.AndReq(Create(Name("delme"), View(
              NonEmptyVector(Column.Pubid, Column.Title, Column.OtherTags, Column.CustomField(CustomField.Tag.Id(4)),
                Column.Implications(Forwards), Column.CustomField(CustomField.Tag.Id(3)), Column.Implications(Backwards),
                Column.CustomField(CustomField.Implication.Id(7)), Column.CustomField(CustomField.Text.Id(1))),
              SortCriteria(Vector(), SortCriterion.Conclusive(Column.Pubid, Asc)), HideDead, None,
              Some(ImpGraphConfig(ImpGraphConfig.GraphDir.LeftToRight, ImpGraphConfig.LabelFormat.PubidAndTitle,
                ImpGraphConfig.Colours.ByTag(TagGroupId(412)
                )))))))
            assertRequest(bin, expect)
          }
        }
      }

      "resp" - {
        "v1.0" - {
          "rename" - {
            val bin    = BinaryData.fromHex("0100100100000182E82916014E09497373756554657374E0CBA98D5D40BFD7327786DA86")
            val expect = \/-((ReqId(8),\/-(verifiedEventsFromJson("""{"#":744,"event":{"SavedViewUpdate":{"id":22,"values":[{"name":"IssueTest"}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
            assertResponse(UpdateSavedViews)(bin, expect)
          }
          "create" - {
            val bin    = BinaryData.fromHex("0100120100000182E926170564656C6D650904070601740004030101740003030001690007017800010000000001E0CBA98D5D40BFD7327786DA86")
            val expect = \/-((ReqId(9),\/-(verifiedEventsFromJson("""{"#":745,"event":{"SavedViewCreate":{"columns":["pubid","title","tags",{"custom":{"tag":4}},{"imps":"->"},{"custom":{"tag":3}},{"imps":"<-"},{"custom":{"imp":7}},{"custom":{"text":1}}],"filter":null,"filterDead":"hide","id":23,"name":"delme","order":{"init":[],"last":{"column":"pubid","method":"asc"}}}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
            assertResponse(UpdateSavedViews)(bin, expect)
          }
          "delete" - {
            val bin    = BinaryData.fromHex("5945B41D01000A07641738295653")
            val expect = (ReqId(10),UpdateSavedViews.AndReq(Delete(Id(23))))
            assertRequest(bin, expect)
          }
        }
      }
    }

    // =================================================================================================================
    "UpdateManualIssues" - {
      import ManualIssueCmd._
      import Text.ManualIssue._
      "req" - {
        "v1.0" - {
          "create" - {
            val bin    = BinaryData.fromHex("5945B41D01000B0863026C04617364207267000138295653")
            val expect = (ReqId(11),UpdateManualIssues.AndReq(Create(nonEmpty(Literal("asd "), ReqRef(GenericReqId(1))))))
            assertRequest(bin, expect)
          }
          "delete" - {
            val bin    = BinaryData.fromHex("5945B41D01000D08640738295653")
            val expect = (ReqId(13),UpdateManualIssues.AndReq(Delete(ManualIssueId(7))))
            assertRequest(bin, expect)
          }
        }
      }

      "resp" - {
        "v1.0" - {
          "create" - {
            val bin    = BinaryData.fromHex("0100160100000182EB1C07026C046173642072670001E0CBA98D5D40BFD7327786DA86")
            val expect = \/-((ReqId(11),\/-(verifiedEventsFromJson("""{"#":747,"event":{"ManualIssueCreate":{"id":7,"text":[{"lit":"asd "},{"req":{"gr":1}}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
            assertResponse(UpdateManualIssues)(bin, expect)
          }
          "delete" - {
            val bin    = BinaryData.fromHex("01001A0100000182ED1D07E0CBA98D5D40BFD7327786DA86")
            val expect = \/-((ReqId(13),\/-(verifiedEventsFromJson("""{"#":749,"event":{"ManualIssueDelete":7}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
            assertResponse(UpdateManualIssues)(bin, expect)
          }
        }
      }
    }

    // =================================================================================================================
    "ReqTypeImplicationMod" - {
      "req" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("5945B41D0100100A00010138295653")
          val expect = (ReqId(16),ReqTypeImplicationMod.AndReq((CustomReqTypeId(1),Mandatory)))
          assertRequest(bin, expect)
        }
      }

      "resp" - {
        "v1.0" - {
          val bin    = BinaryData.fromHex("0100200100000182F00D0001014901E0CBA98D5D40BFD7327786DA86")
          val expect = \/-((ReqId(16),\/-(verifiedEventsFromJson("""{"#":752,"event":{"CustomReqTypeUpdate":{"id":1,"values":[{"imp":true}]}}, "createdAt":"2019-09-27T06:18:51.853Z"}"""))))
          assertResponse(ReqTypeImplicationMod)(bin, expect)
        }
      }
    }

    // =================================================================================================================
    "push" - {
      import webSocket.push.codec

      "v1.0" - {

        "GenericReqTitleSet" - {
          val bin    = BinaryData.fromHex("010083E81A0009016C04626C6168E0B0318F5D00A9622600060CF606")
          val expect = Event.GenericReqTitleSet(9, "blah").verified(1000, Instant.parse("2019-09-28T10:10:56.644Z"))
          assertDecodeOk(codec)(bin, expect)
        }

        "ReqTagsPatch" - {
          val bin    = BinaryData.fromHex("01008162246700160200040046010013E0B0318F5D00A9622600060CF606")
          val expect = Event.ReqTagsPatch(22, nesd[ApplicableTagId](4, 70)(19)).verified(354, Instant.parse("2019-09-28T10:10:56.644Z"))
          assertDecodeOk(codec)(bin, expect)
        }

      }
    }

  }
}
