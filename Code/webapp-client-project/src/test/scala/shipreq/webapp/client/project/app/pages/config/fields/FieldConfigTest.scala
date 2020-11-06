package shipreq.webapp.client.project.app.pages.config.fields

import shipreq.base.util.{Disabled, Enabled, Exclusive}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.config.Buttons
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test.PrepareEnv
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._
import shipreq.webapp.member.test.project.{SampleProject6, SampleProject7}
import utest._
import utest.framework.TestPath

object FieldConfigTest extends TestSuite {
  import FieldConfigTestDsl._
  import Buttons.{displayFailure => displayButtonFailure}
  import shipreq.webapp.member.test.project.SampleProject7.Values._

  PrepareEnv()

  private def runActions(project: Project)(a: *.Actions)(implicit tp: TestPath): Unit =
    runPlan(project)(Plan.action(a))

  private def runPlan(project: Project)(p: *.Plan)(implicit tp: TestPath): Unit = {
    import ProjectSpaTestDsl._

    val name = p.name.fold(tp.value.mkString("Test: ", ".", ""))(_.value)

    ProjectSpaTestDsl.runTest(
      liftFieldConfigPageTests(p).asAction(name),
      page = Page.CfgFields,
      project = project)
  }

//  @inline private implicit def autoSomeEnabled(e: Enabled): Option[Enabled] = Some(e)
//  @inline private implicit def autoSomeString(s: String): Option[String] = Some(s)

  private def testFieldListView()(implicit tp: TestPath) =
    runActions(SampleProject6.project)(
      *.emptyAction
        +> filterDead.assert(HideDead)
        +> fieldList.assert(
        "Description",
        "Major Feature",
        "Priority",
        StaticField.NormalAltStepTree.name,
        StaticField.ExceptionStepTree.name,
        StaticField.StepGraph.name,
        "Status",
        "Notes")
        +> fieldDetail("Description").assert("MF, UC—OptionalOther—Not applicable")
        +> fieldDetail("Notes").assert("BR—Not applicableOther—Optional")
        +> fieldDetail("Priority").assert("All—Mandatory")

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> fieldList.assert(
        "Description",
        "Major Feature",
        "Priority",
        "Reporter", // dead
        StaticField.NormalAltStepTree.name,
        StaticField.ExceptionStepTree.name,
        StaticField.StepGraph.name,
        "Released", // dead
        "Status",
        "Notes")
        +> fieldDetail("Description").assert("MF, SI, UC—OptionalOther—Not applicable")
        +> fieldDetail("Notes").assert("BR—Not applicableOther—Optional")
        +> fieldDetail("Priority").assert("All—Mandatory")
    )

  private def testTextFieldEdit()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Description")
        +> filterDead.assert(HideDead)
        +> editorName.assert("Description")
        +> editorNameError.assert.empty
        +> ruleResItems(0).assert("Mandatory", "Not applicable", "Optional")
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional"),
        RuleRow.other("BR, CO, FR", "Not applicable"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> editorName.assert("Description")
        +> editorNameError.assert.empty
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setEditorName("")
        +> editorNameError.assert("Cannot be blank.")

        >> setEditorName("Component")
        +> editorNameError.assert("Already in use.")

        >> setEditorName("COMponENT")
        +> editorNameError.assert("Already in use.")

        >> setEditorName("Description")

        >> addEditorRule
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow.New,
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(2, "MF")
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional", reqTypesError = "Defined elsewhere: MF"),
        RuleRow("MF", "Optional", reqTypesError = "Defined elsewhere: MF"),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(2, "xxx")
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow("XXX", "Optional", reqTypesError = "XXX is not a valid req type."),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> setRuleReqTypes(2, "DD")
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow("DD", "Optional", reqTypesError = "DD has been deleted."),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, cancel = Enabled, save = Disabled))

        >> setRuleReqTypes(2, "co fr")
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow("CO FR", "Optional"),
        RuleRow.other("BR, DD", "Not applicable"))

        >> delEditorRule(2)
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))

        >> clickFilterDead
        +> filterDead.assert(HideDead)
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional"),
        RuleRow.other("BR, CO, FR", "Not applicable"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> addEditorRule
        >> addEditorRule
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional"),
        RuleRow.New,
        RuleRow.New,
        RuleRow.other("BR, CO, FR", "Not applicable"))

        >> setRuleReqTypes(1, "co")
        >> setRuleReqTypes(2, "fr, br")
        >> setRuleReqRes(1, "Mandatory")
        +> editorRules.assert(
        RuleRow("MF, UC", "Optional"),
        RuleRow("CO", "Mandatory"),
        RuleRow("FR, BR", "Optional"),
        RuleRow.other("", "Not applicable"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, cancel = Enabled, save = Enabled))

        >> clickSaveButton
        +> editorRules.assert(
        RuleRow("BR, FR, MF, UC", "Optional"),
        RuleRow("CO", "Mandatory"),
        RuleRow.other("", "Not applicable"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> editorName.assert("Description")
        +> editorNameError.assert.empty
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("BR, FR, MF, UC", "Optional"),
        RuleRow("CO", "Mandatory"),
        RuleRow.other("DD", "Not applicable"))

        >> delEditorRule(1)
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("CO", "Mandatory"),
        RuleRow.other("BR, DD, FR, MF, UC", "Not applicable"))

        >> setRuleReqRes(1, "Not applicable")
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("CO", "Not applicable"),
        RuleRow.other("BR, DD, FR, MF, UC", "Not applicable"))

        >> clickSaveButton
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow.other("BR, CO, DD, FR, MF, UC", "Not applicable"))

        >> clickFilterDead
        +> filterDead.assert(HideDead)
        +> editorRules.assert(RuleRow.all("Not applicable"))
    )

  private def testTextFieldDead()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Description")
        +> filterDead.assert(HideDead)

        >> clickDeleteButton
        +> filterDead.assert(ShowDead)
        +> editorName.assert("Description")
        +> editorNameError.assert.empty
        +> editorRules.assert(
        RuleRow("SI", "Optional", dead = true),
        RuleRow("MF, UC", "Optional"),
        RuleRow.other("BR, CO, DD, FR", "Not applicable"))
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))
        +> editorEditables.assert(0)
    )

  private def testImpFieldCreate(p: Project)(implicit tp: TestPath) =
    runActions(p)(

      clickNew("Implication field")
        +> filterDead.assert(HideDead)
        +> messageHeader.assert.empty
        +> editorDropdown.assert.contains("")
        +> editorDropdownError.assert(true) // blank
        +> editorDropdownItems.assert("BR: Business Rule", "CO: Constraint", "FR: Functional Requirement", "UC: Use Case")
        +> editorRules.assert(RuleRow.all("Optional"))
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> messageHeader.assert.empty
        +> editorDropdown.assert.contains("")
        +> editorDropdownError.assert(true) // blank
        +> editorDropdownItems.assert("BR: Business Rule", "CO: Constraint", "FR: Functional Requirement", "UC: Use Case")
        +> editorRules.assert(RuleRow.all("Optional"))
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> setEditorDropdown("FR: Functional Requirement")
        +> messageHeader.assert.empty
        +> editorDropdown.assert.contains("FR: Functional Requirement")
        +> editorDropdownError.assert(false)
        +> editorDropdownItems.assert("BR: Business Rule", "CO: Constraint", "FR: Functional Requirement", "UC: Use Case")
        +> editorRules.assert(RuleRow.all("Optional"))
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Enabled))

        >> clickFilterDead
        +> filterDead.assert(HideDead)

        >> clickSaveButton
        +> fieldList.valueBy(_.last).assert("Functional Requirement")
        +> fieldDetail("Functional Requirement").assert("All—Optional")
        +> editorDropdown.assert.empty
        +> editorRules.assert(RuleRow.all("Optional"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
    )

  private def testImpFieldCreateCant()(implicit tp: TestPath) =
    runActions(
      applyEventsSuccessfully(
        SampleProject7.project,
        Event.CustomReqTypeDeleteSoft(br),
        Event.CustomReqTypeDeleteSoft(co),
        Event.CustomReqTypeDeleteSoft(fr),
        Event.FieldCustomImpCreate(2000, uc, CustomImpFieldGD(FieldReqTypeRules.optional)),
      )
    )(
      clickNew("Implication field")
        +> filterDead.assert(HideDead)
        +> messageHeader.assert.contains("No req types available")
        +> editorDropdown.assert.empty
        +> editorRules.size.assert(0)
        +> buttonsEnabled.assert(Buttons(cancel = Enabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> messageHeader.assert.contains("No req types available")
        +> editorDropdown.assert.empty
        +> editorRules.size.assert(0)
        +> buttonsEnabled.assert(Buttons(cancel = Enabled))
    )

  private def testImpFieldUpdate()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Major Feature")
        +> filterDead.assert(HideDead)
        +> editorDropdown.assert.empty
        +> editorRules.assert(RuleRow.all("Optional"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
    )

  private def testImpFieldDead()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Major Feature")
        +> filterDead.assert(HideDead)

        >> addEditorRule
        >> setRuleReqTypes(0, "MF")
        >> setRuleReqRes(0, "Mandatory")
        >> clickSaveButton

        >> clickDeleteButton
        +> filterDead.assert(ShowDead)
        +> editorRules.assert(
        RuleRow("SI", "Not applicable", dead = true),
        RuleRow("MF", "Mandatory"),
        RuleRow.other("BR, CO, DD, FR, UC", "Optional"))
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))
        +> editorEditables.assert(0)
    )

  private def testTagFieldCreate()(implicit tp: TestPath) =
    runActions(
      applyEventsSuccessfully(
        SampleProject7.project,

        Event.ApplicableTagCreate(4567.AT, ApplicableTagGD(
          key                = "omfg",
          desc               = None,
          colour             = None,
          applicableReqTypes = ApplicableReqTypes.empty,
          parents            = Map.empty,
          children           = Vector.empty)),

        Event.ApplicableTagCreate(4568.AT, ApplicableTagGD(
          key                = "wow",
          desc               = None,
          colour             = None,
          applicableReqTypes = ApplicableReqTypes.empty,
          parents            = Map.empty,
          children           = Vector.empty)),

        Event.TagGroupCreate(4569.TG, TagGroupGD(
          name        = "Surprise",
          desc        = None,
          exclusivity = Exclusive,
          parents     = Map.empty,
          children    = Vector(4568.AT, 4567.AT))),

        Event.TagGroupCreate(4777.TG, TagGroupGD(
          name        = "Nada",
          desc        = None,
          exclusivity = Exclusive,
          parents     = Map.empty,
          children    = Vector())),
      )
    )(

      clickNew("Tag field")
        +> filterDead.assert(HideDead)
        +> editorDropdown.assert.contains("")
        +> editorDropdownError.assert(true) // blank
        +> editorDropdownItems.assert("Nada", "Surprise")
        +> editorRules.assert(RuleRow.all("Optional"))
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> editorDropdown.assert.contains("")
        +> editorDropdownError.assert(true) // blank
        +> editorDropdownItems.assert("Nada", "Surprise")
        +> editorRules.assert(RuleRow.all("Optional"))
        +> ruleResItems(0).assert("Default to…", "Mandatory", "Not applicable", "Optional")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> setEditorDropdown("Surprise")
        +> editorDropdown.assert.contains("Surprise")
        +> editorDropdownError.assert(false)
        +> editorDropdownItems.assert("Nada", "Surprise")
        +> editorRules.assert(RuleRow.all("Optional"))
        +> ruleDefaultItems(0).assert()
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Enabled))

        >> setRuleReqRes(0, "Default to…")
        +> editorRules.assert(RuleRow.AllEmptyDefault)
        +> ruleDefaultItems(0).assert("omfg", "wow")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> setRuleDefault(0, "wow")
        +> editorRules.assert(RuleRow.all("Default to…", default = "wow"))
        +> ruleDefaultItems(0).assert("omfg", "wow")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Enabled))

        >> setEditorDropdown("Nada")
        +> editorRules.assert(RuleRow.AllEmptyDefault)
        +> ruleDefaultItems(0).assert()
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Disabled))

        >> setEditorDropdown("Surprise")
        +> editorRules.assert(RuleRow.all("Default to…", default = "wow"))
        +> ruleDefaultItems(0).assert("omfg", "wow")
        +> buttonsEnabled.assert(Buttons(cancel = Enabled, save = Enabled))

        >> clickFilterDead
        +> filterDead.assert(HideDead)

        >> clickSaveButton
        +> fieldList.valueBy(_.last).assert("Surprise")
        +> fieldDetail("Surprise").assert("All—Default to wow")
        +> editorDropdown.assert.empty
        +> editorRules.assert(RuleRow.all("Default to…", default = "wow"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
    )

  private def testTagFieldCreateCant()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(
      clickNew("Tag field")
        +> filterDead.assert(HideDead)
        +> messageHeader.assert.contains("No tag groups available")
        +> editorDropdown.assert.empty
        +> editorRules.size.assert(0)
        +> buttonsEnabled.assert(Buttons(cancel = Enabled))

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> messageHeader.assert.contains("No tag groups available")
        +> editorDropdown.assert.empty
        +> editorRules.size.assert(0)
        +> buttonsEnabled.assert(Buttons(cancel = Enabled))
    )

  private def testTagFieldUpdate()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Priority")
        +> filterDead.assert(HideDead)
        +> editorDropdown.assert.empty
        +> editorRules.assert(
        RuleRow("BR", "Default to…", default = "pri=med"),
        RuleRow("CO", "Not applicable"),
        RuleRow("FR, MF", "Mandatory"),
        RuleRow.other("UC", "Optional"))
        +> buttonsEnabled.assert(Buttons(delete = Enabled, close = Enabled, save = Disabled))
    )

  private def testTagFieldDead()(implicit tp: TestPath) =
    runActions(SampleProject7.project)(

      selectField("Priority")
        +> filterDead.assert(HideDead)

        >> clickDeleteButton
        +> filterDead.assert(ShowDead)
        +> editorRules.assert(
        RuleRow("BR", "Default to…", default = "pri=med"),
        RuleRow("CO", "Not applicable"),
        RuleRow("FR, MF", "Mandatory"),
        RuleRow.other("DD, SI, UC", "Optional"))
        +> buttonsEnabled.assert(Buttons(restore = Enabled, close = Enabled))
        +> editorEditables.assert(0)
    )

  private def testStaticFieldMandatory()(implicit tp: TestPath) =
    runActions(SampleProject6.project)(
      selectField(StaticField.NormalAltStepTree.name)
        +> buttonsEnabled.assert(Buttons(close = Enabled))
        +> editorEditables.assert(0)
    )

  private def testStaticFieldOptional()(implicit tp: TestPath) = {
    val newItems = List(
      StaticField.ImplicationGraph.name,
      StaticField.OtherTags.name,
      StaticField.AllTags.name,
      "Implication field",
      "Tag field",
      "Text field"
    )

    runActions(SampleProject6.project)(
      newChoices.assert.equalIgnoringOrder(newItems: _*)
        +> selectField(StaticField.StepGraph.name)
        +> newChoices.assert.equalIgnoringOrder(newItems: _*)
        +> buttonsEnabled.assert(Buttons(remove = Enabled, close = Enabled))
        +> editorEditables.assert(0)

        >> clickRemoveButton
        +> fieldList.assert(
        "Description",
        "Major Feature",
        "Priority",
        StaticField.NormalAltStepTree.name,
        StaticField.ExceptionStepTree.name,
        "Status",
        "Notes")
        +> buttonsEnabled.assert(Buttons(add = Enabled, close = Enabled))
        +> editorEditables.assert(0)

        >> clickCloseButton
        +> newChoices.assert.equalIgnoringOrder(StaticField.StepGraph.name :: newItems: _*)

        >> clickNew(StaticField.StepGraph.name)
        +> buttonsEnabled.assert(Buttons(add = Enabled, close = Enabled))

        >> clickAddButton
        +> newChoices.assert.equalIgnoringOrder(newItems: _*)
        +> fieldList.assert(
        "Description",
        "Major Feature",
        "Priority",
        StaticField.NormalAltStepTree.name,
        StaticField.ExceptionStepTree.name,
        "Status",
        "Notes",
        StaticField.StepGraph.name)
        +> buttonsEnabled.assert(Buttons(remove = Enabled, close = Enabled))
        +> editorEditables.assert(0)
    )
  }

  private def testNoOtherTags()(implicit tp: TestPath) =
    runActions(
      applyEventsSuccessfully(
        SampleProject7.project,
        Event.TagGroupCreate(1000.TG, TagGroupGD("X", ∅, Exclusive, ∅, Vector(misc1, misc2))),
        Event.FieldCustomTagCreate(1000.CFTag, 1000.TG, CustomTagFieldGD.ValueForFieldReqTypeRules(FieldReqTypeRules.empty))
      )
    )(
      *.emptyAction
        +> fieldDetail(StaticField.OtherTags.name).assert("Displays tags not assigned to a field.(Currently no tags fit this criteria.)")

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> fieldDetail(StaticField.OtherTags.name).assert("Displays tags not assigned to a field.(Currently no tags fit this criteria.)")

        >> selectField(StaticField.OtherTags.name)
        +> buttonsEnabled.assert(Buttons(remove = Enabled, close = Enabled))
        +> editorEditables.assert(0)
    )

  private def testOtherTags()(implicit tp: TestPath) =
    runActions(
      applyEventsSuccessfully(
        SampleProject7.project,
        Event.TagDelete(misc1),
        Event.TagDelete(misc2),
        Event.TagDelete(priLow),
        Event.FieldCustomDelete(priField),
      )
    )(
      *.emptyAction
        +> fieldDetail(StaticField.OtherTags.name).assert("Displays tags not assigned to a field.pri=highpri=med")

        >> clickFilterDead
        +> filterDead.assert(ShowDead)
        +> fieldDetail(StaticField.OtherTags.name).assert("Displays tags not assigned to a field.misc1misc2pri=highpri=lowpri=med")

        >> selectField(StaticField.OtherTags.name)
        +> buttonsEnabled.assert(Buttons(remove = Enabled, close = Enabled))
        +> editorEditables.assert(0)
    )

  override def tests = Tests {

    "fieldList" - {
      "view" - testFieldListView()
    }

    "static" - {
      "mandatory"   - testStaticFieldMandatory()
      "optional"    - testStaticFieldOptional()
      "noOtherTags" - testNoOtherTags()
      "otherTags"   - testOtherTags()
    }

    "textField" - {
      "edit" - testTextFieldEdit()
      "dead" - testTextFieldDead()
    }

    "impField" - {
      def p = SampleProject7.project
      "create"       - testImpFieldCreate(p)
      "createCant"   - testImpFieldCreateCant()
      "createDeadMF" - testImpFieldCreate(applyEventSuccessfully(p, Event.FieldCustomDelete(mfField)))
      "update"       - testImpFieldUpdate()
      "dead"         - testImpFieldDead()
    }

    "tagField" - {
      "create"     - testTagFieldCreate()
      "createCant" - testTagFieldCreateCant()
      "update"     - testTagFieldUpdate()
      "dead"       - testTagFieldDead()
    }

  }
}
