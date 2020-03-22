package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text._
import Event._
import SampleProject6.{project => project0}
import UnsafeTypes._

/**
 * Builds on SampleProject #6 with:
 *
 *   - Business Justification text field
 *       FR       : Optional
 *       CO       : N/A
 *       SI (dead): N/A
 *       Otherwise: Mandatory
 *
 *   - Alternatives text field
 *       SI (dead): Optional
 *       Otherwise: N/A
 *
 *   - Component text field
 *       CO FR    : Optional
 *       SI (dead): Optional
 *       Otherwise: N/A
 *
 *   - Priority tag field
 *       BR       : Default to pri=med
 *       CO       : N/A
 *       MF FR    : Mandatory
 *       Otherwise: Optional
 *
 *   - Released tag field (restored, used to be Dead)
 *       CO       : Default to pri=med (not in scope)
 *       Otherwise: Mandatory
 *
 *   - Status tag field
 *       BR CO    : Default to uat (dead)
 *       FR       : Default to uat2 (dead)
 *       SI (dead): Default to uat3 (dead)
 *       MF       : Default to wip
 *       Otherwise: Optional
 *
 *   - Version tag field
 *       MF       : N/A
 *       Otherwise: Default to pri=low (not in scope)
 *
 *   - BR-1: Must make moneh
 *   - BR-2: Must make moar moneh! #prod (note: #prod n/a to BR)
 *   - BR-3: CEO owns islands! #wip
 *   - MF-3: tag with #prod
 *   - MF-4: Business Justification = "x"
 *   - UC-1: tag with #v1.0 and #prod
 *
 *   - #prod now only applicable to UC & MF
 *
 * @since 2.1
 */
object SampleProject7 {

  trait Values extends SampleProject6.Values {
    val List(bizJustField, alternativesField, componentField) = List[CustomField.Text.Id](8, 9, 10)

    val brs = (0 to 10).iterator.map(i => GenericReqId(i + 1400)).toVector
  }

  object Values extends Values
  import Values._

  lazy val project = WebappTestUtil.applyEventsSuccessfully(project0,

    FieldCustomTextCreate(bizJustField, CustomTextFieldGD("Business Justification",
      FieldReqTypeRules.mandatory.optional(fr).notApplicable(co, si))),

    FieldCustomTextCreate(alternativesField, CustomTextFieldGD("Alternatives",
      FieldReqTypeRules.notApplicable.optional(si))),

    FieldCustomTextCreate(componentField, CustomTextFieldGD("Component",
      FieldReqTypeRules.notApplicable.optional(co, fr, si))),

    FieldCustomTagUpdate(priField, CustomTagFieldGD(
      FieldReqTypeRules.optional.mandatory(mf, fr).notApplicable(co).defaultTo(priMed)(br))),

    FieldCustomRestore(relField),

    FieldCustomTagUpdate(relField, CustomTagFieldGD(
      FieldReqTypeRules.mandatory.defaultTo(priMed)(co))),

    FieldCustomTagUpdate(statusField, CustomTagFieldGD(
      FieldReqTypeRules.optional.defaultTo(wip)(mf).defaultTo(uat)(br, co).defaultTo(uat2)(fr).defaultTo(uat3)(si))),

    Event.FieldCustomTagCreate(verField, verTG, CustomTagFieldGD(FieldReqTypeRules.defaultTo(priLow).notApplicable(mf))),

    GenericReqCreate(brs(1), br, GenericReqGD.ValueForTitle("Must make moneh")),
    GenericReqCreate(brs(2), br, GenericReqGD.ValueForTitle("Must make moar moneh")),
    GenericReqCreate(brs(3), br, GenericReqGD.ValueForTitle("CEO owns islands!")),

    ReqTagsPatch(brs(2), nesd()(prod)),
    ReqTagsPatch(brs(3), nesd()(wip)),
    ReqTagsPatch(mfs(3), nesd()(prod)),
    ReqTagsPatch(uc1   , nesd()(v10, prod)),

    ReqFieldCustomTextSet(mfs(4), bizJustField, Vector(Text.CustomTextField.Literal("x"))),

    ApplicableTagUpdate(prod, ApplicableTagGD.ValueForApplicableReqTypes(onlyReqTypes(uc, mf)))
  )

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
}
