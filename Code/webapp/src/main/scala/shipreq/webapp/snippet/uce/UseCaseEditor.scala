package shipreq.webapp
package snippet.uce

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.{JsExp, JsCmd, JsCmds}
import net.liftweb.util.Helpers.nextFuncName
import JsCmds.Noop

import app.{AppConfig, Defaults, DI, RequestVars}
import db.{UseCaseSummary, UseCaseHeader}
import lib.{Misc, SnippetHelpers, Locks, StaticSnippetHelpers}
import lib.ScalazSubset._
import lib.Types._
import feature.uc._
import feature.uc.change._
import feature.uc.field._
import feature.uc.persist.{UseCasePersistence, UseCaseSaveCheckpoint}
import feature.validation.VFailure
import util.JsExt.JqFocus

object UseCaseEditor {

  case class State(uc: UseCase, prevSave: Option[UseCaseSaveCheckpoint], saveEnabled: Boolean) {
    def currentRevision: Short = prevSave.fold(0: Short)(_.rec.rev)
  }
  object State {
    def apply(cp: UseCaseSaveCheckpoint): State = State(cp.uc, Some(cp), false)
  }

  case class UcModifier(
    updateFn: UseCaseUpdater => UcUpdateResult,
    nopFn: Option[Renderer => JsCmd],
    focusOnErr: Option[JsExp],
    errFn: Option[VFailure => JsCmd] = None)
}

import UseCaseEditor._

// =====================================================================================================================

object UseCaseEditorDemo {

  @inline private def ucs1 = new UseCaseSummary((-1).tag, (1:Short).tag, "DEMO", Misc.currentTimeAsIso8601Str)
  @inline private def ucs2 = new UseCaseSummary((-2).tag, (2:Short).tag, "Request Refund", Misc.currentTimeAsIso8601Str)
  @inline private def ucs3 = new UseCaseSummary((-3).tag, (3:Short).tag, "Update Profile", Misc.currentTimeAsIso8601Str)

  def useCase = {
    import shipreq.base.util.BiMap
    import feature.uc.step._, feature.uc.text._, FreeTextTerms._, scalaz.Name
    import Defaults._

    val h = UseCaseHeader("Purchase Things".tag)
    val fl = fieldList.value.fields
    val ncf = Misc.filterCovar[NormalCourseField](fl).head
    val ecf = Misc.filterCovar[ExceptionCourseField](fl).head
    val fg = Misc.filterCovar[FlowGraphField](fl).head
    def TF(defn: TextFieldDefinition) = Misc.findTextField(defn, fl).get

    val fvs = List(
      TF(tfDescription) ~> FreeText(List(PlainText("This is a demonstration of the use case editor. Feel free to experiment.\nYou can learn more about its capabilities by clicking \"Cool Features\" in the top-right of the screen.\n\nThis demonstrative use case was written by an imaginary junior and has a number of problems. Can you spot them? Can you think of any improvements? Try amending it and see how easy it is, and how productive you become!")))
      , TF(tfActors) ~> FreeText(List(PlainText("* Actor #1\n* Actor #2\n* Actor #3")))
      , TF(tfPreConditions) ~> FreeText(List(PlainText("These references to "), StepRef("F898146860317OBUEZU".tag, "1.0.3".tag), PlainText(", "), StepRef("F898146860378T2CDS3".tag, "1.0.8".tag), PlainText(" and "), StepRef("F898146860441DIWTJY".tag, "1.1".tag), PlainText(" will always point to the same steps. Try inserting or removing a step and watch what happens.")))
      , TF(tfPostConditions) ~> FreeText(List(PlainText("Need to use math? Not a problem and looks great to boot. "), MathTexTerm("c = \\sqrt {a^2 + b^2}")))
      , ncf ~> StepFieldValue(ncf,StepTree(List(StepNode("F898146860208051SD4".tag,0,0,List(StepNode("F898146860207CBXT4K".tag,1,1,Nil),StepNode("F898146860326ESMACD".tag,1,2,Nil),StepNode("F898146860317OBUEZU".tag,1,3,Nil),StepNode("F898146860308LAZORN".tag,1,4,Nil),StepNode("F898146860344KFAM03".tag,1,5,Nil),StepNode("F898146860355HLDSBG".tag,1,6,Nil),StepNode("F898146860367NLMUJJ".tag,1,7,Nil),StepNode("F898146860378T2CDS3".tag,1,8,List(StepNode("F8981468603891PSEIO".tag,2,1,Nil),StepNode("F898146860402X02UVB".tag,2,2,Nil),StepNode("F898146860415UGLO42".tag,2,3,Nil))),StepNode("F89814686042600XESQ".tag,1,9,Nil))),StepNode("F898146860441DIWTJY".tag,0,1,List(StepNode("F898146860452NJSJNZ".tag,1,1,Nil),StepNode("F898146860473TX0Q1B".tag,1,2,Nil),StepNode("F898146860484UMRQTF".tag,1,3,Nil),StepNode("F898146860495S03ZBS".tag,1,4,Nil))))),Map("F898146860495S03ZBS".tag->StepText(FreeText(List(PlainText("System creates a new invoice with content matching the one selected."))),None,Some(FlowToClause(Map("F898146860378T2CDS3".tag->"1.0.8".tag)))),"F898146860326ESMACD".tag->StepText(FreeText(List(PlainText("System presents Customer with things in stock."))),Some(FlowFromClause(Map("F898146860355HLDSBG".tag->"1.0.6".tag))),Some(FlowToClause(Map("F898146860441DIWTJY".tag->"1.1".tag)))),"F898146860355HLDSBG".tag->StepText(FreeText(List(PlainText("Customer may add more things."))),None,Some(FlowToClause(Map("F898146860326ESMACD".tag->"1.0.2".tag)))),"F898146860415UGLO42".tag->StepText(FreeText(List(PlainText("List of things and associated quantities."))),None,None),"F898146860441DIWTJY".tag->StepText(FreeText(List(PlainText("Customer wishes to repeat previous purchase."))),Some(FlowFromClause(Map("F898146860326ESMACD".tag->"1.0.2".tag))),None),"F898146860367NLMUJJ".tag->StepText(FreeText(List(PlainText("Customer indicates they are finished selecting things."))),None,None),"F898146860208051SD4".tag->StepText(FreeText(List(PlainText("Customer purchases things."))),None,None),"F898146860452NJSJNZ".tag->StepText(FreeText(List(PlainText("Customer indicates thus."))),None,None),"F898146860484UMRQTF".tag->StepText(FreeText(List(PlainText("Customer selects one."))),None,Some(FlowToClause(Map("F898146860506XT0ZUD".tag->"1.E.1".tag)))),"F898146860308LAZORN".tag->StepText(FreeText(List(PlainText("System prompts for quantity."))),None,None),"F8981468603891PSEIO".tag->StepText(FreeText(List(PlainText("Customer name."))),None,None),"F898146860402X02UVB".tag->StepText(FreeText(List(PlainText("Purchase date."))),None,None),"F898146860344KFAM03".tag->StepText(FreeText(List(PlainText("Customer enters quantity."))),None,None),"F898146860317OBUEZU".tag->StepText(FreeText(List(PlainText("Customer selects a thing."))),None,None),"F898146860207CBXT4K".tag->StepText(FreeText(List(PlainText("Customer indicates wish to purchase things."))),None,None),"F89814686042600XESQ".tag->StepText(FreeText(List(PlainText("Customer confirms purchase."))),None,None),"F898146860473TX0Q1B".tag->StepText(FreeText(List(PlainText("System presents Customer with their previous invoices."))),Some(FlowFromClause(Map("F898146860525SNAD4C".tag->"1.E.1.2".tag))),None),"F898146860378T2CDS3".tag->StepText(FreeText(List(PlainText("System shows Customer an invoice with the following:"))),Some(FlowFromClause(Map("F898146860495S03ZBS".tag->"1.1.4".tag))),None)))
      , ecf ~> StepFieldValue(ecf,StepTree(List(StepNode("F898146860506XT0ZUD".tag,0,1,List(StepNode("F898146860515FTQ5MY".tag,1,1,Nil),StepNode("F898146860525SNAD4C".tag,1,2,Nil))))),Map("F898146860506XT0ZUD".tag->StepText(FreeText(List(PlainText("Thing is out of stock."))),Some(FlowFromClause(Map("F898146860484UMRQTF".tag->"1.1.3".tag))),None),"F898146860515FTQ5MY".tag->StepText(FreeText(List(PlainText("System removes the item from Customer's invoice."))),None,None),"F898146860525SNAD4C".tag->StepText(FreeText(List(PlainText("System informs the user thus."))),None,Some(FlowToClause(Map("F898146860473TX0Q1B".tag->"1.1.2".tag))))))
      , fg ~> ()
      , TF(tfUCRelationships) ~> FreeText(List(PlainText("This extends "), UseCaseRef(ucs2.number, ucs2.title), PlainText(".\nThis used to extend "), InvalidUseCaseRef((4: Short).tag, None), PlainText(" but that use case was deleted.")))
      , TF(tfConstraints) ~> FreeText.empty
      , TF(tfFreqOfUse) ~> FreeText.empty
      , TF(tfSpecialReqs) ~> FreeText.empty
      , TF(tfAssumptions) ~> FreeText.empty
      , TF(tfNotesAndIssues) ~> FreeText.empty
    )
    val sls = Name(BiMap[LocalStepId, StepLabel]("F898146860495S03ZBS".tag->"1.1.4".tag,"F898146860326ESMACD".tag->"1.0.2".tag,"F898146860355HLDSBG".tag->"1.0.6".tag,"F898146860415UGLO42".tag->"1.0.8.c".tag,"F898146860441DIWTJY".tag->"1.1".tag,"F898146860367NLMUJJ".tag->"1.0.7".tag,"F898146860208051SD4".tag->"1.0".tag,"F898146860452NJSJNZ".tag->"1.1.1".tag,"F898146860484UMRQTF".tag->"1.1.3".tag,"F898146860308LAZORN".tag->"1.0.4".tag,"F8981468603891PSEIO".tag->"1.0.8.a".tag,"F898146860402X02UVB".tag->"1.0.8.b".tag,"F898146860506XT0ZUD".tag->"1.E.1".tag,"F898146860344KFAM03".tag->"1.0.5".tag,"F898146860317OBUEZU".tag->"1.0.3".tag,"F898146860515FTQ5MY".tag->"1.E.1.1".tag,"F898146860207CBXT4K".tag->"1.0.1".tag,"F89814686042600XESQ".tag->"1.0.9".tag,"F898146860473TX0Q1B".tag->"1.1.2".tag,"F898146860525SNAD4C".tag->"1.E.1.2".tag,"F898146860378T2CDS3".tag->"1.0.8".tag))
    UseCase.as(ucs1.number, h, fvs, sls)
  }

  val relations: UseCaseRelations = CachedUseCaseRelations(List(ucs1, ucs2, ucs3))
  val state = State(useCase, None, false)
  val changeConstraint = Some(LimitTotalNumberOfSteps(AppConfig.DemoUseCaseMaxSteps))
}

// =====================================================================================================================

object UseCaseEditorFns extends StaticSnippetHelpers with DI {

  def loadLatest(ucId: UseCaseIdentId): (State, UseCaseRelations) = requireResult_!(for {
      lock   <- Locks.UseCaseNumbers.readM(RequestVars.ProjectId.get.value)
      dao    <- daoProvider.forTransaction
      ucRec  <- Box(dao.findUseCaseLatestRev(ucId)) ~> NotFoundResponse()
    } yield {
      val rels = CachedUseCaseRelations(RequestVars.UseCases.get)
      val cp = UseCasePersistence.load(ucRec).useRels(rels).run(dao, lock)
      (State(cp), rels)
    })

  def allowSave(before: State, after: UseCase): Boolean = before.prevSave match {
    case None     => false
    case Some(cp) => !UseCaseEquality.uc.equal(cp.uc, after)
  }
}

import UseCaseEditorFns._

class UseCaseEditor(initialState: UseCaseEditor.State, val rels: UseCaseRelations, val changeConstraint: Option[ChangeConstraint])
  extends StatefulSnippet with SnippetHelpers {

  // Constructor for demo page
  def this() = this(UseCaseEditorDemo.state, UseCaseEditorDemo.relations, UseCaseEditorDemo.changeConstraint)

  // Constructor for real page
  def this(p: (State, UseCaseRelations)) = this(p._1, p._2, None)
  def this(ucId: UseCaseIdentId) = this(loadLatest(ucId))

  private var state__ = initialState
  @inline final def state = state__
  @inline final def uc = state.uc
  @inline final def uch = uc.header
  @inline final def fields = uc.fields
  @inline final def fieldValues = uc.fieldValues

  val textFieldIds: Map[Field, LocalTextFieldId] =
    Misc.filterCovar[TextField](fields)
    .map(f => (f -> nextFuncName.tag[IsLocalTextFieldId]))
    .toMap

  private var renderer__ = Renderer(state, textFieldIds, update, state.prevSave.map(_ => save _))
  private var ucUpdater__ = UseCaseUpdater(uc, rels)
  @inline final def renderer = renderer__
  @inline final def ucUpdater = ucUpdater__

  protected def setState(newState: State): Unit = {
    state__ = newState
    renderer__ = renderer__.copy(state = newState)
    ucUpdater__ = ucUpdater__.copy(uc = newState.uc)
  }

  override def dispatch = { case _ => renderer.render }

  def update(m: UcModifier): JsCmd = {
    val r1 = m.updateFn(ucUpdater)
    val r2 = changeConstraint.fold(r1)(_ apply r1)
    r2 match {

      case a@Changed(newUc, changes) =>
        setState(State(newUc, state.prevSave, allowSave(state, newUc)))
        renderer.jsRespondToChanges(changes)

      case NoChange =>
        m.nopFn.fold(Noop)(_(renderer))

      case ChangeFailure(vf) =>
        val a = m.errFn.fold(renderer.jsRespondChangeFailure(vf))(_(vf))
        val b = m.focusOnErr.fold(Noop)(_ ~> JqFocus)
        a |+| b
    }
  }

  def save(): JsCmd = state.prevSave match {
    case Some(cp) =>
      daoProvider.withTransaction(dao => {
        val lock = Locks.SingleUseCase.writeP(cp, cp)
        UseCasePersistence.save(uc, cp, lock, dao) match {
          case Some(cp) =>
            setState(State(cp))
            jsPostSave
          case None =>
            jsPostSaveNop
        }
      })
    case None => shouldNeverHappen_!("Save failed. Ident missing.")
  }

  def jsPostSave: JsCmd = jsPostSaveNop & renderer.jsUpdateRevision
  def jsPostSaveNop: JsCmd = renderer.jsEnableSaveButton(false)

}
