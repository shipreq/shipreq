package shipreq.webapp.base.test

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.VectorTree
import shipreq.base.util.VectorTree.{Location => Loc}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text._

/**
 * Builds on SampleProject #3 to add:
 *   - UC-1 with some steps.
 *
 * UC steps:
 *   - (#10) 1.0
 *   - (#11) 1.0.1
 *   - (#12) 1.0.2
 *   - (#13) 1.0.3
 *   - (#14) 1.1
 *   - (#15) 1.1.1
 */
object SampleProject4 {
  import DataImplicits._
  import UseCases.StepFlow

  trait Values extends SampleProject3.Values {
    val uc1 = UseCaseId(1203)

    val step10_label = "1.0"
    val step11_label = "1.0.1"
    val step12_label = "1.0.2"
    val step13_label = "1.0.3"
    val step14_label = "1.1"
    val step15_label = "1.1.1"
  }
  object Values extends Values
  import SampleProject.Values._

  lazy val project = {
    val p   = SampleProject3.project
    var pr  = p.content.reqs.pubids
    var ucs = emptyDataMap(UseCase)
    var sf  = StepFlow.emptyUniDir
    var ic  = p.idCeilings
    var rt  = p.content.reqText

    def addUseCase(id   : Int                            = -1,
                   title: Text.UseCaseTitle.OptionalText ,// = Text.empty,
                   ncac : UseCaseSteps.Tree              ,// = rootOnlyStepTree(),
                   ec   : UseCaseSteps.Tree              = VectorTree.empty,
                   live : Live                           = Live): UseCaseId = {

      val ucId = UseCaseId(if (id > 0) id else (ic.req + 1))
      ic = ic.copy(req = ic.req max ucId.value)

      val pos = pr.allocUC(ucId).consume1(pr = _).pos

      val uc = UseCase(ucId, pos, title, UseCaseSteps(ncac), UseCaseSteps(ec), live)
      ucs += uc

      ucId
    }

    def newStep(id   : Int,
                title: Text.UseCaseStep.OptionalText = Text.empty,
                live : Live                          = Live): UseCaseStep = {
      val i = UseCaseStepId(if (id > 0) id else (ic.useCaseStep + 1))
      ic = ic.copy(useCaseStep = ic.useCaseStep max i.value)
      UseCaseStep(i, title, live)
    }

    val ncac =
      VectorTree.empty
        .append(                newStep(10)                         )     // UC-n.0
        .insertAfter(Loc(0)   , newStep(11, title = "Get food")     ).get // UC-n.0.1
        .insertAfter(Loc(0, 0), newStep(12, title = "Put in mouth") ).get // UC-n.0.2
        .insertAfter(Loc(0, 1), newStep(13, title = "Still hungry?")).get // UC-n.0.3
        .append(                newStep(14, title = "Have no food") )     // UC-n.1
        .insertAfter(Loc(1)   , newStep(15, title = "Steal food")   ).get // UC-n.1.1

    val uc1 = addUseCase(title = "Eat food", ncac = ncac)
    // println(ncac.map(_.title.mkString(",")))

    rt = ReqData.Text.at(descField, uc1).set("This UC is about eating.")(rt)

    sf = sf.addPairs(13 -> 11, 15 -> 12)

    val p2 =
      (Project.name.set("Sample Project 4(+)") compose
        Project.reqs.set(Requirements(p.content.reqs.genericReqs, UseCases.Stateless(ucs, StepFlow BiDir sf).withState, pr)) compose
        Project.reqText.set(rt) compose
        Project.idCeilings.set(ic)
        ) (p)
    DataProp.project.allIncludingConfig assert p2
    p2
  }

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)

  lazy val projectWithOtherTags =
    applyEventSuccessfully(project, Event.FieldStaticAdd(StaticField.OtherTags))

  lazy val projectWithAllAndOtherTags =
    applyEventSuccessfully(projectWithOtherTags, Event.FieldStaticAdd(StaticField.AllTags))
}
