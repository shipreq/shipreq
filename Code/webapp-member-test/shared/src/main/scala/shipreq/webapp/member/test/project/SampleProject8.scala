package shipreq.webapp.member.test.project

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.filter.{CompiledFilter, Filter, FilterAlgebra}
import shipreq.webapp.member.project.text._
import shipreq.webapp.member.test.WebappTestUtil
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.SampleProject7.{project => project0}
import shipreq.webapp.member.test.project.UnsafeTypes._

object SampleProject8 {

  trait Values extends SampleProject7.Values
  object Values extends Values
  import Values._

  lazy val project = WebappTestUtil.applyEventsSuccessfully(project0
    , ApplicableTagUpdate(priMed, ApplicableTagGD.ValueForColour(Colour("#123456")))
    , ApplicableTagUpdate(priHigh, ApplicableTagGD.ValueForColour(Colour("#ff0000")))
    , ReqTagsPatch(brs(2), nesd()(priHigh, priMed))
    )

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
  lazy val filterValidator = FilterAlgebra.validate(project.config)
  lazy val filterCompiler = FilterDead.memo(Filter.Valid.compiler(project, plainText, textSearch, _, applyFilterDeadToReqs = false))

  def needFilter(str: String, filterDead: FilterDead): CompiledFilter = {
    val f = Filter.parseAndValidate(str, filterValidator).getOrThrow().get
    filterCompiler(filterDead)(f)
  }
}
