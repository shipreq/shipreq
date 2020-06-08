package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.filter.{CompiledFilter, Filter, FilterAlgebra}
import shipreq.webapp.base.text._
import Event._
import SampleProject7.{project => project0}
import WebappTestUtil._
import UnsafeTypes._

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
