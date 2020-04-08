package shipreq.webapp.client.project.app.pages.config.issues

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.Exclusive
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.app.Style.{issueConfig => *}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets


object OtherIssueSources {

  final case class Props(config: ProjectConfig,
                         pw    : ProjectWidgets.NoCtx,
                         router: Routes.RouterCtl,
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private case class MandatoryField(name: String, subtext: Option[String])

  private object MandatoryField {

    def derive(config: ProjectConfig): Iterator[MandatoryField] = {
      val liveReqTypes = config.reqTypes.liveIds
      val fieldRules   = config.fieldRules(HideDead)

      def liveExceptions(f: Field, mandatory: Boolean): Iterator[ReqType.Mnemonic] =
        MutableArray(
          f.fieldReqTypeRules
            .perReqType
            .keysIterator
            .filter(liveReqTypes.contains)
            .filter(fieldRules(_)(f.fieldId).isMandatory == mandatory)
            .map(config.reqTypes.need(_).mnemonic)
        ).sortBy(_.value).iterator

      config.fields.iterator().filter(_.live(config) is Live).flatMap { f =>

        val subtext: Option[String] =
          if (f.fieldReqTypeRules.otherwise.isMandatory) {
            val reqTypes = liveExceptions(f, mandatory = false)
            val sub =
              if (reqTypes.isEmpty)
                "" // No exception, everything is mandatory
              else
                UiText.sortedAndClause(reqTypes.map(_.value)) + " excepted"
            Some(sub)
          } else {
            val reqTypes = liveExceptions(f, mandatory = true)
            Option.when(reqTypes.nonEmpty)(
              UiText.sortedAndClause(reqTypes.map(_.value)) + " only")
          }

        subtext.map(_ => MandatoryField(config.fieldName(f.fieldId), subtext.filter(_.nonEmpty)))
      }
    }

    val builtInFields: List[MandatoryField] =
      List(
        SpecialBuiltInField.Title.name,
        "Use Case Steps",
      ).map(MandatoryField(_, Some("built-in")))
  }

  private def renderWithSubtext(main: String, sub: Option[String]) =
    TagMod(main, sub.whenDefined(s => <.span(*.otherSourcesSubtext, s"($s)")))

  private def render(p: Props): VdomNode = {

    def renderData[A](title : VdomNode,
                      page  : Routes.Page,
                      data  : TraversableOnce[A])(
                      sortBy: A => String,
                      render: A => TagMod) = {

      val content =
        if (data.isEmpty)
          <.div(*.otherSourcesNone, "None.")
        else
          <.ul(
            *.otherSourcesUL,
            MutableArray(data).sortBy(sortBy).iterator.toTagMod(a => <.li(*.otherSourcesLI, render(a))))

      <.div(*.otherSourcesContent,
        <.div(*.otherSourcesHeader, p.router.link(page)(title)),
        <.div(content))
    }

    val exclusiveTagGroups =
      renderData(
        title  = "Exclusive Tag Groups",
        page   = Routes.Page.CfgTags,
        data   = p.config.tags.tagGroupIterator().filter(t => t.live.is(Live) && t.exclusivity.is(Exclusive)))(
        sortBy = _.name,
        render = _.name)

    val reqTypesWithMandatoryImplication =
      renderData(
        title  = "Req Types with Mandatory Implication",
        page   = Routes.Page.CfgReqTypes,
        data   = p.config.reqTypes.custom.valuesIterator.filter(t => t.live.is(Live) && t.implication.is(Mandatory)))(
        sortBy = _.mnemonic.value,
        render = r => p.pw.reqTypeFull(r.id))

    val mandatoryFields =
      renderData(
        title  = "Mandatory Fields",
        page   = Routes.Page.CfgFields,
        data   = MandatoryField.derive(p.config) ++ MandatoryField.builtInFields)(
        sortBy = _.name,
        render = f => renderWithSubtext(f.name, f.subtext))

    <.section(
      <.div(*.sectionTitle, "Other Configurable Causes of Issues"),
      <.div(*.otherSources,
        <.div(
          exclusiveTagGroups,
          reqTypesWithMandatoryImplication,
        ),
        <.div(*.otherSourcesGap),
        <.div(
          mandatoryFields,
        )
      )
    )
  }

  val Component = ScalaComponent.builder[Props]("OtherIssueSources")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}