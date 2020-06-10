package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.UiText.EnglishIntExt
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

/** This isn't really an editor; it's read/only! But it's what appears in place of the editor. */
object StaticFieldEditor {

  final case class Props(field     : StaticField,
                         config    : ProjectConfig,
                         filterDead: FilterDead,
                         pw        : ProjectWidgets.NoCtx) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val ul    = <.ul(*.staticFieldUL)
  private val li    = <.li(*.staticFieldLI)
  private val liTag = <.li(*.staticFieldTagLI)

  private val mandatory =
    li("This field is mandatory and cannot be removed.")

  private def renderOtherTags(p: Props): VdomNode = {
    def renderTagList(ids: IterableOnce[ApplicableTagId]): TagMod =
      if (ids.iterator.isEmpty)
        EmptyVdom
      else
        <.ul(
          p.config.tags.sortTagIds(ids).toTagMod(id =>
            liTag(p.pw.tagSimple(id, includeDesc = true))))

    val allTags = p.config.liveTagFieldDistribution.notUsedInFields
    val (live, dead) = allTags.partition(p.config.tags.needApplicableTag(_).live is Live)

    ul(
      li("A field for any miscellaneous tags not assigned to other tag fields."),
      li(
        s"There are currently ${live.size.unitsOf("tag")} that fit this criteria.",
        renderTagList(live)
      ),
      li(
        s"When looking at deleted requirements, the following deleted tags are also included:",
        renderTagList(dead)
      ).when(dead.nonEmpty && p.filterDead.is(ShowDead)),
    )
  }

  private def render(p: Props): VdomNode = {
    def ul(lis: VdomNode*) =
      this.ul(
        lis.toTagMod(li(_)),
        mandatory.when(p.field.existence is Mandatory))

    val body =
      p.field match {

        case StaticField.NormalAltStepTree =>
          ul("Sequences of interactions that lead to a successful outcome.")

        case StaticField.ExceptionStepTree =>
          ul(
            "Conditions that prevent a successful outcome.",
            "Sequences of interactions that describe how exceptions will be handed.")

        case StaticField.ImplicationGraph =>
          ul("A graph depicting all requirements related by implication.")

        case StaticField.StepGraph =>
          ul("A graph depicting all possible flow paths through a use case.")

        case StaticField.AllTags =>
          ul(
            "Displays all tags in a single field, even those the appear in other tag fields.",
            "Allows users to edit all tags in a single place, regardless of which fields the tags are assigned to.")

        case StaticField.OtherTags =>
          renderOtherTags(p)
      }

    // div is for tests
    <.div(body)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
