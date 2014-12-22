package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react._, vdom.prefix_<^.{Tag => ReactTag, Modifier => TagMod, _}, ScalazReact._
import scalaz.effect.IO
import shipreq.webapp.base.data._
import Tag.Id

private[tags] object DetailPane {

  type Rels = Seq[Rel]

  case class Rel(id: Id, name: String, unlink: IO[Unit])

  case class Props(descEditor: ReactElement, children: Rels, parents: Rels) {

    def render: ReactElement =
      <.section(
        descPane,
        <.table(
          <.thead(<.tr(
            <.th("Parents"),
            <.th("Children"))),
          <.tbody(<.tr(
            <.td(parentsPane),
            <.td(childrenPane)))))

    def descPane: ReactElement =
      <.label(
        "Description:",
        descEditor)

    def parentsPane: ReactElement =
      relList(parents.sortBy(_.name), <.ul, "This is a top-level tag.")

    def childrenPane: ReactElement =
      relList(children, <.ol, "This has no children.")

    def relList(rels: Rels, container: ReactTag, noneMsg: String): ReactElement =
      if (rels.isEmpty)
        <.div(noneMsg)
      else
        container(
          rels.map(r =>
            <.li(
              ^.key := r.id.value,
              r.name,
              <.button(
                ^.marginLeft := "2ex",
                ^.onclick ~~> r.unlink,
                "Remove"))))

  }

  val Component = ReactComponentB[Props]("Cfg: Tag Detail")
    .render(_.render)
    .build
}