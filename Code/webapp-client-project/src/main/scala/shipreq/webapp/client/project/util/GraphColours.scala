package shipreq.webapp.client.project.util

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.NonEmptyArraySeq
import shipreq.webapp.base.ui.widgets.Dropdown
import shipreq.webapp.client.project.app.Style.{reqgraphPage => *}
import shipreq.webapp.member.UiText
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig.Colours

object GraphColours {

  val key: Colours => Dropdown.ItemKey = {
    case Colours.ByReqType => "r"
    case Colours.ByTag(id) => "t" + id.value.toString
  }

  type Unsorted = (String, Dropdown.Item[Colours])

  val staticItems: ArraySeq[Unsorted] =
    ArraySeq(
      UiText.FieldNames.fieldType -> Dropdown.Item(
        key   = key(Colours.ByReqType),
        label = UiText.FieldNames.fieldType,
        value = Colours.ByReqType,
      ),
    )

  def pxOptions[A](pxTags           : Px[Tags],
                   pxFilterDead     : Px[FilterDead],
                   pxSelectedColours: Px[Option[Colours]]): Px[NonEmptyArraySeq[Dropdown.Item[Colours]]] =
    for {
      tags     <- pxTags
      fd       <- pxFilterDead
      selected <- pxSelectedColours
    } yield {

      var extraTagGroupId: Option[TagGroupId] =
        selected match {
          case None
             | Some(Colours.ByReqType) => None
          case Some(Colours.ByTag(id)) => Some(id)
        }

      var tagGroups =
        fd.filterFn.iteratorBy(tags.tagGroupIterator())(_.live)
          .tapEach(tg => if (extraTagGroupId.contains(tg.id)) extraTagGroupId = None)
          .toVector

      for (id <- extraTagGroupId)
        tagGroups :+= tags.needTagGroup(id)

      def tagGroupItems: Iterator[Unsorted] =
        tagGroups.iterator.map { g =>
          val txt = "Tag: " + g.name
          val col = Colours.ByTag(g.id)
          txt -> Dropdown.Item(
            key = key(col),
            label = <.span(*.deadDropdownItem.when(g.live is Dead), txt),
            value = col,
          )
        }

      def all: Iterator[Unsorted] =
        staticItems.iterator ++ tagGroupItems

      NonEmptyArraySeq.force(MutableArray(all).sortBy(_._1).map(_._2).arraySeq)
    }

}
