package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.BackendScope
import org.scalajs.dom.HTMLSelectElement
import scalaz.Equal
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.base.util.Util

object SelectAndInvoke {

  def Component[A: Equal](staticProps: StaticProps[A]) =
    ReactComponentB[Props[A]]("SelectAction")
      .stateless
      .backend(new Backend(_, staticProps))
      .render(_.backend.render)
      .build

  final case class StaticProps[A: Equal](invokeButtonLabel: String, optionLabel: A => String) {
    @inline def component = Component(this)
  }

  final case class Choice[A](value : A,
                             select: Option[IO[Unit]],
                             invoke: Option[IO[Unit]])

  final case class Props[A](selected: Option[A],
                            choices : Seq[Choice[A]],
                            disabled: Boolean)

  final class Backend[A: Equal]($: BackendScope[Props[A], Unit], staticProps: StaticProps[A]) {
    import staticProps._
    
    @inline def p = $.props
    
    def render: ReactElement = {
      val (actionTags, actionIndex) =
        Util.foldAndIndexS(p.choices, Vector.empty[ReactTag])((q, k, o) => q :+
          <.option(
            ^.value    := k,
            ^.disabled := o.select.isEmpty,
            optionLabel(o.value)))

      def changeAction: SyntheticEvent[HTMLSelectElement] => Option[IO[Unit]] =
        e => actionIndex.get(e.target.value).flatMap(_.select)

      val selection =
        p.selected.flatMap(sel => actionIndex.find(_._2.value ≟ sel)).orElse(actionIndex.headOption)

      val dropdown =
        <.select(
          ^.value      := selection.map(_._1),
          ^.disabled   := p.disabled,
          ^.onChange ~~>? changeAction,
          actionTags)

      val invokeIO =
        selection.flatMap(_._2.invoke)

      val invokeButton =
        <.button(
          ^.disabled  := (p.disabled || invokeIO.isEmpty),
          ^.onClick ~~>? invokeIO,
          invokeButtonLabel)

      <.div(dropdown, invokeButton)
    }
  }
}
