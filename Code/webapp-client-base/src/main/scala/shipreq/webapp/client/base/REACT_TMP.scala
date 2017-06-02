package shipreq.webapp.client.base

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^.{EmptyVdom, TagMod}

object REACT_TMP {

  implicit class OptionCallbackExt(private val self: Option[Callback]) extends AnyVal {
    def getOrEmpty: Callback =
       self.getOrElse(Callback.empty)
  }

  implicit class TagModObjExt(private val self: TagMod.type) extends AnyVal {
    def when(cond: Boolean)(t: => TagMod): TagMod =
       if (cond) t else EmptyVdom

    @inline def unless(cond: Boolean)(t: => TagMod): TagMod =
       when(!cond)(t)
  }

}
