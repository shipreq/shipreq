package shipreq.webapp.client.lib

import japgolly.scalajs.react.{CallbackTo, Callback, ReactElement}

package object ui extends EditorExt {

  type SimpleEditor[I] = SimpleEditor2[I, I]

  type SimpleEditor2[A, B] = Editor[A, B, CallbackTo, Unit, Unit, Callback, ReactElement]
}