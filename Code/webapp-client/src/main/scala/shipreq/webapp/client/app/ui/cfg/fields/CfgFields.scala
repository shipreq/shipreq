package shipreq.webapp.client.app.ui.cfg.fields

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra.OnUnmount
import monocle.macros.Lenser
import monocle.Optional
import monocle.std.option.some
import scala.annotation.tailrec
import scala.language.reflectiveCalls
import scalajs.js.{undefined, UndefOr, UndefOrOps, Array => JsArray}
import scalajs.js.JSConverters._
import scalaz.effect.IO
import scalaz.{Maybe, Memo, \&/}
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.syntax.bind.ToBindOps

import shipreq.prop.CycleDetector
import shipreq.prop.util.Multimap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
//import shipreq.webapp.base.data.Validators.{tag => V}
//import shipreq.webapp.base.data.Validators.shared.HashRefKeyVS
import shipreq.webapp.base.protocol.DeletionAction._
import shipreq.webapp.base.protocol.FieldProtocol
import shipreq.webapp.base.protocol.Routines.FieldCrud
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui.{RowDetailButton, ShowDeletedToggler}
import shipreq.webapp.client.lib.{FailureIO, SuccessIO, CrudIO}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND

object CfgFields {
  case class Props(cp: ClientProtocol, remote: FieldCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
    //def component: ReactComponentU_ = MainTable.Component(this)
    def component = <.h3("Pending...")
    println(clientData.project.fields)
  }
}
import CfgFields.Props

private[fields] object MainTable {

}