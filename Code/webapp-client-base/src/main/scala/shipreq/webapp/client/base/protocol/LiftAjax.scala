package shipreq.webapp.client.base.protocol

import scala.scalajs.js
import scala.scalajs.js.{UndefOr, |}
import scala.scalajs.js.annotation.JSName

@JSName("lift") @js.native
object LiftAjax extends js.Object {

  /**
    * @return false, always.
    */
  def ajax(data: String,
           onSuccess: js.Function1[js.Any, Unit] = null,
           onFailure: js.Function0[Unit] = null,
           responseType: String = null,
           onUploadProgress: js.Function = null): Boolean = js.native

  /**
   * @param url Get from WebappConfig and surround with slashes.
   * @param version A counter declared by liftAjax.js which starts at 0 and increases for each call.
   * @return The AJAX URL.
   */
  def calcAjaxUrl(url: String, version: Int | Null): String = js.native

}
