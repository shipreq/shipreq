package shipreq.webapp.client.protocol

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

@JSName("liftAjax")
object LiftAjax extends js.Object {

  def lift_ajaxHandler(input: String,
                       success: js.Function1[js.Any, Unit] = null,
                       failure: js.Function0[Unit] = null,
                       respType: String = null): Boolean = js.native

  /**
   * @param ajaxPath Get from AppConsts and surround with slashes.
   * @param version A counter declared by liftAjax.js which starts at 0 and increases for each call.
   * @return The AJAX URL.
   */
  def addPageNameAndVersion(ajaxPath: String, version: js.UndefOr[Int]): String = js.native
}
