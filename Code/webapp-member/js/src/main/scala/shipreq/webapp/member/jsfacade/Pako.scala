package shipreq.webapp.member.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|

@js.native
@nowarn("msg=dead|never used")
sealed trait Pako extends js.Object {
  import Pako._

  def deflate(data: Data, options: DeflateOptions = js.native): Data = js.native

  /** The same as deflate, but creates raw data, without wrapper (header and adler32 crc). */
  def deflateRaw(data: Data, options: DeflateOptions = js.native): Data = js.native

  /** Throws an exception on error */
  def inflate(data: Data, options: InflateOptions = js.native): Data = js.native

  /** The same as inflate, but creates raw data, without wrapper (header and adler32 crc).
    *
    * Throws an exception on error.
    */
  def inflateRaw(data: Data, options: InflateOptions = js.native): Data = js.native
}

@nowarn("msg=dead|never used")
object Pako {

  @js.native
  @JSGlobal("Pako")
  val instance: Pako = js.native

  type Data = Uint8Array | js.Array[Int] | String

  /** See http://zlib.net/manual.html#Advanced */
  @js.native
  trait DeflateOptions extends js.Object {

    /** Z_NO_COMPRESSION:         0
      * Z_BEST_SPEED:             1
      * Z_BEST_COMPRESSION:       9
      * Z_DEFAULT_COMPRESSION:   -1
      */
    var level     : js.UndefOr[Int] = js.native
    var windowBits: js.UndefOr[Int] = js.native
    var memLevel  : js.UndefOr[Int] = js.native
    var strategy  : js.UndefOr[Int] = js.native
    var dictionary: js.UndefOr[Int] = js.native
  }


  @js.native
  trait InflateOptions extends js.Object {
    var windowBits: js.UndefOr[Int] = js.native
  }
}
