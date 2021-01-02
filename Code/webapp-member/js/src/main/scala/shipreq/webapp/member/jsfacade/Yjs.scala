package shipreq.webapp.member.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|

@JSGlobal("Y")
@js.native
@nowarn
object Yjs extends js.Object {

  type Update      = Uint8Array
  type StateVector = Uint8Array

  def applyUpdate(doc: Doc, update: Update): Unit = js.native

  /**
   * @param targetStateVector The state of the target that receives the update. Leave empty to write all known structs
   */
  def encodeStateAsUpdate(doc: Doc, targetStateVector: StateVector = js.native): Update = js.native

  def encodeStateVector(doc: Doc): StateVector = js.native

  @js.native
  final class Doc extends js.Object {
    var guid    : String = js.native
    var clientID: Int    = js.native

    def getArray(name: String = js.native): YArray = js.native
    def getMap  (name: String = js.native): YMap   = js.native
    def getText (name: String = js.native): YText  = js.native
  }

  type Value =
    Uint8Array |
    AbstractType |
    String | JsNumber | Boolean // JSON - currently excluding object & null

  @js.native
  sealed abstract class AbstractType extends js.Object

  @js.native
  final class YArray extends AbstractType {
    def length: Int = js.native
    def toArray(): js.Array[Value] = js.native
    def insert(index: Int, content: js.Array[Value]): Unit = js.native
    def delete(index: Int, length: Int): Unit = js.native
    def push(content: js.Array[Value]): Unit = js.native
    def get(index: Int): js.UndefOr[Value] = js.native
    def slice(start: Int, end: Int = js.native): js.Array[Value] = js.native
  }

  @js.native
  final class YMap extends AbstractType {
     def set(key: String, value: Value): Unit = js.native
     def get(key: String): js.UndefOr[Value] = js.native
     def delete(key: String): Unit = js.native
     def has(key: String): Boolean = js.native
     def entries(): js.Iterator[js.Array[js.Any]] = js.native
     def values(): js.Iterator[Value] = js.native
     def keys(): js.Iterator[String] = js.native
  }

  @js.native
  final class YText extends AbstractType {
    def length: Int = js.native
    @JSName("toString") def toText(): String = js.native
    def insert(index: Int, content: String): Unit = js.native
    def delete(index: Int, length: Int): Unit = js.native
  }

}
