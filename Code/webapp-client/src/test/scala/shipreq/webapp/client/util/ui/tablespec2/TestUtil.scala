package shipreq.webapp.client.util.ui.tablespec2

import scalaz.Equal
import scalaz.effect.IO
import shipreq.prop.test.Gen
import RowStatus._

object TestUtil {

  def fields2[A,B](empty: (A,B)) =
    FieldSet2[(A,B)](_._1, _._2)(empty)

  implicit val eqRowStatus = Equal.equalA[RowStatus]

  val failedRowStatus =
    Failed(IO(()))

  def genRowStatus: Gen[RowStatus] =
    Gen.oneof(Sync, Locked, failedRowStatus)
}