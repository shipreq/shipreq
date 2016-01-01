package shipreq.webapp.client.app.reqtable

import shipreq.webapp.base.data._
import DataImplicits._
import Applicability.Subject

/**
 * Rules in project config prohibit certain combinations of column and requirement.
 */
object Applicability {
  private val pass = (_: Any) => true

  def apply(project: Project): Applicability = {
    val reqTypeFilter: Column => ReqTypeId => Boolean = {
      case Column.CustomField(id, _) => project.config.customField(id).reqTypes.filter
      case _: Column.BuiltIn         => pass
    }
    new Applicability(reqTypeFilter)
  }

  sealed trait Subject[A] {
    def apply(reqTypeFilter: ReqTypeId => Boolean)(a: A): Boolean
  }

  implicit object SubjectRow extends Subject[Row] {
    override def apply(reqTypeFilter: ReqTypeId => Boolean)(row: Row): Boolean =
      row match {
        case r: GenericReqRow   => SubjectReq(reqTypeFilter)(r.req)
        case _: ReqCodeGroupRow => true
      }
  }

  implicit object SubjectReq extends Subject[Req] {
    override def apply(reqTypeFilter: ReqTypeId => Boolean)(r: Req): Boolean =
      reqTypeFilter(r.reqTypeId)
  }
}

/**
 * Determination of which columns and rows are not-applicable.
 */
class Applicability(reqTypeFilter: Column => ReqTypeId => Boolean) {

  def apply(c: Column): ApplicabilityC =
    new ApplicabilityC(reqTypeFilter(c))

  def wrap[A](f: Column => Row => A)(`n/a`: A): Column => Row => A =
    c => apply(c).wrap(f(c))(`n/a`)
}

class ApplicabilityC(reqTypeFilter: ReqTypeId => Boolean) {

  @inline def apply[S](s: S)(implicit e: Subject[S]): Boolean =
    e(reqTypeFilter)(s)

  def choose[S: Subject, A](s: S, na: => A)(ok: => A): A =
    if (apply(s)) ok else na

  def wrap[S: Subject, A](f: S => A)(`n/a`: A): S => A =
    r => if (apply(r)) f(r) else `n/a`
}
