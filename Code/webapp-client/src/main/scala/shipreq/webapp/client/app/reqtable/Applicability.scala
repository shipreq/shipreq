package shipreq.webapp.client.app.reqtable

import shipreq.webapp.base.data._
import Applicability.Subject

/**
 * Rules in project config prohibit certain combinations of column and requirement.
 */
object Applicability { // TODO Now that time has passed, this looks stupid, over-specialised and confusing. Redo.
  private val alwaysApplicable: Any => Applicable =
    _ => Applicable

  def apply(project: Project): Applicability = {
    val reqTypeFilter: Column => ReqTypeId => Applicable =
      c => Column.field(c, project.config) match {
        case Some(f) => f.applicable
        case None    => alwaysApplicable
      }
    new Applicability(reqTypeFilter)
  }

  sealed trait Subject[A] {
    def apply(reqTypeFilter: ReqTypeId => Applicable)(a: A): Applicable
  }

  implicit object SubjectRow extends Subject[Row] {
    override def apply(reqTypeFilter: ReqTypeId => Applicable)(row: Row): Applicable =
      row match {
        case r: ReqRow          => SubjectReq(reqTypeFilter)(r.req)
        case _: ReqCodeGroupRow => Applicable
      }
  }

  implicit object SubjectReq extends Subject[Req] {
    override def apply(reqTypeFilter: ReqTypeId => Applicable)(r: Req): Applicable =
      reqTypeFilter(r.reqTypeId)
  }
}

/**
 * Determination of which columns and rows are not-applicable.
 */
class Applicability(reqTypeFilter: Column => ReqTypeId => Applicable) {

  def apply(c: Column): ApplicabilityC =
    new ApplicabilityC(reqTypeFilter(c))

  def wrap[A](f: Column => Row => A)(`n/a`: A): Column => Row => A =
    c => apply(c).wrap(f(c))(`n/a`)
}

class ApplicabilityC(reqTypeFilter: ReqTypeId => Applicable) {

  @inline def apply[S](s: S)(implicit e: Subject[S]): Applicable =
    e(reqTypeFilter)(s)

  def choose[S: Subject, A](s: S, `n/a`: => A)(ok: => A): A =
    apply(s) match {
      case Applicable    => ok
      case NotApplicable => `n/a`
    }

  def wrap[S: Subject, A](f: S => A)(`n/a`: A): S => A =
    r => choose(r, `n/a`)(f(r))
}
