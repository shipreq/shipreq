package shipreq.base.test.specs2

import org.specs2.execute._

object AllowUnitAsResult {
  implicit def unitAsResult: AsResult[Unit] = new AsResult[Unit] {
    def asResult(r: =>Unit) =
      ResultExecution.execute(r)(_ => Success())
  }
}
