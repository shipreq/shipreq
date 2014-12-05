package shipreq.prop

import scalaz.Equal

final case class EvalOver(input: Any) {

  def atom(name: => String, failure: FailureReasonO): EvalL =
    Eval.atom(name, this, failure)

  def test[A](name: => String, t: Boolean): EvalL =
    Eval.test(name, this, t)

  def equal[A: Equal](name: => String, actual: A, expect: A): EvalL =
    Eval.equal(name, this, actual, expect)
}