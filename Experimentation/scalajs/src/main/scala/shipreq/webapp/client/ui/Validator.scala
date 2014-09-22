package shipreq.webapp.client.ui

import scalaz.{\/-, -\/, \/}

// TODO not really a validator, data input/recv pipeline/receiver/valve/protocol/rules/gateway/enforcer
// TODO Determine Validator properties/laws
trait Validator[I, C, O] {
  def liveCorrect: I => I
  def correct    : I => C
  def validate   : C => ErrorMsg \/ O
  def c2i        : C => I
  final def correctAndValidate = validate compose correct
}

object Validator {

  def forRow[S, W, I, C, O](norm: Validator[I, C, O], f: ValidateFnW[S, W, O], w: W): S => Validator[I, C, O] =
    s => new Validator[I, C, O] {
      def liveCorrect = norm.liveCorrect
      def correct     = norm.correct
      def c2i         = norm.c2i
      def validate = c => {
        val orig = norm.validate(c)
        orig.flatMap(o => f(s, w, o).fold(orig)(-\/.apply))
      }
    }

  def nop[I]: Validator[I, I, I] = new Validator[I, I, I] {
    override def liveCorrect = identity
    override def correct     = identity
    override def validate    = \/-.apply
    override def c2i         = identity
  }

  // TODO Delete this? If keeping add a version where A=B and Equal is used
  // TODO Delete/change this default error msg
  def uniqueness[S, W, A, B](extract: (S,W) => Stream[A], cmp: (A, B) => Boolean, errorMsg: ErrorMsg = "Already in use. Duplicate."): ValidateFnW[S,W,B] =
    (s, w, b) => {
      val dupFound = extract(s, w).exists(cmp(_,b))
      //.foldLeft(0)((j, a) => if (j <= 1 && cmp(a,i)) j + 1 else j) // TODO effeciency, too eager
      if (dupFound) Some(errorMsg) else None
    }
}
