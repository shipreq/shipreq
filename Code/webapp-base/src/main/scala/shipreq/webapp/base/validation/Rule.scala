package shipreq.webapp.base.validation

import scalaz.Endo
import shipreq.webapp.base.TextMod

class Rule[A](l: Endo[A], c: Endo[A], v: Constraint[A]) {
  private def _l = l
  private def _c = c
  private def _v = v

  def liveCorrect(f: Endo[A] => Endo[A])            : Rule[A] = new Rule(f(l), c, v)
  def correct    (f: Endo[A] => Endo[A])            : Rule[A] = new Rule(l, f(c), v)
  def constraint (f: Constraint[A] => Constraint[A]): Rule[A] = new Rule(l, c, f(v))

  @inline final def +(that: Rule[A]): Rule[A] = this addRule that

  def addRule(that: Rule[A]): Rule[A] =
    new Rule(
      this._l andThen that._l,
      this._c andThen that._c,
      this._v + that._v)

  def forField(n: String): ValidatorU[A, A, A] =
    Validator(
      CorrectionPartU.endo(c).addLiveCorrect(l.run),
      ValidationPartU.forConstraint(n, v))
}

object Rule {
  def apply[A](constraint: Constraint[A]): Rule[A] =
    new Rule(Endo.idEndo, Endo.idEndo, constraint)

  def lv[A](liveCorrect: Endo[A], constraint: Constraint[A]): Rule[A] =
    new Rule(liveCorrect, Endo.idEndo, constraint)
}

object Rules {

  def lengthInRange(range: Range.Inclusive) =
    Rule.lv(TextMod.truncateToLength(range), Constraints.lengthInRange(range))

  def whitelistCharsR(charRange: String, errMsg: String) =
    Rule.lv(TextMod.regexReplace(s"[^$charRange]".r, ""), Constraints.whitelistCharsR(charRange)(errMsg))

  /*
    def whitelistCharsR(charRegex: String) = matchesR(s"^[$charRegex]*$$".r)
    def whitelistCharsS(charList: String) = whitelistCharsR(quote(charList))
    def blacklistCharsR(charRegex: String) = matchesR(s"^[^$charRegex]*$$".r)
    def blacklistCharsS(charList: String) = blacklistCharsR(quote(charList))
  */
}