package shipreq.webapp.shared.data

import scalaz.std.list._
import shipreq.base.prop._
import shipreq.webapp.shared.data.delta.Rev

object DataProp {

  private implicit class SeqExt[A](val s: Seq[A]) extends AnyVal {
    @inline final def isUnique = s.distinct == s
  }

  lazy val rev =
    Prop[Rev]("rev ≥ 0", _.value >= 0)

  lazy val reqType =
    Prop[ReqType]("oldMnemonics doesn't contain current mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

  object customReqType {

    private def mnemonicBlacklist  =
      ReqType.static.map(_.mnemonic).toSet

    // starting to overlap with validation....
    lazy val mnemonicStatic =
      Prop[CustomReqType]("mnemonic doesn't overlap with static",
        a => mnemonicBlacklist.intersect(a.oldMnemonics + a.mnemonic).isEmpty)

    lazy val all = mnemonicStatic ∧ reqType.subst
  }

  object customReqTypes {

    lazy val uniqueMnemonics =
      Prop[CustomReqTypes]("each mnemonic is unique",
        _.data.toList.flatMap(b => b.mnemonic :: b.oldMnemonics.toList).isUnique)

    lazy val uniqueId =
      Prop[CustomReqTypes]("each CustomReqTypeId is unique", _.data.map(_.id).isUnique)

    lazy val uniqueNames =
      Prop[CustomReqTypes]("each CustomReqType name is unique", _.data.map(_.name).isUnique)

    lazy val each =
      customReqType.all.forall[CustomReqTypes, List](_.data)

    lazy val all =
      uniqueMnemonics ∧ uniqueId ∧ uniqueNames ∧ rev.contramap(_.rev) ∧ each
  }

  lazy val project =
    customReqTypes.all.contramap[Project](_.customReqTypes)
}
