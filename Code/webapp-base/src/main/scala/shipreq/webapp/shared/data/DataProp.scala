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
    Prop[ReqType]("oldMnemonics doesn't contain mnemonic", a => !a.oldMnemonics.contains(a.mnemonic))

  object customReqTypes {

    lazy val uniqueMnemonics =
      Prop[CustomReqTypes]("each mnemonics is unique",
        _.data.toList.flatMap(b => b.mnemonic :: b.oldMnemonics.toList).isUnique)

    lazy val uniqueId =
      Prop[CustomReqTypes]("each CustomReqTypeId is unique", _.data.map(_.id).isUnique)

    lazy val uniqueNames =
      Prop[CustomReqTypes]("each CustomReqType name is unique", _.data.map(_.name).isUnique)

    lazy val all =
      uniqueMnemonics ∧ uniqueId ∧ uniqueNames ∧ rev.contramap(_.rev) ∧ reqType.forall[CustomReqTypes, List](_.data)
  }

  lazy val project =
    customReqTypes.all.contramap[Project](_.customReqTypes)
}
