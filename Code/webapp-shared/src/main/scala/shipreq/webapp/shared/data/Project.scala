package shipreq.webapp.shared.data

final case class Project(customReqTypes: Map[CustReqType.Id, CustReqType]) {

  lazy val allReqTypes: List[ReqType] =
    customReqTypes.values.foldLeft(ReqType.static)((a, b) => b :: a)

  // constraints:
  // - Σ oldMnemonics + Σ mnemonic contains no dups

  lazy val allActiveMnemonics: Set[ReqType.Mnemonic] =
    allReqTypes.foldLeft(Set.empty[ReqType.Mnemonic])((a, r) => a + r.mnemonic)

  lazy val allMnemonics: Set[ReqType.Mnemonic] =
    allReqTypes.foldLeft(allActiveMnemonics)((a, r) => a ++ r.oldMnemonics)
}