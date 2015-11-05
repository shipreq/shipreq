package domainy

import org.scalajs.dom.console
import copied.TaggedTypes._
import Data._

object FakeDao {

  private implicit class OEXT[A](val a: A) extends AnyVal {
    def log(s: String) = { console.log(s"$s $a"); a}
    def fakeCreate    : A    = log("FAKE CREATE:")
    def fakeUpdate    : A    = log("FAKE UPDATE:")
    def fakeDeleteHard: Unit = log("FAKE HARD-DELETE :")
    def fakeDeleteSoft: Unit = log("FAKE SOFT-DELETE:")
    def fakeRestore   : Unit = log("FAKE RESTORE:")
  }
  private var _fakeId = 999L // JS is single-threaded
  private def fakeId = { _fakeId = _fakeId + 1; _fakeId }

  object customIssueType {
    def create(v: CustomIssueTypeV) =
      v.withId(CustomIssueTypeId(fakeId)).fakeCreate

    def update(a: CustomIssueType) = a.fakeUpdate

    def deleteHard(id: CustomIssueTypeId) = id.fakeDeleteHard
  }

  object customReqType {
//    def create(mnemonic: ReqTypeMnemonic, name: String, implicationRequired: Boolean) =
//      CustomReqTypeV(mnemonic, Set.empty, name, implicationRequired, true).withId(CustomReqTypeId(fakeId)).fakeCreate

    def create(nv: CustomReqTypeNV) =
      CustomReqType(CustomReqTypeId(fakeId), nv.mnemonic, Set.empty, "NAME TODO", nv.implicationRequired, true).fakeCreate

    def update(id: CustomReqTypeId, v: CustomReqTypeNV) =
      CustomReqType(id, v.mnemonic, Set.empty, v.name, v.implicationRequired, true).fakeUpdate

    def deleteHard(id: CustomReqTypeId) = id.fakeDeleteHard
    def deleteSoft(id: CustomReqTypeId) = id.fakeDeleteSoft
    def restore(id: CustomReqTypeId) = id.fakeRestore
  }

}
