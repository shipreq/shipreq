package shipreq.webapp.base.protocol

import boopickle._
import nyaya.prop._
import nyaya.test.DefaultSettings
import nyaya.test.PropTestOps._
import scalaz.Equal
import utest._
import shipreq.webapp.base.RandomData
import BinCodecMemberData._

object BinCodecTest extends TestSuite {

  def prop[A: Pickler: Equal]: Prop[A] =
    Prop.equalSelf[A]("read.write = id", a => {
      val bytes = PickleImpl.intoBytes(a)
      val a2 = UnpickleImpl[A].fromBytes(bytes)
      a2
    })

  implicit val settings = DefaultSettings.propSettings.setSampleSize(8*4) //.setSeed(5).setDebug

  override def tests = TestSuite {

//    RandomData.projectConfig.map(_.customIssueTypes.data) mustSatisfy prop
//    RandomData.customReqTypes mustSatisfy prop
//    RandomData.customIssueTypes mustSatisfy prop
//    RandomData.revAndTagTree mustSatisfy prop
//    RandomData.customFieldId mustSatisfy prop
//    RandomData.projectConfig mustSatisfy prop
//    RandomData.project.map(_.content.reqs) mustSatisfy prop // FAIL
//    RandomData.project.map(_.content.reqCodes) mustSatisfy prop // FAIL
//    RandomData.project.map(_.content.reqText.data) mustSatisfy prop // FAIL
//    RandomData.project.map(_.content.reqTags) mustSatisfy prop // PASS
//    RandomData.project.map(_.content.implications) mustSatisfy prop // PASS

    RandomData.project mustSatisfy prop
  }
}
