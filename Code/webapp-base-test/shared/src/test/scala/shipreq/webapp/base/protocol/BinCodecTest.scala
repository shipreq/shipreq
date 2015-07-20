package shipreq.webapp.base.protocol

import boopickle._
import japgolly.nyaya._
import japgolly.nyaya.test.DefaultSettings
import japgolly.nyaya.test.PropTestOps._
import scalaz.Equal
import utest._
import shipreq.webapp.base.RandomData
import BinDataCodecs._

object BinCodecTest extends TestSuite {

  def prop[A: Pickler: Equal]: Prop[A] =
    Prop.equalSelf[A]("read.write = id", a => {
      val bytes = PickleImpl.intoBytes(a)
      val a2 = UnpickleImpl[A].fromBytes(bytes)
      a2
    })

  implicit val settings = DefaultSettings.propSettings.setSampleSize(8*4).setGenSize(100) //.setSeed(5)

  override def tests = TestSuite {

//    RandomData.projectConfig.map(_.customIssueTypes.data) mustSatisfy prop
//    RandomData.customReqTypes mustSatisfy prop
//    RandomData.customIssueTypes mustSatisfy prop
//    RandomData.revAndTagTree mustSatisfy prop
//    RandomData.customFieldId mustSatisfy prop
//    RandomData.projectConfig mustSatisfy prop
//    RandomData.project.map(_.reqs) mustSatisfy prop // FAIL
//    RandomData.project.map(_.reqCodes) mustSatisfy prop // FAIL
//    RandomData.project.map(_.reqText.data) mustSatisfy prop // FAIL
//    RandomData.project.map(_.reqTags) mustSatisfy prop // PASS
//    RandomData.project.map(_.implications) mustSatisfy prop // PASS

    RandomData.project mustSatisfy prop

  }
}
