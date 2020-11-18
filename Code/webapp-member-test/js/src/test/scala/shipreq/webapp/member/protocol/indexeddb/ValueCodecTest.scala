package shipreq.webapp.member.protocol.indexeddb

import boopickle.DefaultBasic._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node.asyncTest
import shipreq.base.util.BinaryData
import utest._

object ValueCodecTest extends TestSuite {

  override def tests = Tests {

    // Note: pickleCompressEncrypt is covered in IndexedDbTest

    "versionedBinary" - asyncTest {
      import ValueCodec.Async._
      type A = Int

      val codec1: BinaryLayer[A] = _.pickleBasic[Int]
      val codec2: BinaryLayer[A] = _.pickleBasic[String].xmap(_.toInt)(_.toString)

      val v1 = versionedBinary(codec1)
      val v2 = versionedBinary(codec1, codec2)

      for {
        bin1   <- v1.encode(123)
        bin2   <- v2.encode(687)
        res1v1 <- v1.decode(bin1)
        res1v2 <- v2.decode(bin1)
        res2v1 <- v1.decode(bin2).attempt
        res2v2 <- v2.decode(bin2)
        res0   <- binary.encode(BinaryData.empty).flatMap(v2.decode).attempt
      } yield {

        assertEq(res1v1, 123)
        assertEq(res1v2, 123)
        assertEq(res2v2, 687)

        assert(res2v1.isLeft)
        assert(res0.isLeft)

        s"""bin1   = ${ValueCodec.binary.decode(bin1).runNow()}
           |bin2   = ${ValueCodec.binary.decode(bin2).runNow()}
           |res2v1 = $res2v1
           |res0   = $res0
           |""".stripMargin.trim
      }
    }
  }
}
