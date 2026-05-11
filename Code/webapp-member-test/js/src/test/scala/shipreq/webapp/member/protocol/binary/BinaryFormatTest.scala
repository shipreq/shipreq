package shipreq.webapp.member.protocol.binary

import boopickle.DefaultBasic._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.Node.asyncTest
import shipreq.base.util.BinaryData
import utest._

object BinaryFormatTest extends TestSuite {

  override def tests = Tests {

    // Note: pickleCompressEncrypt is covered in IndexedDbTest

    "versionedBinary" - asyncTest {
      type A = Int

      val codec1: BinaryFormat[A] = BinaryFormat.id.pickleBasic[Int]
      val codec2: BinaryFormat[A] = BinaryFormat.id.pickleBasic[String].xmap(_.toInt)(_.toString)

      val v1 = BinaryFormat.versioned(codec1)
      val v2 = BinaryFormat.versioned(codec1, codec2)

      for {
        bin1   <- v1.encode(123)
        bin2   <- v2.encode(687)
        res1v1 <- v1.decode(bin1)
        res1v2 <- v2.decode(bin1)
        res2v1 <- v1.decode(bin2).attempt
        res2v2 <- v2.decode(bin2)
        res0   <- v2.decode(BinaryData.empty).attempt
      } yield {

        assertEq(res1v1, 123)
        assertEq(res1v2, 123)
        assertEq(res2v2, 687)

        assert(res2v1.isLeft)
        assert(res0.isLeft)

        s"""bin1   = $bin1
           |bin2   = $bin2
           |res2v1 = $res2v1
           |res0   = $res0
           |""".stripMargin.trim
      }
    }
  }
}
