package shipreq.webapp.member.protocol.binary

import nyaya.gen.Gen
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.member.test.project.RealProject1
import sourcecode.Line
import utest._

object CompressionTest extends TestSuite {

  private def assertRoundTrip(zip: Compression, dataSize: Int)(implicit l: Line): String = {
    val src = BinaryData.fromArraySeq(Gen.byte.arraySeq(dataSize).sample())
    assertRoundTrip(zip, src)
  }

  private def assertRoundTrip(zip: Compression, src: BinaryData)(implicit l: Line): String = {
    val zipped   = zip.compress(src.duplicate)
    val unzipped = zip.decompressOrThrow(zipped)
    assertEq(actual = unzipped, expect = src)
    assertNotEq(src, zipped)
    "%,d bytes => %,d bytes".format(src.length, zipped.length)
  }

  override def tests = Tests {

    "9_raw_0"       - assertRoundTrip(Compression(9, false), 0)
    "9_raw_41"      - assertRoundTrip(Compression(9, false), 41)
    "9_raw_12345"   - assertRoundTrip(Compression(9, false), 12345)
    "3_raw_800"     - assertRoundTrip(Compression(3, false), 800)
    "9_hdr_1771"    - assertRoundTrip(Compression(9, true), 1771)
    "9_raw_project" - assertRoundTrip(Compression(9, false), RealProject1.projectBinary)
  }
}
