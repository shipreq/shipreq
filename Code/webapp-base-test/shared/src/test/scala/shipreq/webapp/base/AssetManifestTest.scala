package shipreq.webapp.base

import sourcecode.Line
import shipreq.base.test.BaseTestUtil._
import utest._

object AssetManifestTest extends TestSuite {

  override def tests = Tests {

    "staticCdn" - {
      val cdn = AssetManifest.StaticAssetCdn("https://static.shipreq.com")
      def test(in: String, out: String = null)(implicit l: Line): Unit =
        assertEq(cdn.modPath(in), Option(out).getOrElse(in))

      "staticJs" - test("/s/11670b0e3e1d31a771f7cd5a49d22ab3.js", "https://static.shipreq.com/11670b0e3e1d31a771f7cd5a49d22ab3.js")
      "dataSvg"  - test("data:image/svg+xml;base64,PHN2ZnPg==")
      "iconsEot" - test("icons.eot") // this damn thing lives in the /s dir too
      "other"    - test("/x/as.js")
    }

  }
}
