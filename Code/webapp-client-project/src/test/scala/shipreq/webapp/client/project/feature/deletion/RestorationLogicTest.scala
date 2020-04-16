package shipreq.webapp.client.project.feature.deletion

import nyaya.test.DefaultSettings
import nyaya.test.PropTest._
import utest._

object RestorationLogicTest extends TestSuite {

  override def tests = Tests {

    "props" - {
      val g = DeletionProps.RandomData(Restore).genProps
      g.mustSatisfyE(_.allProps)(DefaultSettings.propSettings.setSampleSize(2))
//       scala.util.Try(g.bugHunt(100, 500)(Prop.eval(_.allProps))(DefaultSettings.propSettings.setDebug)); ()
    }

  }
}
