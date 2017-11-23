package shipreq.webapp.base.data.deletion

import nyaya.prop.Prop
import nyaya.test.DefaultSettings
import nyaya.test.PropTest._
import utest._

object RestoreLogicTest extends TestSuite {

  override def tests = TestSuite {

    'deleteLogicProps - {
      val g = DeleRestProps.RandomData(Mode.Delete).genProps
       g.mustSatisfyE(_.allProps)(DefaultSettings.propSettings.setSampleSize(7 * 1))
//      scala.util.Try(g.bugHunt(10009, 8)(Prop.eval(_.allProps))(DefaultSettings.propSettings.setDebug)); ()

    }

  }
}
