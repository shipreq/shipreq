package shipreq.webapp.client.project.feature.delerest

import nyaya.prop.Prop
import nyaya.test.DefaultSettings
import nyaya.test.PropTest._
import utest._

object RestoreLogicTest extends TestSuite {

  // Properties

  // Mandatory = input set

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // restore

  // Auto-selected & non-selected
  // - all dead
  // - imp by ≥ 1
  // - at least one of the input set are a transitive parent

  // Auto-selected
  // - if restored
  //   - restored have no dead imps (dead imps would make restoration necessity ambiguous)

  // Non-selected
  // - if selected and restored, ...
  //     - restored have dead imps (is this always true? children of this case might not hold)
  //     - inputs have no dead imps

  // In a project where there is max 1 imp/req, and everything is dead
  // - non-auto selected set == ∅
  // - restore with auto -> delete with auto == id
  // - auto = TC - live

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // delete

  // Auto-selected & non-selected
  // - all live
  // - imp by ≥ 1
  // - at least one of the input set are a transitive parent

  // Auto-selected
  // - if deleted
  //   - deleted have no live imps (live imps would make deletion necessity ambiguous)

  // Non-selected
  // - if selected and deleted, ...
  //     - deleted have live imps (is this always true? children of this case might not hold)
  //     - inputs have no live imps

  // In a project where there is max 1 imp/req, and everything is live
  // - non-auto selected set == ∅
  // - delete with auto -> restore with auto == id
  // - auto = TC - dead


  override def tests = TestSuite {

    // TODO do deletion first

    'deleteLogicProps - {
      val g = DeleRestProps.RandomData(Mode.Delete).genProps
//       g.mustSatisfyE(_.allProps)(DefaultSettings.propSettings.setSampleSize(7 * 1))
      scala.util.Try(g.bugHunt(10000, 8)(Prop.eval(_.allProps))(DefaultSettings.propSettings.setDebug)); ()

    }

  }
}
