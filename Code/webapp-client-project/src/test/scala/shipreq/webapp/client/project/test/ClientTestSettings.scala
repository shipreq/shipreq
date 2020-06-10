package shipreq.webapp.client.project.test

import nyaya.test.{DefaultSettings, PropTestOps}

/**
 * For properties that are tested on both JVM and JS, it is acceptable to run less here in JS.
 * For properties that are tested exclusively on JS, it is important to perform more testing.
 */
object ClientTestSettings extends PropTestOps {

  implicit val propSettings = DefaultSettings.propSettings
    .setSampleSize(50)
}
