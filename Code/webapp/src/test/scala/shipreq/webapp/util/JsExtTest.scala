package shipreq.webapp.util

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import net.liftweb.http.js.{JsMember, JsCmd}
import JsExt._

class JsExtTest extends FreeSpec with Matchers {

  implicit class JsMemberExt(val x: JsMember) {
    def ==>(expectedJs: String) = x.toJsCmd shouldBe expectedJs
  }

  lazy val DummyJsCmd = new JsCmd {
    override def toJsCmd = "dummy"
  }

  "JqDurationExpr" - {
    "JqFadeIn with duration: default" in {
      JqFadeIn() ==> "fadeIn()"
    }

    "JqFadeIn with duration: fast" in {
      JqFadeIn(Fast) ==> "fadeIn('fast')"
    }

    "JqFadeIn with duration: milliseconds" in {
      JqFadeIn(210.ms) ==> "fadeIn(210)"
    }
  }

  "JsMethodWithOptionalOnCompleteCallback" - {
    "JqSlideDown with no args" in {
      JqSlideDown().andThen(DummyJsCmd) ==> "slideDown(function(){dummy})"
    }
    "JqSlideDown with a duration" in {
      JqSlideDown(150.ms).andThen(DummyJsCmd) ==> "slideDown(150,function(){dummy})"
    }
  }
}
