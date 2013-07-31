package com.beardedlogic.usecase.lib

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import com.beardedlogic.usecase.util.JsExt
import JsExt._
import net.liftweb.http.js.{JsMember, JsCmd, JsCmds}

class JsExtTest extends FreeSpec with ShouldMatchers {

  implicit class JsMemberExt(val x: JsMember) {
    def ==>(expectedJs: String) = x.toJsCmd should equal(expectedJs)
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
