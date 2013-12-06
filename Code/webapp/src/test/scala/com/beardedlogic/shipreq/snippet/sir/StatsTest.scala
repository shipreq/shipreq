package com.beardedlogic.shipreq.snippet.sir

import com.beardedlogic.shipreq.test.TestDatabaseSupport
import com.beardedlogic.shipreq.util.NonEmptyTemplate
import org.scalatest.FunSuite

class StatsTest extends FunSuite with TestDatabaseSupport {

  lazy val template = NonEmptyTemplate.load("sir/stats").get

  test("Page should render without errors") {
    val html = Stats.render(template).toString
    html should not include(" class=\"err\">")
  }
}
