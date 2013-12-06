package com.beardedlogic.shipreq.snippet

import com.beardedlogic.shipreq.test.TestDatabaseSupport
import com.beardedlogic.shipreq.util.NonEmptyTemplate
import org.scalatest.FunSuite

class AboutTest extends FunSuite with TestDatabaseSupport {

  lazy val template = NonEmptyTemplate.load("about").get

  test("Page should render without errors") {
    val html = (new About).attribution(template).toString
    countOccurrences(html, "©") should be > 10
  }
}
