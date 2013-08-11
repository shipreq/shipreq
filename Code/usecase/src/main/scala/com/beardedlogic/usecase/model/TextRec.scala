package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.db._
import lib.Types._
import DBHelpers._

case class TextRev(identId: TextIdentId, rev: Short, id: TextRevId, text: String)

object TextAccessor {
}

trait TextAccessor extends DatabaseAccessor {
  import TextAccessor._


}