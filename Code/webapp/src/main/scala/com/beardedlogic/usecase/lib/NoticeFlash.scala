package com.beardedlogic.usecase.lib

import scala.xml.{Text, NodeSeq}
import scalaz.NonEmptyList
import com.beardedlogic.usecase.util.ListFlashVar

object NoticeFlash {

  sealed class NoticeFlashVar extends ListFlashVar[NodeSeq] {
    def addS(msg: String) = add1(Text(msg))
    def addS(msgs: NonEmptyList[String]) = add(msgs.map(Text.apply))
  }

  val errors = new NoticeFlashVar
  val warnings = new NoticeFlashVar
  val notices = new NoticeFlashVar
}
