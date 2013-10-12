package com.beardedlogic.usecase.lib

import Types._
import com.beardedlogic.usecase.db.{UseCaseSummary, DaoT}

trait UseCaseRelations {

  def findUcTitle(num: UseCaseNumber): Option[String]
}

case class CachedUseCaseRelations(ucs: List[UseCaseSummary]) extends UseCaseRelations {
  override def findUcTitle(num: UseCaseNumber) = ucs.find(_.number == num).map(_.title)
}

object UseCaseRelations {
  val Empty: UseCaseRelations = new UseCaseRelations {
    override def findUcTitle(num: UseCaseNumber) = None
  }
}