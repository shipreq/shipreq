package com.beardedlogic.usecase.lib

import Types._

trait UseCaseRelations {

  def findUcTitle(num: UseCaseNumber): Option[String]
}

object UseCaseRelations {
  val Empty: UseCaseRelations = new UseCaseRelations {
    override def findUcTitle(num: UseCaseNumber) = None
  }
}