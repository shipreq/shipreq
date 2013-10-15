package com.beardedlogic.usecase
package db

import lib.ExternalId
import lib.Types._


case class ProjectSummary(
  id: ProjectId,
  name: String,
  ucCount: Int,
  ucUpdatedAt: Option[String])


class UseCaseSummary(
  val id: UseCaseIdentId,
  val number: UseCaseNumber,
  val title: String) {

  def this(ucr: UseCaseRev) = this(ucr.identId, ucr.ident.number, ucr.header.title)

  lazy val eid = ExternalId.UseCase.toExternal(id)
}

class UseCaseSummary2(
  id: UseCaseIdentId,
  number: UseCaseNumber,
  title: String,
  val updatedAt: String) extends UseCaseSummary(id, number, title){

  def this(ucr: UseCaseRev, updatedAt: String) = this(ucr.identId, ucr.ident.number, ucr.header.title, updatedAt)
}
