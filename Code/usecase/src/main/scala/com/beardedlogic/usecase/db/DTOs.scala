package com.beardedlogic.usecase
package db

import feature.ExternalId
import feature.uc.UseCaseFns
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

  def this(ucr: UseCaseRev) = this(ucr.identId, ucr.ident.number, ucr.title)

  final def eid = ExternalId.UseCase.toExternal(id)
  final def fullName = UseCaseFns.fullName(number, title)
}

class UseCaseSummary2(
  id: UseCaseIdentId,
  number: UseCaseNumber,
  title: String,
  val updatedAt: String) extends UseCaseSummary(id, number, title){

  def this(ucr: UseCaseRev, updatedAt: String) = this(ucr.identId, ucr.ident.number, ucr.title, updatedAt)
}
