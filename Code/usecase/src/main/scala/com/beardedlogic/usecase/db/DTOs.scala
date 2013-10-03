package com.beardedlogic.usecase
package db

import lib.ExternalId
import lib.Types._


case class ProjectSummary(
  id: ProjectId,
  name: String,
  ucCount: Int,
  ucUpdatedAt: Option[String])


case class UseCaseSummary(
  id: UseCaseIdentId,
  number: UseCaseNumber,
  title: String,
  updatedAt: String) {

  def this(ucr: UseCaseRev, updatedAt: String) = this(ucr.identId, ucr.ident.number, ucr.header.title, updatedAt)

  lazy val eid = ExternalId.UseCase.toExternal(id)
}
