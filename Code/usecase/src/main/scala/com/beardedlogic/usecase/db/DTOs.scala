package com.beardedlogic.usecase
package db

import lib.ExternalId
import lib.Types._
import AutoExternaliseIds._



case class ProjectSummary(
  id: ProjectId,
  name: String,
  ucCount: Int,
  ucUpdatedAt: Option[String])



// NOTE: These fields names need to match the attributes in list.html
case class UseCaseSummary(
  eid: UseCaseIdentEI, // TODO UseCaseSummary EID not needed anymore
  number: UseCaseNumber,
  title: String,
  updatedAt: String) {

  def this(ucr: UseCaseRev, updatedAt: String) = this(ucr.identId, ucr.ident.number, ucr.header.title, updatedAt)

  def parseId = ExternalId.UseCase.parseO(eid)
}
