package com.beardedlogic.usecase.feature.publish

import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.db.UseCaseRev
import UseCase.ordering

case class DocHeader(
  title: String,
  preface: Option[String])

class Input(val header: Option[DocHeader], ucInput: List[(UseCaseRev, UseCase)]) {
  val sortedUseCases: List[UseCase] = ucInput.map(_._2).sorted
  val revMap: Map[UseCase, UseCaseRev] = ucInput.map(_.swap).toMap
}

trait Publisher[Output] {
  def publish(input: Input): Output
}
