package com.beardedlogic.usecase.feature.publish

import com.beardedlogic.usecase.feature.uc.UseCase

case class DocHeader(
  title: String,
  desc: Option[String])

case class Input(
  header: Option[DocHeader],
  useCases: List[UseCase])

trait Publisher[Output] {
  def publish(input: Input): Output
}
