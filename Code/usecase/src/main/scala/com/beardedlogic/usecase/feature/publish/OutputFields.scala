package com.beardedlogic.usecase.feature.publish

import scalaz.Name
import com.beardedlogic.usecase.feature.uc.step.StepTreeZipper
import com.beardedlogic.usecase.feature.uc.text.FreeText
import com.beardedlogic.usecase.lib.Types._

sealed trait OutputField

case class OF_LastUpdated(when: String @@ ISO8601) extends OutputField

case class OF_Text(title: String, value: FreeText) extends OutputField

case class OF_Step(title: String, value: Option[StepTreeZipper.DeepZipper]) extends OutputField

case class OF_FlowGraph(dot: Name[String]) extends OutputField

