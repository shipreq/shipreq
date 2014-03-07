package shipreq.webapp.feature.publish

import org.joda.time.DateTime
import scalaz.Name
import shipreq.webapp.feature.uc.step.StepTreeZipper
import shipreq.webapp.feature.uc.text.FreeText

sealed trait OutputField

case class OF_Revision(rev: Short) extends OutputField

case class OF_LastUpdated(when: DateTime) extends OutputField

case class OF_Text(title: String, value: FreeText) extends OutputField

case class OF_Step(title: String, value: Option[StepTreeZipper.DeepZipper]) extends OutputField

case class OF_FlowGraph(dot: Name[String]) extends OutputField

