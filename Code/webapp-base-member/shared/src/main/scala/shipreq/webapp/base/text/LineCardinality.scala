package shipreq.webapp.base.text

import shipreq.base.util.IsoBool

sealed abstract class LineCardinality extends IsoBool[LineCardinality] {
  override def companion = LineCardinality
}

object LineCardinality extends IsoBool.Object[LineCardinality] {
  override def positive = MultiLine
  override def negative = SingleLine
}

case object MultiLine  extends LineCardinality
case object SingleLine extends LineCardinality