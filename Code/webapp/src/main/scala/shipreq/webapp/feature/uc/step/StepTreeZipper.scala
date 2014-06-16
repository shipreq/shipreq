package shipreq.webapp.feature.uc.step

import scalaz.Zipper
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.uc.text.StepText

/**
 * Provides zippers for viewing and maneuvering around a tree of steps.
 */
object StepTreeZipper {

  type DeepZipper = DeepBuilder#TreeZipper
  type FlatZipper = FlatBuilder#TreeZipper
  type DeepFocus = DeepBuilder#Focus
  type FlatFocus = FlatBuilder#Focus
  type AnyFocus = Builder#StepNodeView

  sealed trait Builder {
    def textmap: Map[LocalStepId, StepText]
    def labels: Map[LocalStepId, StepLabel]

    sealed trait StepNodeView {
      def node: StepNode
      def id = node.id
      def value: StepText = textmap(id)
      def level: Int = node.level
      def labelIndex: Int = node.labelIndex
      def label: StepLabel = labels(id)
      def text: String = value.text
      def flowFromClause = value.flowFromClause
      def flowToClause = value.flowToClause
      override def toString = s"${label.value}. '$text'"
    }
  }

  /**
   * Builds a zipper comprised of the top level of steps. (eg. 1.0, 1.1, 1.2).
   * In order to descend into a step's children, `down()` will provide a new zipper.
   */
  case class DeepBuilder(textmap: Map[LocalStepId, StepText], labels: Map[LocalStepId, StepLabel]) extends Builder {
    type TreeZipper = Zipper[Focus]
    type Parent = Option[TreeZipper]

    case class Focus(node: StepNode) extends StepNodeView {
      def builder: DeepBuilder = DeepBuilder.this
      def flatBuilder = FlatBuilder(textmap, labels)

      /**
       * Descends into this step's children.
       *
       * @return A new DeepZipper.
       */
      def down: Option[TreeZipper] = node.children match {
        case Nil => None
        case h :: t => Some(build(h, t))
      }

      /**
       * Flattens this step into a single, flat zipper.
       *
       * @return A FlatZipper for this step and all its children.
       */
      def flat: FlatZipper = flatBuilder.build(node, Nil)
    }

    def build(focus: StepNode, rights: Seq[StepNode]): TreeZipper =
      Zipper(Stream.empty, Focus(focus), buildStream(rights))

    private def buildStream(nodes: Seq[StepNode]): Stream[Focus] = nodes match {
      case Nil => Stream.empty
      case h :: t => Focus(h) #:: buildStream(t)
    }
  }

  /**
   * Builds a zipper comprised of all steps in the step tree, children before siblings.
   * (eg. 1.0, 1.0.1, 1.0.2, 1.0.2.a, 1.0.2.b, 1.0.3, 1.1).
   */
  case class FlatBuilder(textmap: Map[LocalStepId, StepText], labels: Map[LocalStepId, StepLabel]) extends Builder {
    type TreeZipper = Zipper[Focus]

    case class Focus(node: StepNode) extends StepNodeView

    def build(focus: StepNode, rights: Seq[StepNode]): TreeZipper = {
      val all = buildStream(focus +: rights)
      Zipper(Stream.empty, all.head, all.tail)
    }

    private def buildStream(nodes: Seq[StepNode]): Stream[Focus] = nodes match {
      case Nil => Stream.empty
      case h :: t => Focus(h) #:: buildStream(h.children) #::: buildStream(t)
    }
  }
}