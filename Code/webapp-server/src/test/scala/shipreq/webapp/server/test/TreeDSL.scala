package shipreq.webapp.server.test

import shipreq.webapp.server.lib.Types._
import shipreq.webapp.base.data.StaticField.NormalAltStepTree.{stepLabelsPerLevel => UseCaseStepLabels}

/**
 * Very old way of generating trees.
 */
object TreeDSL {

  case class NC(node: String, children: List[NC])
  def $(nodes: NC*) = nodes.toList
  implicit def nodeWithoutChildren(n: String) = NC(n, Nil)
  implicit class StringAsNode(val s: String) { def ~>(children: List[NC]) = NC(s, children) }
  implicit class NCListExt(val ncs: List[NC]) {
    val regex = """^(\S+?)/(\S+)$""".r
    val labelSplit = """^(\S+\.)?([^\.]+)$""".r
    def toStepNodes: List[StepNodeWithText] = toStepNodes(0, "", true)
    def toStepNodesN: List[StepNodeWithText] = toStepNodes(0, "", false)
    def toStepNodes(lvl: Int, idPrefix: String, genIds: Boolean): List[StepNodeWithText] = ncs.map { nc =>
      val (lbl, txt) = if (regex.pattern.matcher(nc.node).matches) {
        val regex(l, t) = nc.node; (l, t)
      } else
        (nc.node, "Step:" + nc.node)
      val id = idPrefix + lbl
      val ch = nc.children.toStepNodes(lvl + 1, id + ".", genIds)
      val labelSplit(lblPrefix, lblSuffix) = lbl
      val lblIndex = UseCaseStepLabels(lvl).parseTmp(lblSuffix)
      val id2 = if (genIds) id else null
      StepNodeWithText(LocalStepId(id2), lvl, lblIndex, txt, ch)
    }
  }

  type NodeChange = (String, List[NC])
  def changeChildren(nodes: List[StepNodeWithText], changes: NodeChange*): List[StepNodeWithText] = nodes.map { n =>
    val matches = for ((id, c) <- changes if id == n.id.value) yield c
    val ch = if (matches.isEmpty) n.children else matches(0).toStepNodes
    n.copy(children = changeChildren(ch, changes: _*))
  }
}