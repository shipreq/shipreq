package com.beardedlogic.usecase.test

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.StepLabels._

/**
 * Very old way of generating trees.
 */
object TreeDSL {

  case class NC(val node: String, val children: List[NC])
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
      val lblIndex = LabelMakers(lvl)(lblSuffix)
      val id2 = if (genIds) id else null
      StepNodeWithText(id2.asLocalStepId, lvl, lblIndex, txt, ch)
    }
  }

  type NodeChange = Tuple2[String, List[NC]]
  def changeChildren(nodes: List[StepNodeWithText], changes: NodeChange*): List[StepNodeWithText] = nodes.map { n =>
    val matches = for ((id, c) <- changes if id == n.id) yield c
    val ch = if (matches.isEmpty) n.children else matches(0).toStepNodes
    n.copy(children = changeChildren(ch, changes: _*))
  }
}