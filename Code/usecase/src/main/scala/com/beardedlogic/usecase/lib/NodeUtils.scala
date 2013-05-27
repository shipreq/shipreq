package com.beardedlogic.usecase.lib

import scala.collection.mutable.MutableList
import scala.collection.mutable.{ Map => MutableMap }

/**
 * @since 06/05/2013
 */
object NodeUtils {
  import StepTree.{ Step, StepNode }
  import StepLabels.LabelMakers

  /**
   * Parses a textual representation of a tree.
   *
   * Each line must match the format "<indent><label>. <step text>"
   * Indents must be spaces in multiples of 2.
   */
  def parseStepTree(txt: String): List[StepNode] = {
    val nodes = new MutableList[StepNode]
    val parents = MutableMap[Int, StepNode]()
    val children = MutableMap[StepNode, MutableList[StepNode]]()
    val lineRegex = """^\s*(\S+?)\. (\S+)$""".r
    val topLevelLabel = """^(\S+\.)(\d+)$""".r

    val lines = txt.split("""\s*[\r\n]+""").map(_.replaceFirst("\\s+$", "")).filter(!_.isEmpty)
    val commonIndentSize = lines.map(_.replaceFirst("\\S.+", "").length).min
    val linesWithoutIndent = lines.map(_.substring(commonIndentSize))

    for (line <- linesWithoutIndent) {

      // Parse line
      val indentSize = line.replaceFirst("\\S.+", "").length
      if (indentSize % 2 != 0) throw new RuntimeException("Odd indent size: " + line)
      val indent = indentSize >> 1
      val lineRegex(label, stepText) = line

      // Create node
      val n =
        if (indent == 0) {
          val topLevelLabel(labelPrefix, labelSuffix) = label
          val labelIndex = LabelMakers(0)(labelSuffix)
          val n = new StepNode(label, 0, labelIndex, Step(stepText))
          nodes += n
          n
        } else {
          val p = parents(indent - 1)
          val labelIndex = LabelMakers(indent)(label)
          val n = new StepNode(s"${p.id}.${label}", indent, labelIndex, Step(stepText))
          children(p) += n
          n
        }
      parents(indent) = n
      children(n) = new MutableList[StepNode]
    }

    // Convert to immutable tree
    def addChildren(n: StepNode): StepNode = {
      val nKey = n.copy(children = Nil)
      val ch = children(nKey).map { addChildren(_) }.toList
      n.copy(children = ch)
    }
    nodes.map { addChildren(_) }.toList
  }

  /**
   * Recursively sets all IDs to null.
   */
  def removeIds(l: List[StepNode]): List[StepNode] =
    l.map((n) => n.copy(id = null, children = removeIds(n.children)))

  /**
   * Builds a textual representation of a tree.
   */
  def inspectTree(tree: List[StepNode], indent: String = "", res: List[String] = Nil): List[String] = tree match {
    case Nil => res
    case h :: t =>
      val s = s"${indent}${h.label}. ${h.step.text}"
      val ch = inspectTree(h.children, indent + "  ")
      inspectTree(t, indent, res ::: s :: ch)
  }

  /**
   * Builds a side-by-side, textual representation of two trees.
   */
  def inspectTrees(title1: String, nodes1: List[StepNode], title2: String, nodes2: List[StepNode]): String = {
    val sb = new StringBuilder(1024, "")
    val t1 = inspectTree(nodes1).toIndexedSeq
    val t2 = inspectTree(nodes2).toIndexedSeq
    val t1Size = (title1 +: t1) map (_.length) max
    val t2Size = (title2 +: t2) map (_.length) max
    val fmt = s"%-${t1Size}s | %-${t2Size}s | %s\n"
    val size = Vector(t1.size, t2.size).max
    def x(l: IndexedSeq[String], i: Int) = if (i >= l.size) "" else l(i)
    sb ++= String.format(fmt, title1, title2, "") +
      ("-" * t1Size + "-+-" + "-" * t2Size + "-+\n")
    for (i <- 0 until size) {
      val (a, b) = (x(t1, i), x(t2, i))
      val c = if (a == b) "" else "#"
      sb ++= String.format(fmt, a, b, c)
    }
    sb.toString
  }

  /**
   * Prints a tree to stdout.
   */
  def printTree(tree: List[StepNode]) { inspectTree(tree).foreach { println(_) } }

  /**
   * Prints two trees, side-by-side, to stdout.
   */
  def printTrees(title1: String, nodes1: List[StepNode], title2: String, nodes2: List[StepNode]) {
    print(inspectTrees(title1, nodes1, title2, nodes2))
  }

  /**
   * Recursively corrects the level of all nodes in a tree.
   */
  def fixLevels(nodes: List[StepNode], lvl: Int = 0): List[StepNode] = nodes.map { n =>
    n.copy(level = lvl, children = fixLevels(n.children, lvl + 1))
  }
}