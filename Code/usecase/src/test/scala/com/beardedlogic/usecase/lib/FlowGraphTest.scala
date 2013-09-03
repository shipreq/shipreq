package com.beardedlogic.usecase.lib

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import scalaz.{NonEmptyList => NEL}
import com.beardedlogic.usecase.test.{StepNodeWithText, TestData}
import com.beardedlogic.usecase.test.NodeUtils.parseStepTree
import text.{FlowToClause, Flow, FlowFromClause, StepText}
import UseCaseFns.generateStepAndLabelMap
import FlowGraph._
import Types._
import StepTreeZipper._

class FlowGraphTest extends FunSpec with TestData with PropertyChecks {

  implicit def s2n(x: String): Node = x.asLabel
  implicit def autoLabelT(t: (String, String)): ExplicitFlow = (t._1.asLabel, t._2.asLabel)
  implicit def autoLabelL(l: List[String]): List[Node] = l map s2n
  implicit def dz2L(z: DeepZipper): List[DeepFocus] = z.toStream.toList
  def nl(prefix: String, range: Range): List[Node] = range map (i => s"$prefix.$i".asLabel) toList
  //def nnel(prefix: String, range: Range): NEL[Node] = nl(prefix, range) match {case h::t => NEL(h,t:_*)}
  def hnNel(prefix: String, range: Range): NEL[Node] = NEL(prefix, nl(prefix, range): _*)

  implicit def testTreeToDeepZipper(nodes: List[StepNodeWithText]): DeepZipper = {
    val tree = nodes.toStepTree
    val b = DeepBuilder(nodes.toTextmap(), generateStepAndLabelMap(NCF, tree, UCH))
    b.build(tree.nodes.head, tree.nodes.tail)
  }

  def deepZipperFor(t: List[StepNodeWithText]): DeepZipper = {
    val tree = t.toStepTree
    val textmap = t.toTextmap()
    val b = DeepBuilder(textmap, generateStepAndLabelMap(NCF, tree, UCH))
    b.build(tree.nodes.head, tree.nodes.tail)
  }

  def mod(z: DeepZipper, stepId: String)(f: StepText => StepText): DeepZipper = {
    val b = z.focus.builder
    val id = stepId.asLocalStepId
    val oldValue = b.textmap(id)
    val newValue = f(oldValue)
    val b2 = b.copy(textmap = b.textmap + (id -> newValue))
    b2.build(z.focus.node, z.rights.map(_.node))
  }

  val refs: Flow.Refs = Map(X1 -> S1)
  val addFlowFromClause = (s: StepText) => s.copy(flowFromClause = Some(FlowFromClause(refs)))
  val addFlowToClause = (s: StepText) => s.copy(flowToClause = Some(FlowToClause(refs)))

  import Renderer.{render, NC, AC, EC}

  describe("Modeller") {
    import Modeller._

    lazy val m = model(MockUc4.UC)

    it("startNodes") {
      m.socialData.startNodes.sorted ==== List("7.0", "7.2")
    }
    it("endNodes") {
      m.socialData.endNodes.sorted ==== List("7.0.3", "7.1")
    }
    it("explicitFlows") {
      m.socialData.explicitFlows.sorted ==== List(("7.0.2.a" -> "7.0.1"), ("7.0.2.a" -> "7.1"), ("7.2.1" -> "7.0.3"))
    }
    it("NC.headNodes") {
      m.intraCatData(NC).headNodes ==== List("7.0")
    }
    it("NC.implicitFlows") {
      m.intraCatData(NC).implicitFlows ==== List(NEL("7.0", "7.0.1", "7.0.2", "7.0.2.a", "7.0.3"))
    }
    it("AC.headNodes") {
      m.intraCatData(AC).headNodes ==== List("7.1", "7.2")
    }
    it("AC.implicitFlows") {
      m.intraCatData(AC).implicitFlows ==== List(NEL("7.1"), hnNel("7.2", 1 to 1))
    }

    lazy val testTree = parseStepTree( """
          7.0. _
            1. _
              a. _
              b. _
            2. _
              a. _
                i. _
                ii. _
                iii. _
            3. _ """)
    lazy val testTreeZ = deepZipperFor(testTree)

    describe("implicit flows") {
      it("should hide all children when no flows") {
        (implicitFlows(testTreeZ): List[ImplicitFlow]) ==== List(hnNel("7.0", 1 to 3))
      }

      it("should not hide L1") {
        model(MockUc1.sampleUC).intraCatData(NC).implicitFlows ==== List(hnNel("7.0", 1 to 3))
      }

      it("should not hide L2+ generations that have flows") {
        val examples = Table[List[String], List[ImplicitFlow]](("Step", "Expected implicitFlows")
          , (List("7.0.1", "7.0.2", "7.0.3"), List(hnNel("7.0", 1 to 3)))
          , (List("7.0.1.a", "7.0.1.b"), List(NEL("7.0", "7.0.1", "7.0.1.a", "7.0.1.b", "7.0.2", "7.0.3")))
          , (List("7.0.2.a"), List(NEL("7.0", "7.0.1", "7.0.2", "7.0.2.a", "7.0.3")))
          , (List("7.0.2.a.i", "7.0.2.a.ii", "7.0.2.a.iii"), List(NEL("7.0", "7.0.1", "7.0.2", "7.0.2.a", "7.0.2.a.i", "7.0.2.a.ii", "7.0.2.a.iii", "7.0.3")))
        )
        val clauses = List(addFlowFromClause, addFlowToClause)
        forAll(examples)((steps, results) =>
          for {step <- steps; clauseModFn <- clauses} {
            val z = mod(testTreeZ, step)(clauseModFn)
            (implicitFlows(z): List[ImplicitFlow]) ==== results
          })
      }
    }

    describe("end nodes") {
      it("should always be the last NC node") {
        implicit val c = NC
        endNodes(implicitFlows(testTreeZ)) ==== List("7.0.3")
        endNodes(implicitFlows(mod(testTreeZ, "7.0.3")(addFlowToClause))) ==== List("7.0.3")
      }
      it("should be the last AC nodes without flow-to") {
        implicit val c = AC
        endNodes(implicitFlows(testTreeZ)) ==== List("7.0.3")
        endNodes(implicitFlows(mod(testTreeZ, "7.0.3")(addFlowToClause))) ==== Nil
      }
      it("should be the last EC nodes without flow-to") {
        implicit val c = EC
        endNodes(implicitFlows(testTreeZ)) ==== List("7.0.3")
        endNodes(implicitFlows(mod(testTreeZ, "7.0.3")(addFlowToClause))) ==== Nil
      }
      it("should trickle up to parent when children are hidden") {
        implicit val c = NC
        val z = testTreeZ.modify(f => f.copy(node = f.node.copy(children = f.node.children.dropRight(1))))
        endNodes(implicitFlows(z)) ==== List("7.0.2")
        endNodes(implicitFlows(mod(z, "7.0.2.a")(addFlowToClause))) ==== List("7.0.2.a")
        endNodes(implicitFlows(mod(z, "7.0.2.a.i")(addFlowToClause))) ==== List("7.0.2.a.iii")
      }
    }
  }

  describe("Renderer") {
    val nc = IntraCatData(List("1.0"), List(hnNel("1.0", 1 to 9)))
    val ac = IntraCatData(List("1.1"), List(hnNel("1.1", 1 to 5)))
    val ec = IntraCatData(List("1.E.1"), List(hnNel("1.E.1", 1 to 3)))
    val ef: List[ExplicitFlow] = List(
      ("1.0.4" -> "1.0.7")
      , ("1.0.6" -> "1.0.1")
      , ("1.0.1" -> "1.1")
      , ("1.0.4" -> "1.1")
      , ("1.1.5" -> "1.0.1")
      , ("1.0.9" -> "1.E.1")
      , ("1.E.1.3" -> "1.0.1")
    )
    val sd = SocialData(ef, List("1.0","1.99"), List("1.0.9","99.99"))
    val m = FlowGraphModel(Map(NC -> nc, AC -> ac, EC -> ec), sd)
    lazy val g = render(m).toString

    def expect(s: String): Unit = g should include(s.replace('|','"'))

    it("startNodes") {
      expect("|1.0|;|1.99|")
    }
    it("endNodes") {
      expect("|1.0.9|;|99.99|")
    }
    it("explicitFlows") {
      expect("|1.0.4|->|1.0.7|")
      expect("|1.0.6|->|1.0.1|")
      expect("|1.E.1.3|->|1.0.1|")
    }
    // it("NC.headNodes") {
    it("NC.implicitFlows") {
      expect("|1.0|->|1.0.1|->|1.0.2|->|1.0.3|->|1.0.4|")
    }
    // it("AC.headNodes") {
    it("AC.implicitFlows") {
      expect("|1.1|->|1.1.1|->|1.1.2|->|1.1.3|->|1.1.4|")
    }
    it("EC.implicitFlows") {
      expect("|1.E.1|->|1.E.1.1|->|1.E.1.2|->|1.E.1.3|")
    }
  }

  // it("Generates DOT for arbitrary UCs") {
  // import com.beardedlogic.usecase.test.DataGenerators.useCaseGen
  // val x = useCaseGen(FieldListRec(FL.map(_.rec))).sample.get
  // println(render(model(x)))

  // val sfv = NcSfv
  // val b = DeepBuilder(sfv.textmap, UC.stepsAndLabels.get.ab)
  // val dz = b.build(sfv.tree.head, Nil)
  // implicit val fzs = flattenTopNodes(dz)
  // }
}
