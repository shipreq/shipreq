package shipreq.webapp.feature.uc.step

import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import shipreq.webapp.test.DataGenerators._
import shipreq.webapp.test.TestHelpers

class StepTreeZipperTest extends FunSpec with TestHelpers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxSize = 10, minSuccessful = Cores * 10, workers = Cores)

  implicit lazy val arbNcSfv: Arbitrary[NcSfv] = Arbitrary(genNcSfv(NCF))

  describe("DeepZipper") {
    def deepB(v: NcSfv) = StepTreeZipper.DeepBuilder(v.sfv.textmap, v.stepsAndLabels.value.ab)
    def deep(v: NcSfv) = deepB(v).build(v.sfv.tree.head, v.sfv.tree.tail)

    it("length") {
      forAll((v: NcSfv) => deep(v).length ==== v.sfv.tree.nodes.size)
    }

    it("down & flat") {
      forAll((v: NcSfv) => {
        val b = deepB(v)

        def test(z: b.TreeZipper): Unit =
          z.focus.down match {
            case None =>
              z.focus.node.children.size ==== 0
            case Some(z2) =>
              z2.focus.label should startWith(z.focus.label + ".")
              z2.focus.flat.focus.label ==== z2.focus.label
              z2.focus.flat.length ==== TreeLike(z2.focus.node.children).sizeRecursive + 1
              test(z2)
          }

        test(b.build(v.sfv.tree.head, v.sfv.tree.tail))
      })
    }
  }

  describe("FlatZipper") {
    def flatB(v: NcSfv) = StepTreeZipper.FlatBuilder(v.sfv.textmap, v.stepsAndLabels.value.ab)
    def flat(v: NcSfv) = flatB(v).build(v.sfv.tree.head, v.sfv.tree.tail)

    it("length") {
      forAll((v: NcSfv) => flat(v).length ==== v.sfv.tree.sizeRecursive)
    }
    it("covers all labels") {
      forAll((v: NcSfv) => flat(v).toStream.map(_.label).toList.sorted ==== v.stepsAndLabels.value.bs.toList.sorted)
    }
  }
}
