package shipreq.webapp.base.test

import japgolly.microlibs.nonempty._
import nyaya.gen._
import shipreq.base.test.BaseUtilGen._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.util.GenericData

object WebappBaseGen {

  abstract class GenericDataGen[GD <: GenericData](final val gd: GD) {
    import gd.equalityAttr

    def valueFor(a: gd.Attr): Gen[gd.Value]

    val attr = Gen.chooseNE(gd.attrs)

    lazy val values: Gen[gd.Values] =
      attr.set
        .flatMap(as => Gen sequence as.iterator.map(valueFor).toVector)
        .map(_.foldLeft(gd.emptyValues)(_ + _))

    lazy val nonEmptyValues: Gen[gd.NonEmptyValues] =
      attr.nes.flatMap(forValues)

    lazy val allValues: Gen[gd.NonEmptyValues] =
      forValues(gd.attrs)

    def forValues(as: NonEmptySet[gd.Attr]): Gen[gd.NonEmptyValues] =
      Gen.sequence(as.iterator.map(valueFor).toVector)
        .map(vs => gd.nev(vs.head, vs.tail: _*))
  }

  abstract class GenericDataOptionGen[GD <: GenericData](final val gd: GD) {
    import gd.equalityAttr

    def valueFor(a: gd.Attr): Option[Gen[gd.Value]]

    val gens: Map[gd.Attr, Gen[gd.Value]] =
      gd.attrs.iterator
        .map(a => (a, valueFor(a)))
        .filterDefined_2
        .toMap

    val genableAttrs: Option[NonEmptySet[gd.Attr]] =
      NonEmptySet.option(gens.keySet)

    val attrNE: Option[Gen[gd.Attr]] =
      genableAttrs.map(Gen chooseNE _)

    def values(implicit ss: SizeSpec): Gen[gd.Values] =
      attrNE.fold(Gen pure gd.emptyValues)(_
        .set
        .flatMap(as => Gen sequence as.iterator.map(gens).toVector)
        .map(_.foldLeft(gd.emptyValues)(_ + _)))

    lazy val nonEmptyValues: Option[Gen[gd.NonEmptyValues]] =
      attrNE.map(_.nes.flatMap(forValues))

    lazy val allPossibleValues: Option[Gen[gd.NonEmptyValues]] =
      genableAttrs.map(forValues)

    lazy val allValues: Option[Gen[gd.NonEmptyValues]] =
      if (gens.keySet.size == gd.attrs.size)
        allPossibleValues
      else
        None

    def forValues(as: NonEmptySet[gd.Attr]): Gen[gd.NonEmptyValues] =
      Gen.sequence(as.iterator.map(gens).toVector)
        .map(vs => gd.nev(vs.head, vs.tail: _*))
  }

}
