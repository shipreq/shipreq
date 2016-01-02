package shipreq.webapp.base.test

import nyaya.gen._
import shipreq.base.test.BaseUtilGen._
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

    val nonEmptyValues: Gen[gd.NonEmptyValues] =
      attr.nes
        .flatMap(as => Gen sequence as.iterator.map(valueFor).toVector)
        .map(vs => gd.nev(vs.head, vs.tail: _*))
  }

}
