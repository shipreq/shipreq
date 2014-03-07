package shipreq.webapp.lib

import scalaz.{NonEmptyListFunctions, IdInstances}
import scalaz.std.{StringInstances, OptionInstances, TupleInstances, NodeSeqInstances, MapInstances, ListInstances, FunctionInstances}
import scalaz.syntax.{ToShowOps, ToMonadOps, ToMonadPlusOps, ToFunctorOps, ToFoldableOps, ToBifunctorOps, ToMonoidOps, ToSemigroupOps}

final object ScalazSubset

  extends IdInstances

          with ToSemigroupOps
          with ToMonoidOps
          with ToMonadPlusOps
          with ToMonadOps
          with ToBifunctorOps
          with ToFoldableOps
          with ToFunctorOps
          with ToShowOps

          with FunctionInstances
          with ListInstances
          with OptionInstances
          with StringInstances
          with MapInstances
          with NodeSeqInstances
          with TupleInstances

          with NonEmptyListFunctions
