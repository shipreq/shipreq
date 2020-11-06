package shipreq.webapp.member.project.data.derivation

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.member.UiText.sortedOrClause
import shipreq.webapp.member.project.data._

final class UseCaseStepLabelLookup(reqs: Requirements) {
  import UseCaseStepLabelLookup._

  private val perUC: ReqTypePos => PerUC =
    Memo(new PerUC(reqs, _))

  def apply(ucPos: ReqTypePos, input: String, allowAliases: Boolean): UseCaseStepLabelLookup.Result = {
    var i = input
    i = i.replace(" ", "") // whitespace has been normalised at this point (by the pre-parser)
    i = normalise(i)

    if (i.isEmpty || i.endsWith(".") || blacklist.contains(i))
      -\/(Failure.NotFound)
    else {
      val lookup = perUC(ucPos)
      lookup(i, allowAliases) match {
        case -\/(Failure.NotFound) =>
          // Removing leading zeros from digits and try again
          // eg. 1.02.1 => 1.2.1
          i = trailingZeros.replaceAllIn(i, "$1$2")
          lookup(i, allowAliases)

        case otherwise =>
          otherwise
      }
    }
  }

}

object UseCaseStepLabelLookup {

  type Result = Failure \/ UseCaseStepId

  sealed trait Failure

  object Failure {
    case object NotFound extends Failure

    final case class Ambiguous(one: String, two: String, others: Set[String]) extends Failure {
      val all = others + one + two

      def errMsg(input: String): String =
        s"$input is ambiguous. Did you mean ${sortedOrClause(all, limit = 4)}?"
    }

    implicit def univEq: UnivEq[Failure] = UnivEq.derive
  }

  private[UseCaseStepLabelLookup] def normalise(lbl: String): String =
    lbl.toLowerCase

  private[UseCaseStepLabelLookup] val blacklist: Set[String] =
    (StaticField.useCaseStepTrees.iterator.flatMap(_.stepLabelPrefix)
      ++ Iterator.single(WebappConfig.useCaseStepsDeadNode.toString)
      ).map(normalise).toSet

  private[UseCaseStepLabelLookup] val trailingZeros =
    "(^|\\.)0+([0-9])".r

  // ===================================================================================================================

  private val listOfEmpty       = "" :: Nil
  private val listOfEmptyAndDot = "." :: listOfEmpty

  private final class PerUC(reqs: Requirements, ucPos: ReqTypePos) {

    private val unique: Map[String, UseCaseStepId] = {
      var m = Map.empty[String, UseCaseStepId]
      for {
        uc          <- reqs.getUseCaseByPos(ucPos)
        field       <- StaticField.useCaseStepTrees
        steps       = field.useCaseSteps.get(uc)
        treeFilter  = field.treeFilterAll(steps.tree)
        (loc, step) <- steps.tree.subtreeLocAndValueIterator(treeFilter, (_, _))
      } {
        val partialLoc = steps.partialLocs.forward(loc)
        val id         = step.id
        val fullLabel  = normalise(field.stepLabel(ucPos, partialLoc, UseCaseStepLabelFmt.`N.m`))
        m = m.updated(fullLabel, id)
      }
      m
    }

    private lazy val aliases: Map[String, Set[UseCaseStepId]] = {
      var m = Map.empty[String, Set[UseCaseStepId]]

      @inline def addAlias(label: String, id: UseCaseStepId): Unit =
        m = m.updated(label, m.getOrElse(label, Set.empty) + id)

      for ((fullLabel, id) <- unique) {
        val parts = fullLabel.split('.')

        @tailrec
        def go(idx: Int, suffixes: List[String]): Unit = {
          val part = parts(idx)
          val values: List[String] =
            for {
              sep    <- if (idx == 0) listOfEmpty else listOfEmptyAndDot
              suffix <- suffixes
            } yield {
              val x = sep + part + suffix
              addAlias(x, id)
              x
            }
          if (idx > 0) {

            go(idx - 1, values)
          }
        }

        go(parts.length - 1, listOfEmpty)
      }
      m
    }

    private def fullLabelFor(id: UseCaseStepId): String =
      reqs.useCases.focusStep(id).label(UseCaseStepLabelFmt.`N.m`)

    def apply(s: String, allowAliases: Boolean): UseCaseStepLabelLookup.Result = {
      // println(s"Searching for [$s]")
      unique.get(s) match {
        case Some(id) =>
          \/-(id)

        case None =>
          if (allowAliases)
            aliases.get(s) match {

              case Some(ids) if ids.nonEmpty =>
                val id   = ids.head
                val tail = ids.tail
                if (tail.isEmpty)
                  \/-(id)
                else {
                  val a = fullLabelFor(id)
                  val b = fullLabelFor(tail.head)
                  val c = tail.tail.map(fullLabelFor)
                  -\/(Failure.Ambiguous(a, b, c))
                }

              case _ =>
                -\/(Failure.NotFound)
            }
          else
            -\/(Failure.NotFound)
      }
    }

    @elidable(elidable.INFO)
    def debugPrint(): Unit = {
      def go(m: Map[String, Set[UseCaseStepId]]) =
        m.toList.map {
          case (lbl, ids) => "%-12s -- %s".format(lbl + "|", ids.map(_.value).toList.sorted.mkString(", "))
        }.sorted.foreach(println)
      go(unique.mapValuesNow(Set(_)))
      println()
      go(aliases -- unique.keySet)
    }

    // println(s"Steps: ${unique.size}, aliases: ${aliases.values.foldLeft(0)(_ + _.size)}")
    // debugPrint()
  }

}
