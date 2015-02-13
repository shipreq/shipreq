package shipreq.webapp.client.app.ui

import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import shipreq.webapp.base.data

import scala.collection.GenTraversable
import scalaz.NonEmptyList
import scalaz.effect.IO

/**
 * Requirements Table.
 * "Common Req View & Editor" in the prototype.
 *
 * An Excel-like table for reading and editing requirements.
 */
package object reqtable {

  sealed trait SortDir
  object SortDir {
    case object Asc  extends SortDir
    case object Desc extends SortDir
  }

  final case class SortCriterion[A](criterion: A, dir: SortDir)

  case class ViewSettings(columns: Vector[Column],
                          order  : Vector[SortCriterion[Column]])
}
