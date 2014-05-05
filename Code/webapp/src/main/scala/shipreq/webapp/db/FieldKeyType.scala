package shipreq.webapp
package db

import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.jdbc.JdbcBackend.Session
import feature.uc.field._
import lib.Types._

/**
 * Represents types of fields that a use case (or something else) can have.
 *
 * @since 22/05/2013
 */
sealed abstract class FieldKeyType(val id: Short) {
  def name: String
  def fieldDefn(data: FieldKeyRecData): FieldDefinition
  /** Indicates the maximum number of `text` table records per-UC for this FK. */
  def maxTextPerUc: Option[Short]
}

object FieldKeyType {

  /**
   * A field with a name and a single text value.
   */
  case object Text extends FieldKeyType(30) {
    override def name = "Text"
    override def fieldDefn(data: FieldKeyRecData) = TextFieldDefinition(data.get)
    override def maxTextPerUc = Some(1: Short)
  }

  /**
   * A composite field of Normal Course, and Alternate Course use case step trees.
   */
  case object NormalAndAlternateCourses extends FieldKeyType(31) {
    override def name = "NormalAndAlternateCourses"
    override def fieldDefn(data: FieldKeyRecData) = NormalCourseFieldDefinition
    override def maxTextPerUc = None
  }

  /**
   * A field of Exception Course use case step trees.
   */
  case object ExceptionCourses extends FieldKeyType(32) {
    override def name = "ExceptionCourses"
    override def fieldDefn(data: FieldKeyRecData) = ExceptionCourseFieldDefinition
    override def maxTextPerUc = None
  }

  /**
   * A field that displays a flow graph.
   */
  case object FlowGraph extends FieldKeyType(33) {
    override def name = "FlowGraph"
    override def fieldDefn(data: FieldKeyRecData) = FlowGraphFieldDefinition
    override def maxTextPerUc = Some(0: Short)
  }

  val Values = List(Text, NormalAndAlternateCourses, ExceptionCourses, FlowGraph)

  // -------------------------------------------------------------------------------------------------------------------

  // Lookup by ID. Hardcoded for speed. Will be replaced when Scala 2.11 arrives with real enums.
  def apply(id: Short): FieldKeyType = id match {
    case Text.id => Text
    case NormalAndAlternateCourses.id => NormalAndAlternateCourses
    case ExceptionCourses.id => ExceptionCourses
  }

  assume(Values.map(_.id).distinct.size == Values.size, "Duplicate IDs detected.")
  assume(Values.map(_.name).distinct.size == Values.size, "Duplicate names detected.")

  def init(implicit s: Session): Unit = {
    val u = Q.update[(String, Short)](s"UPDATE field_key_type SET name=? WHERE id=?")
    val i = Q.update[(Short, String, Option[Short])](s"INSERT INTO field_key_type VALUES(?,?,?)")
    for (v <- Values)
      if (u.first((v.name, v.id)) == 0)
        i.execute(v.id, v.name, v.maxTextPerUc)
  }
}
