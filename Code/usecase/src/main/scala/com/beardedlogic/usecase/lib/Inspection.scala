package com.beardedlogic.usecase
package lib

import java.lang.{Long => JLong}
import org.apache.commons.lang3.StringEscapeUtils.escapeJava
import scala.reflect.runtime.universe.{TypeTag => ReflectTypeTag}
import scalaz.{Cord, Show, Name, Need, Value}
import scalaz.syntax.show._

import db.{FieldKeyType, FieldKeyRec}
import Types._
import field._
import text._
import util.BiMap

/**
 * Typeclasses of `scalaz.Show` that returns code that can be pasted back into Scala.
 * Inspired by Ruby's `inspect()` method.
 *
 * To use:
 *   import scalaz.syntax.show._, Inspection._
 * Then do things like
 *   `uc.println`, `uc.show`
 */
object Inspection {

  private val `"`: Cord = "\""
  private val `,`: Cord = ","
  private val `->`: Cord = "->"
  private val `~>`: Cord = "~>"
  private val `(`: Cord = "("
  private val `)`: Cord = ")"
  private val nil: Cord = "Nil"
  private val eol: Cord = "\n"

  private implicit class StringExt(val s: String) extends AnyVal {
    @inline def <>(body: Cord): Cord = (s: Cord) <> body
    @inline def <*>[T](f: T => Cord): Show[T] = {
      val c: Cord = s
      Show.show(t => c <> f(t))
    }
  }

  private implicit class CordExt(val c: Cord) extends AnyVal {
    @inline def <>(body: Cord): Cord = c ++ `(` ++ body ++ `)`
    @inline def ++>(next: Cord): Cord = c ++ `,` ++ next
  }

  // ===================================================================================================================
  // Generic

  implicit def unitInstance    = scalaz.std.anyVal.unitInstance
  implicit def intInstance     = scalaz.std.anyVal.intInstance
  implicit def longInstance    = scalaz.std.anyVal.longInstance
  implicit def shortInstance   = scalaz.std.anyVal.shortInstance
  implicit def booleanInstance = scalaz.std.anyVal.booleanInstance
  implicit def optionInstance[A: Show]: Show[Option[A]] = scalaz.std.option.optionShow

  implicit val str: Show[String] = Show.show(`"` ++ escapeJava(_) ++ `"`)

  implicit def listShow[A: Show]: Show[List[A]] =
    Show.show(l =>
      if (l.isEmpty) nil
      else "List" <> Cord.mkCord(`,`, l.map(Show[A].show): _*))

  implicit def mapShow[K, V](implicit K: Show[K], V: Show[V]): Show[Map[K, V]] =
    "Map" <*> (m =>
      Cord.mkCord(`,`, m.toSeq.view.map {
        case (k, v) => Cord(K show k, `->`, V show v)
      }: _*))

  implicit def bimapShow[K: Show, V: Show]: Show[BiMap[K, V]] = Show.show(b => "Bi" +: b.ab.show)

  implicit def nameShow[A: Show]: Show[Name[A]] = "Name" <*> (_.value.show)
  implicit def needShow[A: Show]: Show[Need[A]] = "Need" <*> (_.value.show)
  implicit def valueShow[A: Show]: Show[Value[A]] = "Value" <*> (_.value.show)

  // ===================================================================================================================
  // Type tags

  private def getTagName(tag: ReflectTypeTag[_]): String = tag.tpe.toString.replaceFirst("^.+\\.","")

  private def taggedStr[Tag <: TypeTag[String]](implicit tt: ReflectTypeTag[Tag]): Show[String @@ Tag] = {
    val tagClass = getTagName(tt)
    val tagSuffix: Cord = s".tag[$tagClass]"
    Show.show(s => str.show(s) ++ tagSuffix)
  }

  private def taggedLong[Tag <: TypeTag[Long]](implicit tt: ReflectTypeTag[Tag]): Show[JLong @@ Tag] = {
    val tagClass = getTagName(tt)
    val tagSuffix: Cord = s".tag[$tagClass]"
    Show.show(s => s.toString +: tagSuffix)
  }

  implicit val textWithNRefs   : Show[TextWithNormalisedRefs] = taggedStr[TextWithNormalisedRefsTag]
  implicit val localTextFieldId: Show[LocalTextFieldId]       = taggedStr[LocalTextFieldIdTag]
  implicit val localStepId     : Show[LocalStepId]            = taggedStr[LocalStepIdTag]
  implicit val labelStr        : Show[LabelStr]               = taggedStr[LabelTag]

  implicit val fieldKeyId : Show[FieldKeyId]     = taggedLong[FieldKeyIdTag]
  implicit val ucIdentId  : Show[UseCaseIdentId] = taggedLong[UseCaseIdentIdTag]
  implicit val ucRevId    : Show[UseCaseRevId]   = taggedLong[UseCaseRevIdTag]
  implicit val textIdentId: Show[TextIdentId]    = taggedLong[TextIdentIdTag]
  implicit val textRevId  : Show[TextRevId]      = taggedLong[TextRevIdTag]
  implicit val userId     : Show[UserId]         = taggedLong[UserIdTag]

  // ===================================================================================================================
  // Fields and values

  implicit val fieldKeyType: Show[FieldKeyType] = {
    val prefix: Cord = "FieldKeyType."
    Show.show(prefix :+ _.toString)
  }

  implicit val fieldKeyRec: Show[FieldKeyRec] = "FieldKeyRec" <*> (x => x.id.show ++> x.fkType.show ++> x.data.show)

  implicit val textFieldDefn: Show[TextFieldDefinition] = "TextFieldDefinition" <*> (_.title.show)

  private val fieldM: Field => Cord = _ match {
    case f: TextField            => "TextField" <> f.defn.show ++> f.rec.show
    case f: NormalCourseField    => "NormalCourseField" <> f.rec.show
    case f: ExceptionCourseField => "ExceptionCourseField" <> f.rec.show
  }
  implicit val field: Show[Field] = Show.show(fieldM)
  implicit val stepField: Show[StepField] = Show.show(fieldM)

  implicit val freeText: Show[FreeText] = {
    val empty: Cord = "FreeText.empty"
    Show.show(x => if (x.isEmpty) empty else "FreeText" <> x.text.show ++> x.refs.show)
  }

  implicit val flowFromClause: Show[FlowFromClause] = "FlowFromClause" <*> (_.refs.show)

  implicit val flowToClause: Show[FlowToClause] = "FlowToClause" <*> (_.refs.show)

  implicit val stepText: Show[StepText] = Show.show(x =>
    if (x.isEmpty) "StepText.empty" <> x.stepId.show
    else "StepText" <> x.stepId.show ++> x.mainClause.show ++> x.flowFromClause.show ++> x.flowToClause.show
  )

  implicit val stepTree: Show[StepTree] = "StepTree" <*> (_.nodes.show)

  implicit val stepNode: Show[StepNode] = "StepNode" <*> (x =>
    x.id.show ++> x.level.show ++> x.labelIndex.show ++> x.children.show)

  implicit val sfv: Show[StepFieldValue] = "StepFieldValue" <*> (x => x.field.show ++> x.tree.show ++> x.textmap.show)

  implicit val fieldValue: Show[Field#Value] = Show.show(_ match {
    case v: FreeText       => Show[FreeText].show(v)
    case v: StepFieldValue => Show[StepFieldValue].show(v)
  })

  // ===================================================================================================================
  // Use Case

  implicit val uchShow: Show[UseCaseHeader] = "UseCaseHeader" <*> (x => x.number.show ++> x.title.show)

  implicit val ucShow: Show[UseCase] = "UseCase.shortcut" <*> (x => {
    val fvTuples = x.fields.map(f => f.show ++ `~>` ++ x.fieldValues(f).show ++ eol)
    x.header.show ++ eol ++> fvTuples.show ++> x.stepsAndLabels.show
  })
}
