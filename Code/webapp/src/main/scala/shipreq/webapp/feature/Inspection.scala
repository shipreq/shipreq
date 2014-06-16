package shipreq.webapp
package feature

import org.apache.commons.lang3.StringEscapeUtils.escapeJava
import org.joda.time.DateTime
import scala.reflect.runtime.universe.{TypeTag => ReflectTypeTag}
import scalaz.{Cord, Show, Name, Need, Value}
import scalaz.syntax.show._

import shipreq.base.util.TaggedTypes.TaggedType
import db.{UseCaseRev, UseCaseIdent, UseCaseHeader, FieldKeyType, FieldKeyRec}
import lib.Types._
import uc.UseCase
import uc.field._
import uc.step._
import uc.text._
import FreeTextTerms._
import shipreq.base.util.BiMap
import shipreq.taskman.api.UserId

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
  private val `()`: Cord = "()"

  private implicit class StringExt(val s: String) extends AnyVal {
    @inline def <>(body: Cord): Cord = (s: Cord) <> body
    @inline def <*>[T](f: T => Cord): Show[T] = {
      val c: Cord = s
      Show.show(t => c <> f(t))
    }
    @inline def const[T]: Show[T] = Show.show(_ => Cord(s))
  }

  private implicit class CordExt(val c: Cord) extends AnyVal {
    @inline def <>(body: Cord): Cord = c ++ `(` ++ body ++ `)`
    @inline def ++>(next: Cord): Cord = c ++ `,` ++ next
  }

  // ===================================================================================================================
  // Generic

  implicit def unitInstance    = scalaz.std.anyVal.unitInstance
  implicit def intInstance     = scalaz.std.anyVal.intInstance
  implicit def shortInstance   = scalaz.std.anyVal.shortInstance
  implicit def booleanInstance = scalaz.std.anyVal.booleanInstance
  implicit def optionInstance[A: Show]: Show[Option[A]] = scalaz.std.option.optionShow

  implicit val unit: Show[Unit] = Show.show(_ => `()`)
  implicit val str: Show[String] = Show.show(`"` ++ escapeJava(_) ++ `"`)
  implicit val long: Show[Long] = Show.show(_.toString + "L")

  implicit def listShow[A: Show]: Show[List[A]] =
    Show.show(l =>
      if (l.isEmpty) nil
      else "List" <> Cord.mkCord(`,`, l.map(Show[A].show): _*))

  def listWithCR[A: Show]: Show[List[A]] = Show.show(l => l.map(_.show ++ eol).show)
  def showListWithCR[A: Show](list: List[A]) = listWithCR[A].show(list)

  implicit def mapShow[K, V](implicit K: Show[K], V: Show[V]): Show[Map[K, V]] =
    "Map" <*> (m =>
      Cord.mkCord(`,`, m.toSeq.view.map {
        case (k, v) => Cord(K show k, `->`, V show v)
      }: _*))

  implicit def bimapShow[K: Show, V: Show]: Show[BiMap[K, V]] = Show.show(b => "Bi" +: b.ab.show)

  implicit def nameShow[A: Show]: Show[Name[A]] = "Name" <*> (_.value.show)
  implicit def needShow[A: Show]: Show[Need[A]] = "Need" <*> (_.value.show)
  implicit def valueShow[A: Show]: Show[Value[A]] = "Value" <*> (_.value.show)

  implicit def tuple2[A: Show, B: Show]: Show[Tuple2[A, B]] = Show.show(t => Cord(`(`, t._1.show, `,`, t._2.show, `)`))

  implicit val datetimeShow: Show[DateTime] = "new DateTime" <*> (_.getMillis.show)

  // ===================================================================================================================
  // Type tags

  private def taggedType[T <: TaggedType](implicit baseShow: Show[T#U]): Show[T] =
    Show.show(t => t.getClass.getSimpleName <> baseShow.show(t.value))

  implicit val fieldKeyId      : Show[FieldKeyId]       = taggedType[FieldKeyId]
  implicit val localTextFieldId: Show[LocalTextFieldId] = taggedType[LocalTextFieldId]
  implicit val localStepId     : Show[LocalStepId]      = taggedType[LocalStepId]
  implicit val textWithNRefs   : Show[NormalisedText]   = taggedType[NormalisedText]
  implicit val projectId       : Show[ProjectId]        = taggedType[ProjectId]
  implicit val labelStr        : Show[StepLabel]        = taggedType[StepLabel]
  implicit val textIdentId     : Show[TextIdentId]      = taggedType[TextIdentId]
  implicit val textRevId       : Show[TextRevId]        = taggedType[TextRevId]
  implicit val ucIdentId       : Show[UseCaseIdentId]   = taggedType[UseCaseIdentId]
  implicit val useCaseNumber   : Show[UseCaseNumber]    = taggedType[UseCaseNumber]
  implicit val ucRevId         : Show[UseCaseRevId]     = taggedType[UseCaseRevId]
  implicit val userId          : Show[UserId]           = taggedType[UserId]

  // ===================================================================================================================
  // Fields and values

  implicit val fieldKeyType: Show[FieldKeyType] = {
    val prefix: Cord = "FieldKeyType."
    Show.show(prefix :+ _.toString)
  }

  implicit val fieldKeyRec: Show[FieldKeyRec] = "FieldKeyRec" <*> (x => x.id.show ++> x.fkType.show ++> x.data.show)

  implicit val textFieldDefn: Show[TextFieldDefinition] = "TextFieldDefinition" <*> (_.title.show)

  private val fieldM: Field => Cord = {
    case f: TextField            => "TextField" <> f.defn.show ++> f.rec.show
    case f: NormalCourseField    => "NormalCourseField" <> f.rec.show
    case f: ExceptionCourseField => "ExceptionCourseField" <> f.rec.show
    case f: FlowGraphField       => "FlowGraphField" <> f.rec.show
  }
  implicit val field: Show[Field] = Show.show(fieldM)
  implicit val stepField: Show[StepField] = Show.show(fieldM)

  implicit val fttPlainText        : Show[PlainText]         = "PlainText" <*> (_.text.show)
  implicit val fttStepRef          : Show[StepRef]           = "StepRef" <*> (x => x.id.show ++> x.label.show)
  implicit val fttInvalidStepRef   : Show[InvalidStepRef]    = "InvalidStepRef" <*> (_.label.show)
  implicit val fttDeletedStepRef   : Show[DeletedRef.type]   = "DeletedRef".const
  implicit val fttUseCaseRef       : Show[UseCaseRef]        = "UseCaseRef" <*> (x => x.num.show ++> x.title.show)
  implicit val fttUseCaseSelfRef   : Show[UseCaseSelfRef]    = "UseCaseSelfRef" <*> (x => x.num.show ++> x.title.show)
  implicit val fttInvalidUseCaseRef: Show[InvalidUseCaseRef] = "InvalidUseCaseRef" <*> (x => x.num.show ++> x.title.show)
  implicit val fttMathTexTerm      : Show[MathTexTerm]       = "MathTexTerm" <*> (_.tex.show)
  implicit val freeTextTerm: Show[FreeTextTerm] = Show.show({
    case t: PlainText         => t.show
    case t: StepRef           => t.show
    case t: InvalidStepRef    => t.show
    case t@ DeletedRef        => t.show
    case t: UseCaseRef        => t.show
    case t: UseCaseSelfRef    => t.show
    case t: InvalidUseCaseRef => t.show
    case t: MathTexTerm       => t.show
  })

  implicit val freeText: Show[FreeText] = {
    val empty: Cord = "FreeText.empty"
    Show.show(x => if (x.isEmpty) empty else "FreeText" <> x.terms.show)
  }

  implicit val flowFromClause: Show[FlowFromClause] = "FlowFromClause" <*> (_.refs.show)

  implicit val flowToClause: Show[FlowToClause] = "FlowToClause" <*> (_.refs.show)

  implicit val stepText: Show[StepText] = {
    val empty: Cord = "StepText.empty"
    Show.show(x => if (x.isEmpty) empty else
      "StepText" <> x.mainClause.show ++> x.flowFromClause.show ++> x.flowToClause.show
    )
  }

  implicit val stepTree: Show[StepTree] = "StepTree" <*> (_.nodes.show)

  implicit val stepNode: Show[StepNode] = "StepNode" <*> (x =>
    x.id.show ++> x.level.show ++> x.labelIndex.show ++> x.children.show)

  implicit val sfv: Show[StepFieldValue] = "StepFieldValue" <*> (x => x.field.show ++> x.tree.show ++> x.textmap.show)

  def showFieldValue(ff: Field, v: Field#Value): Cord =
    ff match {
      case f: TextField            => f.castV(v).show
      case f: NormalCourseField    => f.castV(v).show
      case f: ExceptionCourseField => f.castV(v).show
      case f: FlowGraphField       => f.castV(v).show
    }

  // ===================================================================================================================
  // Use Case

  implicit val uciShow: Show[UseCaseIdent] = "UseCaseIdent" <*> (x => x.identId.show ++> x.number.show ++> x.projectId.show)

  implicit val uchShow: Show[UseCaseHeader] = "UseCaseHeader" <*> (_.title.show)

  implicit val ucShow: Show[UseCase] = "UseCase.as" <*> (x => {
    val fvTuples = x.fields.map(f => f.show ++ `~>` ++ showFieldValue(f, x.fieldValues(f)) ++ eol)
    x.number.show ++> x.header.show ++ eol ++> fvTuples.show ++> x.stepsAndLabels.show
  })

  implicit val ucrShow: Show[UseCaseRev] = "UseCaseRev" <*> (x =>
    x.ident.show ++> x.rev.show ++> x.id.show ++> x.header.show ++> x.createdAt.show)
}
