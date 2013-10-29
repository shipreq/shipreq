package com.beardedlogic.usecase.feature

import scala.xml.NodeSeq
import scalaz.NonEmptyList
import net.liftweb.json._
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.db.{DaoS, UseCaseRev}

/**
 * The filtering of a project's use cases.
 */
sealed trait UcFilter

/**
 * No filter. All use cases remain in scope.
 */
case object All extends UcFilter

/**
 * Only specified use cases are allowed.
 * To all other use cases: "Sorry mate, not with those shoes. It's not your night."
 */
case class Whitelist(ids: List[UseCaseIdentId]) extends UcFilter

// =====================================================================================================================

object UcFilter {

  type UseCases = List[UseCaseRev]

  def init(ucs: UseCases): NonEmptyList[UcFilter] = NonEmptyList(
    All,
    Whitelist(ucs.map(_.identId))
  )

  // -------------------------------------------------------------------------------------------------------------------
  // JSON

  private[this] object FilterJson {
    val typeKey = "$"
    val typeAll = "all"
    val typeWhitelist = "wl"
    val whitelistIds = "ids"

    import net.liftweb.json.JsonDSL._

    def build(typeName: String, data: List[JField]): JObject =
      JObject(JField(typeKey, JString(typeName)) :: data)

    def serialise(ff: UcFilter): JObject = ff match {
      case All => build(typeAll, Nil)
      case Whitelist(ids) => build(typeWhitelist, JField(whitelistIds, Extraction.decompose(ids)) :: Nil)
    }

    def deserialise(typeName: String, data: List[JField]): UcFilter = typeName match {
      case `typeAll` => All
      case `typeWhitelist` =>
        val ids = (data \ whitelistIds).extract[List[UseCaseIdentId]]
        Whitelist(ids)
    }

    object UseCaseIdentIdSerialiser extends CustomSerializer[UseCaseIdentId](format => ( {
      case JInt(l) => l.longValue.tag[UseCaseIdentId]
    }, {
      case l: UseCaseIdentId => JInt(l.longValue)
    }))

    object Serialiser extends CustomSerializer[UcFilter](format => ( {
      case JObject(JField(typeKey, JString(name)) :: data) => deserialise(name, data)
    }, {
      case f: UcFilter => serialise(f)
    }))

    implicit val jsonFormats = Serialization.formats(NoTypeHints) + Serialiser + UseCaseIdentIdSerialiser
  }

  implicit def jsonFormats = FilterJson.jsonFormats

  def toJson[F <: UcFilter](f: F): Json[F] =
    Serialization.write(f).tag[IsJsonFor[F]]

  def fromJson(json: Json[UcFilter]): UcFilter =
    Serialization.read[UcFilter](json)

  // -------------------------------------------------------------------------------------------------------------------
  // Application

  def apply(f: UcFilter, ucs: UseCases): UseCases = f match {
    case All            => ucs
    case Whitelist(ids) => ucs.filter(uc => ids.contains(uc.identId))
  }

  def loadApply(f: UcFilter, dao: DaoS, projectId: ProjectId): UseCases = f match {
    case All            => dao.findAllLatestUseCaseRevsByProject(projectId)
    case Whitelist(ids) => dao.findAllLatestUseCaseRevs(projectId, ids)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Rendering

  def render[F <: UcFilter](f: F, selected: Boolean, ucs: UseCases): Option[(NodeSeq, () => F)] = ???
}