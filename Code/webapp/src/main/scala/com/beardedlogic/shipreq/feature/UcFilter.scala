package shipreq.webapp.feature

import net.liftweb.common.Full
import net.liftweb.http.S
import net.liftweb.json._
import scala.xml.{Text, NodeSeq}
import scalaz.{MonadPlus, NonEmptyList}

import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Types._
import shipreq.webapp.db.BasicUseCaseInfo
import shipreq.webapp.lib.SnippetHelpers.shouldNeverHappen_swallowInProd

/**
 * The filtering of a project's use cases.
 */
sealed trait UcFilter

object UcFilters {

  /**
   * No filter. All use cases remain in scope.
   */
  case object All extends UcFilter {
    val json: Json[UcFilter] = UcFilter.toJson(this)
  }

  /**
   * Only specified use cases are allowed.
   * To all other use cases: "Sorry mate, not with those shoes. It's not your night."
   */
  case class Whitelist(ids: List[UseCaseIdentId]) extends UcFilter
}

// =====================================================================================================================

object UcFilter {
  import UcFilters._

  type UseCases = List[BasicUseCaseInfo]

  def init(ucs: UseCases, selected: UcFilter): NonEmptyList[UcFilter] =
    selected match {
      case All          => NonEmptyList(All, Whitelist(ucs.map(_.identId)))
      case s: Whitelist => NonEmptyList(All, s)
    }

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

  def apply[C[_] : MonadPlus, U <: BasicUseCaseInfo](f: UcFilter)(ucs: C[U]): C[U] = f match {
    case All            => ucs
    case Whitelist(ids) => ucs.filter(uc => ids.contains(uc.identId))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Rendering

  private[this] object Rendering {
    val optionParamName = "ucfilter"
    val idAll = "a"
    val idWhitelist = "w"

    def Z = NodeSeq.Empty
    val someChecked = Some(Text("checked"))
    val preRenderedAllSel = renderFieldAll(true)
    val preRenderedAllNotSel = renderFieldAll(false)

    def renderAllOptions(fs: NonEmptyList[UcFilter], selected: UcFilter, ucs: UseCases): NodeSeq =
      <div class="ucfilter-group">
        {fs.foldMap(f => ucFilterOption(f, f eq selected, ucs))}
      </div>

    def renderFieldAll(selected: Boolean) = wrap("all", selected)(radio(All, selected, "All use cases"))

    def ucFilterOption(f: UcFilter, selected: Boolean, ucs: UseCases): NodeSeq = f match {
      case All =>
        if (selected) preRenderedAllSel else preRenderedAllNotSel

      case Whitelist(ids) =>
        wrap("wl", selected)(radio(f, selected, "Only selected use cases"), ucCheckboxes(ucs, ids))
    }

    def checkedAttr(enabled: Boolean): Option[Text] =
      if (enabled) someChecked else None

    def idstr(f: UcFilter): String = f match {
      case All          => idAll
      case Whitelist(_) => idWhitelist
    }

    def wrap(className: String, selected: Boolean)(content: NodeSeq, subContent: NodeSeq = Z): NodeSeq = {
      val wrappedSubContent = (
        if (subContent.isEmpty)
          Z
        else if (selected)
          <div class="sub">{subContent}</div>
        else
          <div class="sub" style="display:none">{subContent}</div>
      )
      val className2 = "ucfilter " + className
      <div class={className2}>{content}{wrappedSubContent}</div>
    }

    def radio(f: UcFilter, selected: Boolean, desc: String): NodeSeq =
      <div class="radio"><label>
        <input type="radio" name={optionParamName} class="ucfilter" value={idstr(f)} checked={checkedAttr(selected)} /> {desc}
      </label></div>

    def ucCheckboxes(ucs: UseCases, selectedIds: List[UseCaseIdentId]): NodeSeq =
      if (ucs.isEmpty)
        Z
      else
        <ol class="ucs">{ucs foldMap ucCheckbox(selectedIds, idWhitelist)}</ol>

    def ucCheckbox(selectedIds: List[UseCaseIdentId], namePrefix: String)(uc: BasicUseCaseInfo): NodeSeq = {
      val selected = selectedIds.contains(uc.identId)
      val name = ucParamName(namePrefix, uc)
      <li class="checkbox"><label><input type="checkbox" name={name} value="1" checked={checkedAttr(selected)}/> {uc.fullName}</label></li>
    }

    def ucParamName(prefix: String, id: UseCaseIdentId) = s"$prefix-$id"

    def parseRequest(ucs: UseCases): UcFilter = {
      S.param(optionParamName) match {
        case Full(`idAll`) =>
          All

        case Full(`idWhitelist`) =>
          val ids = ucs.map(_.identId).filter(id => S.param(ucParamName(idWhitelist, id)).isDefined)
          Whitelist(ids)

        case p =>
          shouldNeverHappen_swallowInProd(All)(s"Invalid UC-filter param: $p")
      }
    }
  }

  def render(selected: UcFilter, ucs: UseCases): (NodeSeq, () => UcFilter) =
    render(init(ucs, selected), selected, ucs)

  def render(fs: NonEmptyList[UcFilter], selected: UcFilter, ucs: UseCases): (NodeSeq, () => UcFilter) = {
    val r = Rendering.renderAllOptions(fs, selected, ucs)
    val f = () => Rendering.parseRequest(ucs)
    (r, f)
  }
}