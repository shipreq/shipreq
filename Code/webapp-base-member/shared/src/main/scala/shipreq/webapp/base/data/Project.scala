package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import monocle.{Lens, Optional}
import monocle.macros.Lenses
import monocle.std.option.pSome
import scalaz.Equal
import scalaz.std.anyVal.intInstance
import shipreq.base.util._
import shipreq.base.util.univeq._
import DataImplicits._

object Project {
  type Name = String

  val customIssueTypes    : Lens[Project, CustomIssueTypeIMap  ] = config  ^|-> ProjectConfig.customIssueTypes
  val reqTypes            : Lens[Project, ReqTypes             ] = config  ^|-> ProjectConfig.reqTypes
  val fields              : Lens[Project, FieldSet             ] = config  ^|-> ProjectConfig.fields
  val tagTree             : Lens[Project, TagTree              ] = config  ^|-> ProjectConfig.tags ^|-> Tags.tree
  val customFields        : Lens[Project, FieldSet.CustomFields] = fields  ^|-> FieldSet.customFields
  val reqs                : Lens[Project, Requirements         ] = content ^|-> ProjectContent.reqs
  val reqCodes            : Lens[Project, ReqCodes             ] = content ^|-> ProjectContent.reqCodes
  val reqText             : Lens[Project, ReqData.Text         ] = content ^|-> ProjectContent.reqText
  val reqTags             : Lens[Project, ReqData.Tags         ] = content ^|-> ProjectContent.reqTags
  val implications        : Lens[Project, Implications.BiDir   ] = content ^|-> ProjectContent.implications
  val deletionReasons     : Lens[Project, DeletionReasons      ] = content ^|-> ProjectContent.deletionReasons
  val genericReqs         : Lens[Project, GenericReqIMap       ] = content ^|-> ProjectContent.genericReqs
  val useCases            : Lens[Project, UseCases             ] = content ^|-> ProjectContent.useCases
  val pubidRegister       : Lens[Project, PubidRegister        ] = content ^|-> ProjectContent.pubidRegister
  val reqCodeTrie         : Lens[Project, ReqCode.Trie         ] = content ^|-> ProjectContent.reqCodeTrie
  val implicationsSrcToTgt: Lens[Project, Implications.UniDir  ] = content ^|-> ProjectContent.implicationsSrcToTgt
  val useCaseIMap         : Lens[Project, UseCaseIMap          ] = content ^|-> ProjectContent.useCaseIMap
  val useCaseStepIndex    : Lens[Project, UseCases.StepIndex   ] = content ^|-> ProjectContent.useCaseStepIndex

  val reqtableViewsNE: Optional[Project, reqtable.SavedViews.NonEmpty] =
    reqtableViews ^<-? pSome

  def reqtableView(id: reqtable.SavedView.Id): Optional[Project, reqtable.SavedView] =
    reqtableViewsNE ^|-? reqtable.SavedViews.NonEmpty.at(id)

  implicit lazy val equality: Equal[Project] = ScalazMacros.deriveEqual

  // Not allowed by validator.
  // This ensures that initial ProjectNameSet events (generated on project creation) apply instead of being discarded
  // as NO-OPs due to the hashcodes being unchanged before and after.
  final val emptyProjectName: Name =
    ""

  val empty: Project =
    Project(
      emptyProjectName,
      ProjectConfig.empty,
      ProjectContent.empty,
      reqtable.SavedViews.empty,
      IdCeilings.zero)
}

@Lenses
final case class Project(name         : Project.Name,
                         config       : ProjectConfig,
                         content      : ProjectContent,
                         reqtableViews: reqtable.SavedViews.Optional,
                         idCeilings   : IdCeilings) {

  override def toString =
    s"Project($idCeilings)"
    //ShowSize(this).showTree

  lazy val deadReqIds: Set[ReqId] =
    content.reqs.reqIterator.filter(_.live(config.reqTypes) is Dead).map(_.id).toSet

  lazy val deadReqCount: Int =
    deadReqIds.size

  lazy val reqTypeCount: LiveDeadStatMap[ReqTypeId, Int] = {
    val b = new LiveDeadStatMap.Builder[ReqTypeId, Int]
    for (r <- content.reqs.reqIterator) {
      val live = r.live(config.reqTypes)
      b(r.reqTypeId).mod(live)(_ + 1)
    }
    b.result()
  }

  lazy val atomScan = AtomScan(this)

  /**
   * Transitive closure of implications going source → target.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationSrcToTgtTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(Forwards)

  /**
   * Transitive closure of implications going target → source.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationTgtToSrcTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(Backwards)

  private def implicationTransitiveClosure(dir: Direction): TransitiveClosure[ReqId] =
    content.implications.transitiveClosure(
      dir,
      content.reqs.idIterator,
      TransitiveClosure.Filter terminalSet deadReqIds)

  def liveReqIterator(): Iterator[Req] =
    content.reqs.reqIterator.filter(_.live(config.reqTypes) is Live)

  def reqtableViewIterator: Iterator[reqtable.SavedView] =
    reqtableViews.fold[Iterator[reqtable.SavedView]](Iterator.empty)(_.iterator)

  def prettyPrintImplicationGraph: String =
    Util.quickJSB { sb =>

      val indentFn: Int => String =
        Memo(". " * _)

      val _fmt: ReqId => String =
        id => {
          var a = id.value.toString
          val r = content.reqs.need(id)
          if (r.live(config.reqTypes) is Dead) {
            a += (if (r.liveExplicitly is Dead) "!" else "-")
            if (r.allowLiveChange(config.reqTypes) is Deny) a += "!"
          }
          a
        }

      val fmt = Memo(_fmt)

      var first = true

      def go(_ids: TraversableOnce[ReqId], indent: Int): Unit = {
        val indentStr = indentFn(indent)
        MutableArray(_ids).map(id => (id, fmt(id))).sortBy(_._2).array.foreach { case (id, show) =>
          if (first) first = false else sb append '\n'
          sb append indentStr
          sb append show
          go(content.implications.forwards(id), indent + 1)
        }
      }
      go(content.reqs.idIterator.filter(content.implications.backwards(_).isEmpty), 0)
    }
}
