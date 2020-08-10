package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import nyaya.util.Multimap
import scala.collection.mutable
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText

/** The resulting tags for all requirements in a project, after automation has been applied.
  *
  * Automation is currently:
  *   - field defaults
  *   - derivative tags (MF-34)
  *
  * This is basically an API to access project tags as seen from the user's point of view.
  */
sealed trait VirtualProjectTags {
  import VirtualProjectTags._

  def apply(reqId: ReqId): ResultsMono

  def apply(reqId: ReqId, filterDead: FilterDead): ResultsLiveDead

  def underTagGroup(tagGroupId: TagGroupId, filterDead: FilterDead): ReqId => Vector[ApplicableTagId]

  def withTagFieldDistFn(distFn: FilterDead => TagFieldDistribution.TagIds): VirtualProjectTags

  final def withTagFieldDist(dist: TagFieldDistribution.TagIds): VirtualProjectTags =
    withTagFieldDistFn(_ => dist)

  def prettyPrint: String

  @elidable(elidable.INFO)
  final override def toString = prettyPrint
}

object VirtualProjectTags {

  def apply(p: Project): VirtualProjectTags =
    new Mutable(p).result

  /** Results that aren't indexed by anything. */
  sealed trait ResultsMono {
    def manualLiveValues: Multimap[ApplicableTagId, List, LocationOf.Tag.InReq]
    def naTagsInLiveText: Multimap[ApplicableTagId, List, Location.Text]

    def defaults: Map[CustomField.Tag.Id, ApplicableTagId]
    final def usesDefaults = defaults.nonEmpty
  }

  /** Results indexed by FilterDead. */
  sealed trait ResultsLiveDead {
    val fieldSet: CustomField.Tag.Id => Set[ApplicableTagId]
    def allSet  : Set[ApplicableTagId]
    def otherSet: Set[ApplicableTagId]

    val fieldOrdered: CustomField.Tag.Id => Vector[ApplicableTagId]
    def allOrdered  : Vector[ApplicableTagId]
    def otherOrdered: Vector[ApplicableTagId]

    def withTagFieldDist(dist: TagFieldDistribution.TagIds): ResultsLiveDead

    def isEmpty: Boolean
    final def nonEmpty = !isEmpty
  }

  // ===================================================================================================================

  private object Mutable {

    object ForReq {
      val emptyInReq  = Multimap.empty[ApplicableTagId, List, LocationOf.Tag.InReq]
      val emptyInText = Multimap.empty[ApplicableTagId, List, Location.Text]
    }

    final class ForReq(nonApplicableTags: Set[ApplicableTagId], tagLive: ApplicableTagId => Live) {
      var manualLive         = ForReq.emptyInReq
      var manualDead         = ForReq.emptyInReq
      var deadTagsInLiveText = ForReq.emptyInText
      var naTagsInLiveText   = ForReq.emptyInText
      var defaults           = Map.empty[CustomField.Tag.Id, ApplicableTagId]

      def addManual(id     : ApplicableTagId,
                    loc    : LocationOf.Tag.InReq,
                    ctx    : Live,
                    allowNA: Boolean = false): Unit =
        if (allowNA || !nonApplicableTags.contains(id))
          ctx & tagLive(id) match {
            case Live => manualLive = manualLive.add(id, loc)
            case Dead => manualDead = manualDead.add(id, loc)
          }

      def addTagInLiveText(id: ApplicableTagId, loc: Location.Text): Unit =
        if (tagLive(id) is Dead)
          deadTagsInLiveText = deadTagsInLiveText.add(id, loc)
        else if (nonApplicableTags.contains(id))
          naTagsInLiveText = naTagsInLiveText.add(id, loc)
        else
          manualLive = manualLive.add(id, loc)

      def addDefault(f: CustomField.Tag.Id, id: ApplicableTagId): Unit =
        defaults = defaults.updated(f, id)
    }
  }

  private final class Mutable(p: Project) {
    import Mutable._

    val deadTags    = p.config.tags.deadApplicableTagIds
    val tagsInText  = p.atomScan.tagRefs
    val tagLive     = (id: ApplicableTagId) => Dead when deadTags.contains(id)
    val reqTags     = p.content.reqTags
    val reqTypes    = p.config.reqTypes
    val fieldRules  = p.config.fieldRules(ShowDead) // ShowDead so that it doesn't replace dead defaults with Optional
    val liveTagDist = p.config.liveTagFieldDistribution

    // -----------------------------------------------------------------------------------------------------------------
    def collectManualTags(req: Req, b: ForReq): Unit = {

      // Scan live text
      {
        val liveTextScanner: LocAndValue[LocationOf.Tag.InReq, ApplicableTagId] => Unit =
          req.live(reqTypes) match {
            case Live =>
              t => t.loc match {
                case loc: Location.Text => b.addTagInLiveText(t.value, loc)
                case _                  => b.addManual(t.value, t.loc, ctx = Live)
              }
            case Dead =>
              t => b.addManual(t.value, t.loc, ctx = Dead)
          }

        for (t <- tagsInText(req.id).live)
          liveTextScanner(t)
      }

      // Scan dead text
      for (t <- tagsInText(req.id).dead)
        b.addManual(t.value, t.loc, ctx = Dead, allowNA = true)

      // Scan tag fields
      for (t <- reqTags(req.id))
        b.addManual(t, Location.Tags, ctx = Live)
    }

    // -----------------------------------------------------------------------------------------------------------------
    def addDefaults(req: Req, b: ForReq): Unit = {
      import FieldReqTypeRules.Resolution

      lazy val effectiveTags: Set[ApplicableTagId] =
        b.manualLive.keySet ++ b.deadTagsInLiveText.keySet ++ b.naTagsInLiveText.keySet

      for (f <- p.config.liveCustomTagFields) {
        fieldRules(req.reqTypeId).tag(f.id) match {

          case Resolution.DefaultTo(default) =>
            val relevant = liveTagDist.inField(f.id)
            if (!effectiveTags.exists(relevant.contains))
              b.addDefault(f.id, default)

          case Resolution.Mandatory
             | Resolution.Optional
             | Resolution.NotApplicable =>
        }
      }
    }

    // -----------------------------------------------------------------------------------------------------------------
    val result: VirtualProjectTags = {
      val data = mutable.HashMap.empty[ReqId, ForReq]

      // Step 1: Scan all requirements
      for (req <- p.content.reqs.reqIterator()) {
        val nonApplicableTags = p.config.naTags(req.reqTypeId).set
        val b = new ForReq(nonApplicableTags, tagLive)
        data.update(req.id, b)

        // Process current req
        collectManualTags(req, b)
        addDefaults(req, b)
      }

      val monoResults: ReqId => ResultsMono =
        Memo { reqId =>
          val b = data(reqId)
          new ResultsMono {
            override def manualLiveValues = b.manualLive
            override def naTagsInLiveText = b.naTagsInLiveText
            override def defaults         = b.defaults
          }
        }

      val tagOrderByName = p.config.tags.orderByName
      val tagOrderByPos  = p.config.tags.orderByPos

      val allSetMemo: FilterDead => ReqId => Set[ApplicableTagId] = {
        var live: ReqId => Set[ApplicableTagId] = null
        val x: FilterDead => ReqId => Set[ApplicableTagId] =
          FilterDead.memo {
            case HideDead =>
              Memo { reqId =>
                val b = data(reqId)
                var results = b.manualLive.keySet
                results ++= b.deadTagsInLiveText.keys
                results ++= b.naTagsInLiveText.keys
                results ++= b.defaults.values
                results
              }
            case ShowDead =>
              Memo { reqId =>
                val b = data(reqId)
                var results = live(reqId)
                results ++= b.manualDead.keys
                results
              }
          }
        live = x(HideDead)
        x
      }

      def allLiveDeadResults(distFn: FilterDead => TagFieldDistribution.TagIds): FilterDead => ReqId => ResultsLiveDead = {
        FilterDead.memoLazy { fd =>
          val allSetFD = allSetMemo(fd)
          val defaultDist = distFn(fd)
          Memo { reqId =>
            def liveDeadResults(dist: TagFieldDistribution.TagIds): ResultsLiveDead =
              new ResultsLiveDead {

                override def isEmpty =
                  allSet.isEmpty

                override def allSet =
                  allSetFD(reqId)

                override lazy val otherSet = {
                  val tagsUsedInFields = dist.usedInFields
                  allSet &~ tagsUsedInFields
                }

                override val fieldSet = Memo { fid =>
                  val legal = dist inField fid
                  allSet & legal
                }

                override lazy val allOrdered =
                  MutableArray(allSet).sortBy(tagOrderByName.apply).to(Vector)

                override def otherOrdered =
                  MutableArray(otherSet).sortBy(tagOrderByName.apply).to(Vector)

                override val fieldOrdered = Memo { fid =>
                  MutableArray(fieldSet(fid)).sortBy(tagOrderByPos.apply).to(Vector)
                }

                override def withTagFieldDist(dist2: TagFieldDistribution.TagIds) =
                  if (dist eq dist2)
                    this
                  else
                    liveDeadResults(dist2)
              }

            liveDeadResults(defaultDist)
          }
        }
      }

      def allResults(distFn: FilterDead => TagFieldDistribution.TagIds): VirtualProjectTags = {
        val fdResults = allLiveDeadResults(distFn)

        new VirtualProjectTags {
          override def apply(reqId: ReqId): ResultsMono =
            monoResults(reqId)

          override def apply(reqId: ReqId, filterDead: FilterDead): ResultsLiveDead =
            fdResults(filterDead)(reqId)

          override def underTagGroup(tagGroupId: TagGroupId, filterDead: FilterDead): ReqId => Vector[ApplicableTagId] = {
            val tagLookup = fdResults(filterDead)
            val tagScope  = p.config.tagFieldDistribution(filterDead).inTagGroup(tagGroupId)
            val tagOrder  = p.config.tags.applicableTagOrdering(tagGroupId, filterDead)

            reqId =>
              MutableArray(tagLookup(reqId).allSet.iterator.filter(tagScope.contains))
                .sort(tagOrder)
                .iterator()
                .toVector
          }

          override def withTagFieldDistFn(distFn2: FilterDead => TagFieldDistribution.TagIds) =
            allResults(distFn2)

          @elidable(elidable.INFO)
          override def prettyPrint: String = {
            val pt = PlainText.ForProject.noCtx(p)
            val sep = "=" * 80

            def show(id: ApplicableTagId): String = {
              val tag = p.config.tags.needApplicableTag(id)
              tag.live match {
                case Live => tag.name
                case Dead => s"${tag.name} (DEAD)"
              }
            }

            def showM[A](i: Multimap[ApplicableTagId, List, A]): Iterator[String] =
              i.iterator.map { case (tagId, as) =>
                s"${show(tagId)} found in ${MutableArray(as.map(_.toString)).sort.mkString("[", ", ", "]")}"
              }

            def showD(i: Map[CustomField.Tag.Id, ApplicableTagId]): Iterator[String] =
              i.iterator.map { case (fieldId, tagId) =>
                val field = p.config.fields.custom(fieldId)
                val group = p.config.tags.needTagGroup(field.tagId).name
                val s = s"${show(tagId)} from $group field (#${fieldId.value})"
                field.live(p.config) match {
                  case Live => s
                  case Dead => s + " (DEAD)"
                }
              }

            p.content.reqs.reqIterator().map { req =>
              val b = data(req.id)
              val title = s"${PlainText.pubid(req.pubid, p)}: ${pt.reqTitle(req)}"
              var items = Vector.empty[String]
              items ++= showM(b.manualLive        ).map("manualLive         : " + _)
              items ++= showM(b.manualDead        ).map("manualDead         : " + _)
              items ++= showM(b.deadTagsInLiveText).map("deadTagsInLiveText : " + _)
              items ++= showM(b.naTagsInLiveText  ).map("naTagsInLiveText   : " + _)
              items ++= showD(b.defaults          ).map("defaults           : " + _)
              val detail = items.sorted.map("  - " + _).mkString("\n")
              if (detail.isEmpty)
                title
              else
                s"$title\n$detail"
            }
              .toVector
              .sorted
              .mkString(sep + "\n", "\n", "\n" + sep)
          }
        }
      }

      allResults(p.dataLogic.tagFieldDist)
    }
  }
}
