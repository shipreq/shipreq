package shipreq.webapp.client.project.app.pages.content.reqtable

import monocle.Optional
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.data.savedview.{Column => C, SortCriterion => SC}
import shipreq.webapp.base.text.PlainText
import shipreq.base.util.{Applicable, NotApplicable}
import shipreq.webapp.base.sort.{Sorter => SorterBase}
import SorterBase._

object Sorter {
  val Types = new WithTypes[Setup, Row]
  import Types._

  type TagOrder = DataLogic.TagOrder

  /**
   * Project data prepared in a way that various sorts will use.
   */
  final class Setup(val p: Project, plainText: PlainText.ForProject.NoCtx) {

    def normalisedText(f: PlainText.ForProject.NoCtx => String) =
      DataLogic.normaliseStringForSorting(f(plainText))

    val applicability: ProjectApplicability[Column, Row] =
      Row.applicability(p.config.applicability)

    @inline def reqTypesToMnemonicOrder =
      p.config.reqTypes.order
  }

  private def pubidNormaliser(setup: Setup): Pubid => (Int, Int) =
    setup.p.dataLogic.pubidSortKeyFn

  // CodeGroups are only displayed when sorting by code.
  // CodeGroups cannot have a blank code.
  // Therefore, CodeGroups cannot affect the conclusivity of a Pubid sort.
  val pubidSorter = sorter[(Int, Int)](
    prep =
      setup => {
        val n = pubidNormaliser(setup)
        val `n/a` = (-1, -1)
        ;{
          case r: Row.ForReq       => n(r.req.pubid)
          case _: Row.ForCodeGroup => `n/a`
        }
      },
    sort = SortFn.intPair
  )

  def pubidVectorSorter(loc: Optional[Row, Vector[Pubid]]): SorterForSMCB =
    SorterForSMCB(bp =>
      sorter[Vector[(Int, Int)]](
        rowMod = typicalRowModFn(loc, SortFn.intPair)(pubidNormaliser),
        prep =
          setup => {
            val n = pubidNormaliser(setup)
            row => loc.getOption(row).fold(Vector.empty[(Int, Int)])(_ map n)
          },
        sort = SortFn.intPairVector(bp)
    ))

  val reqTypeSorter = sorter[Int](
    prep = setup => {
      case r: Row.ForReq       => setup.reqTypesToMnemonicOrder(r.req.pubid.reqTypeId)
      case _: Row.ForCodeGroup => -1
    },
    sort = SortFn.int
  )

  def reqCodeSorter: SorterForSMCB =
    SorterForSMCB(SorterBase.reqCodeSorter(Row.reqCodesO, _))

  def tagSorter(loc: Optional[Row, Vector[ApplicableTagId]], order: Setup => TagOrder): SorterForSMCB =
    SorterForSMCB(bp =>
      sorter[Vector[Int]](
        rowMod = typicalRowModFn(loc, SortFn.int)(order(_).apply),
        prep   = setup => loc.getOption(_).fold(Vector.empty[Int])(_ map order(setup)),
        sort   = SortFn.intVector(bp)
    ))

  def textSorterS(c: Column, f: Setup => PlainText.ForProject.NoCtx => Row => String): SorterForSMCB =
    SorterForSMCB(bp =>
      sorter[String](
        prep = setup => {
          val g = f(setup)
          val rowApplicability = setup.applicability.byField(c)
          (row: Row) => rowApplicability(row) match {
            case Applicable    => setup.normalisedText(g(_)(row))
            case NotApplicable => ""
          }
        },
        sort = SortFn.string(bp)
      ))

  def textSorter(c: Column, f: PlainText.ForProject.NoCtx => Row => String): SorterForSMCB =
    textSorterS(c, _ => f)

  def customTextFieldSorter(id: CustomField.Text.Id, c: Column): SorterForSMCB =
    textSorter(c, p => {
      case r: Row.ForReq       => p.customTextFieldOption(id)(r.req) getOrElse ""
      case _: Row.ForCodeGroup => ""
    })

  val titleSorter: SorterForSMCB =
    textSorter(C.Title, p => {
      case r: Row.ForReq       => p.reqTitle(r.req)
      case r: Row.ForCodeGroup => p.codeGroupTitle(r.group)
    })

  def deletionReasonSorter: SorterForSMCB =
    textSorterS(C.DeletionReason, s => pt => {
      case r: Row.ForReq       => pt.deleteReasonForReq(r.req) getOrElse ""
      case _: Row.ForCodeGroup => pt.deleteReasonForCodeGroup getOrElse ""
    })

  // ===================================================================================================================
  // Sort criteria

  val inconclusiveIB: C.SortInconclusiveNoBlanks => SorterForSMIB = {
    case C.ReqType => SorterForSMIB(reqTypeSorter)
  }

  val inconclusiveCB: C.SortInconclusiveHasBlanks => SorterForSMCB = {
    case c: C.CustomField =>
      c.id match {
        case id: CustomField.Text       .Id => customTextFieldSorter(id, c)
        case id: CustomField.Tag        .Id => tagSorter(Row.cfTag(id), _.p.dataLogic.tagOrderByPos)
        case id: CustomField.Implication.Id => pubidVectorSorter(Row.cfImp(id))
      }
    case C.Title                            => titleSorter
    case C.Code                             => reqCodeSorter
    case C.OtherTags                        => tagSorter(Row.otherTags, _.p.dataLogic.tagOrderByName)
    case C.AllTags                          => tagSorter(Row.allTags, _.p.dataLogic.tagOrderByName)
    case C.Implications(dir)                => pubidVectorSorter(Row.implications(dir))
    case C.DeletionReason                   => deletionReasonSorter
  }

  val inconclusive: SC.Inconclusive => Sorter = {
    case sc: SC.InconclusiveCB => inconclusiveCB(sc.column)(sc.method)
    case sc: SC.InconclusiveIB => inconclusiveIB(sc.column)(sc.method)
  }

  def conclusive(sc: SC.Conclusive): Sorter = {
    val r: SorterForSMIB = sc.column match {
      case C.Pubid => SorterForSMIB(pubidSorter)
    }
    r(sc.method)
  }

  def orderingForAllTags  (d: DataLogic): Ordering[ApplicableTagId] = d.tagOrderingByName
  def orderingForOtherTags(d: DataLogic): Ordering[ApplicableTagId] = d.tagOrderingByName
  def orderingForTagField (d: DataLogic): Ordering[ApplicableTagId] = d.tagOrderingByPos
  def orderingForImpField (d: DataLogic): Ordering[Pubid          ] = d.pubidOrdering
}