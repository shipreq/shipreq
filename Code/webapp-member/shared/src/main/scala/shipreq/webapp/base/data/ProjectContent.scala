package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import monocle.Lens
import monocle.macros.Lenses
import scalaz.Equal
import shipreq.base.util.Util
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.ShowSize

object ProjectContent {
  val genericReqs         : Lens[ProjectContent, GenericReqIMap           ] = reqs ^|-> Requirements.genericReqs ^|-> GenericReqs.imap
  val useCases            : Lens[ProjectContent, UseCases                 ] = reqs ^|-> Requirements.useCases
  val pubidRegister       : Lens[ProjectContent, PubidRegister            ] = reqs ^|-> Requirements.pubids
  val reqCodeTrie         : Lens[ProjectContent, ReqCode.Trie             ] = reqCodes ^|-> ReqCodes.trie
  val implicationsSrcToTgt: Lens[ProjectContent, Implications.Graph.UniDir] = implications ^|-> Implications.srcToTgt
  val useCaseIMap         : Lens[ProjectContent, UseCaseIMap              ] = useCases ^|-> UseCases.imap
  val useCaseStepIndex    : Lens[ProjectContent, UseCases.StepIndex       ] = useCases ^|-> UseCases.stepIndex // for equality

  val empty: ProjectContent =
    ProjectContent(
      Requirements.empty,
      ReqCodes.empty,
      ReqData.Text.empty,
      ReqData.emptyTags,
      Implications.empty,
      DeletionReasons.empty)

  import ReqData._ // for equality
  implicit lazy val equality: Equal[ProjectContent] = ScalazMacros.deriveEqual
}

/** @param reqTags Directly applied tags. This includes all tag columns, but not tags in text.
  *                Use Project#dataLogic to consider tags from all sources.
  */
@Lenses
final case class ProjectContent(reqs           : Requirements,
                                reqCodes       : ReqCodes,
                                reqText        : ReqData.Text,
                                reqTags        : ReqData.Tags,
                                implications   : Implications,
                                deletionReasons: DeletionReasons) {

  /** ReqCodes referenced in anything anywhere (including text in dead custom-text fields). */
  lazy val codeRefs: Set[ReqCodeId] =
    Util.mergeSets(
      reqs.genericReqs.localCodeRefs,
      reqs.useCases.localCodeRefs,
      reqText.localCodeRefs,
      reqCodes.scan.localCodeRefs,
    )

  lazy val useCaseStepRefs: Set[UseCaseStepId] =
    Util.mergeSets(
      reqs.genericReqs.localUseCaseStepRefs,
      reqs.useCases.localUseCaseStepRefs,
      reqText.localUseCaseStepRefs,
      reqCodes.scan.localUseCaseStepRefs,
    )

  /** Dead or alive */
  def allRichText: List[(String, Iterator[Text.AnyOptional])] =
    ("Deletion reasons",  deletionReasons.reasons.iterator.map(_.whole))                 ::
    ("CodeGroups",        reqCodes.groups.iterator.map(_.title))                         ::
    ("GenericReq titles", reqs.genericReqs.imap.valuesIterator.map(_.title))             ::
    ("UseCase titles",    reqs.useCases.imap.valuesIterator.map(_.title))                ::
    ("UseCase steps",     reqs.useCases.stepIterator.map(_.titleExplicitly))             ::
    ("Text fields",       reqText.data.valuesIterator.flatMap(_.valuesIterator).map(_.whole)) ::
    Nil

  def countTextAtoms: ShowSize.Node = {
    val counted =
      allRichText.map { case (name, txts) =>
        ShowSize.Node.countChildren(name, txts.flatten.toList)(Atom.Type.of(_).toString) }
    ShowSize.Node.sum("TextAtoms", counted: _*)
  }

  def reqTextFor(id: CustomField.Text.Id): Map[ReqId, Text.CustomTextField.NonEmptyText] =
    reqText.data.getOrElse(id, Map.empty)
}
