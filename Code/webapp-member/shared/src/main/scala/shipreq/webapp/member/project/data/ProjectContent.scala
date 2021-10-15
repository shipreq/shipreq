package shipreq.webapp.member.project.data

import cats.Eq
import japgolly.microlibs.cats_ext.CatsMacros
import monocle.Lens
import monocle.macros.Lenses
import shipreq.base.util.Util
import shipreq.webapp.member.project.text.{Atom, Text}
import shipreq.webapp.member.project.util.ShowSize

object ProjectContent {
  val genericReqs         : Lens[ProjectContent, GenericReqIMap           ] = reqs andThen Requirements.genericReqs andThen GenericReqs.imap
  val useCases            : Lens[ProjectContent, UseCases                 ] = reqs andThen Requirements.useCases
  val pubidRegister       : Lens[ProjectContent, PubidRegister            ] = reqs andThen Requirements.pubids
  val reqCodeTrie         : Lens[ProjectContent, ReqCode.Trie             ] = reqCodes andThen ReqCodes.trie
  val implicationsSrcToTgt: Lens[ProjectContent, Implications.Graph.UniDir] = implications andThen Implications.srcToTgt
  val useCaseIMap         : Lens[ProjectContent, UseCaseIMap              ] = useCases andThen UseCases.imap
  val useCaseStepIndex    : Lens[ProjectContent, UseCases.StepIndex       ] = useCases andThen UseCases.stepIndex // for equality

  val empty: ProjectContent =
    ProjectContent(
      Requirements.empty,
      ReqCodes.empty,
      ReqData.Text.empty,
      ReqData.emptyTags,
      Implications.empty,
      DeletionReasons.empty)

  import ReqData._ // for equality
  implicit lazy val equality: Eq[ProjectContent] = CatsMacros.deriveEq
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
