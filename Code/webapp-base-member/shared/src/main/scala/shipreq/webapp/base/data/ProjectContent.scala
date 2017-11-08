package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import monocle.Lens
import monocle.macros.Lenses
import scalaz.Equal
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.{Atom, Text}
import shipreq.webapp.base.util.ShowSize

object ProjectContent {
  val genericReqs         : Lens[ProjectContent, GenericReqIMap     ] = reqs ^|-> Requirements.genericReqs
  val useCases            : Lens[ProjectContent, UseCases           ] = reqs ^|-> Requirements.useCases
  val pubidRegister       : Lens[ProjectContent, PubidRegister      ] = reqs ^|-> Requirements.pubids
  val reqCodeTrie         : Lens[ProjectContent, ReqCode.Trie       ] = reqCodes ^|-> ReqCodes.trie
  val implicationsSrcToTgt: Lens[ProjectContent, Implications.UniDir] = implications ^<-> Implications.biToUni
  val useCaseIMap         : Lens[ProjectContent, UseCaseIMap        ] = useCases ^|-> UseCases.imap
  val useCaseStepIndex    : Lens[ProjectContent, UseCases.StepIndex ] = useCases ^|-> UseCases.stepIndex // for equality

  val empty: ProjectContent =
    ProjectContent(
      Requirements.empty,
      ReqCodes.empty,
      ReqData.emptyText,
      ReqData.emptyTags,
      Implications.emptyBiDir,
      DeletionReasons.empty)

  import ReqData._ // for equality
  implicit lazy val equality: Equal[ProjectContent] = ScalazMacros.deriveEqual
}

@Lenses
final case class ProjectContent(reqs           : Requirements,
                                reqCodes       : ReqCodes,
                                reqText        : ReqData.Text,
                                reqTags        : ReqData.Tags,
                                implications   : Implications.BiDir,
                                deletionReasons: DeletionReasons) {

  /** Dead or alive */
  def allRichText: List[(String, Iterator[Text.AnyOptional])] =
    ("Deletion reasons",  deletionReasons.reasons.iterator.map(_.whole))                 ::
    ("CodeGroups",        reqCodes.groups.iterator.map(_.title))                         ::
    ("GenericReq titles", reqs.genericReqs.valuesIterator.map(_.title))                  ::
    ("UseCase titles",    reqs.useCases.imap.valuesIterator.map(_.title))                ::
    ("UseCase steps",     reqs.useCases.stepIterator.map(_.titleExplicitly))             ::
    ("Text fields",       reqText.valuesIterator.flatMap(_.valuesIterator).map(_.whole)) ::
    Nil

  def countTextAtoms: ShowSize.Node = {
    val counted =
      allRichText.map { case (name, txts) =>
        ShowSize.Node.countChildren(name, txts.flatten.toList)(Atom.Type.of(_).toString) }
    ShowSize.Node.sum("TextAtoms", counted: _*)
  }

}
