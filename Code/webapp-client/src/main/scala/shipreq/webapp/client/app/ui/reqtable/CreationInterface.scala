package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.equal.ToEqualOps
import shipreq.base.util.UnivEq.univEqOption
import shipreq.base.util.{Must, NonEmptyVector, UnivEq}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.CreateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.app.ui.SelectOne.{Choice, Choices}
import shipreq.webapp.client.app.ui.reqtable.edit.{ImplicationEditor, TagEditor, ReqCodeEditor, RichTextEditor}
import shipreq.webapp.client.app.ui.{ProjectWidgets, SelectOne, VUCA}
import shipreq.webapp.client.lib.TIO
import shipreq.webapp.client.util.Enabled

object CreationInterface {
  sealed trait Type
  case object ReqCodeGroupType extends Type
  case class GenericReqType(rt: CustomReqTypeId) extends Type
  implicit def typeEquality: UnivEq[Type] = UnivEq.force

  type SelType = Option[Type]
  val selectComponent = SelectOne.Component[SelType]

  case class Props(createIO: (CreateContentCmd, TIO.Success, String => TIO.Failure) => IO[Unit], state: State)

  @Lenses
  case class State(selectedType: SelType,
                   rcg         : CreateReqCodeGroupState,
                   greq        : CreateGenericReqState)

  sealed trait Status
  case object Editing extends Status
  case object Locked extends Status
  case class Failed(reason: String) extends Status
  implicit def statusEquality: UnivEq[Status] = UnivEq.force

  @Lenses
  case class CreateReqCodeGroupState(status: Status, reqCode: String, title: String)

  @Lenses
  case class CreateGenericReqState(status: Status, reqCodes: String, title: String, tags: String, imp: String)

  implicit val reusabilityState: Reusability[State] = Reusability.byRef

  def initState: State =
    State(None,
      CreateReqCodeGroupState(Editing, "", ""),
      CreateGenericReqState(Editing, "", "", "", ""))
}

// =====================================================================================================================
import shipreq.webapp.client.app.ui.reqtable.CreationInterface._

class CreationInterface($             : CompStateFocus[State],
                        project       : Px[Project],
                        projectText   : Px[PlainText.ForProject],
                        projectWidgets: Px[ProjectWidgets],
                        textSearch    : Px[TextSearch]) {

  val types: Px[Choices[SelType]] =
    project.map { p =>
      val blank = Choice[SelType](None, "", Enabled)
      val rcg   = Choice[SelType](Some(ReqCodeGroupType), UiText.reqCodeGroup, Enabled)
      val rts   = p.config.liveCustomReqTypes
                    .map(rt => Choice[SelType](Some(GenericReqType(rt.id)), rt.fullName, Enabled))
                    .sortBy(_.label)
      (NonEmptyVector(blank) :+ rcg) ++ rts
    }

  val Component = ReactComponentB[Props]("Creation")
    .stateless
    .render($ => render($.props))
    //    .configure(shouldComponentUpdate) TODO
    .build

  private def render(p: Props) = {
    val s = p.state

    val select: SelType => IO[Unit] =
      $ _setStateL State.selectedType

    val selProps = SelectOne.Props[SelType](
      s.selectedType, types.value(), Some(select))

    val detail: Type => TagMod = {
      case GenericReqType(rt) => CreateGenericReq.Component((p, rt))
      case ReqCodeGroupType   => CreateReqCodeGroup.Component(p)
    }

    <.div(
      "Create ",
      selectComponent(selProps),
      s.selectedType map detail)
  }

  def ctrls(create: Option[IO[Unit]], status: Status, setStatus: Status => IO[Unit]): TagMod = {
    val c: TagMod = createButton(create)
    failureNotice(status, setStatus).fold(c)(c + _)
  }

  def createButton(create: Option[IO[Unit]]) =
    <.button(
      ^.disabled := create.isEmpty,
      ^.onClick ~~>? create,
      "Create") // english

  def failureNotice(status: Status, setStatus: Status => IO[Unit]): Option[TagMod] =
    status match {
      case Editing | Locked => None
      case Failed(err) => Some(
        <.div(err, <.button("Got it", ^.onClick ~~> setStatus(Editing)))) // English
    }

  def ajax(p: Props, setStatus: Status => IO[Unit], cmd: CreateContentCmd): IO[Unit] = IO {
    val remoteCall = p.createIO(cmd,
      TIO.Success(setStatus(Editing)),
      f => TIO.Failure(setStatus(Failed(f))))
    remoteCall >> setStatus(Locked)
  }.join

  object CreateReqCodeGroup { // ---------------------------------------------------------------------------------------

    val $$ = $ zoomL State.rcg
    val setStatus  = $$ zoomL CreateReqCodeGroupState.status  setStateIO (_: Status)
    val setReqCode = $$ zoomL CreateReqCodeGroupState.reqCode setStateIO (_: String)
    val setTitle   = $$ zoomL CreateReqCodeGroupState.title   setStateIO (_: String)

    val mkPropsReqCode = ReqCodeEditor.ForGroup.prepare(None, project.map(_.reqCodes.trie))
    val mkPropsTitle   = RichTextEditor.ReqCodeGroupTitle.prepare(project, projectText, projectWidgets, textSearch)

    def render(p: Props) = {
      val state = p.state.rcg

      val propsReqCode = mkPropsReqCode(VUCA.vu(state.reqCode, setReqCode))
      val propsTitle   = mkPropsTitle  (VUCA.vu(state.title,   setTitle))

      val create: Option[IO[Unit]] =
        for {
          code  <- propsReqCode.parseResult.toOption
          title <- propsTitle.parseResult.toOption
          if state.status ≠ Locked
        } yield
          ajax(p, setStatus, CreateContentCmd.CreateReqCodeGroup(code, title))

      <.table(
        <.thead(
          <.tr(
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th())),
        <.tbody(
          <.tr(
            <.td(propsReqCode.render),
            <.td(propsTitle.render),
            <.td(ctrls(create, state.status, setStatus)))))
    }

    val Component = ReactComponentB[Props]("CreateRCG")
      .stateless
      .render($ => render($.props))
      .build
  }

  object CreateGenericReq { // ---------------------------------------------------------------------------------------

    type Props = (CreationInterface.Props, CustomReqTypeId)

    val $$ = $ zoomL State.greq
    val setStatus   = $$ zoomL CreateGenericReqState.status   setStateIO (_: Status)
    val setReqCodes = $$ zoomL CreateGenericReqState.reqCodes setStateIO (_: String)
    val setTitle    = $$ zoomL CreateGenericReqState.title    setStateIO (_: String)
    val setTags     = $$ zoomL CreateGenericReqState.tags     setStateIO (_: String)
    val setImp      = $$ zoomL CreateGenericReqState.imp      setStateIO (_: String)

    val tagLookup = project.map(p => TagEditor.lookupG(p, _.tags.all))
    val impLookup = Px.apply2(project, projectText)(ImplicationEditor.lookupAll) map Must.apply

    val mkPropsReqCodes = ReqCodeEditor.ForReqs.prepare(Set.empty, project.map(_.reqCodes.trie))
    val mkPropsTitle    = RichTextEditor.GenericReqTitle.prepare(project, projectText, projectWidgets, textSearch)
    val mkPropsTags     = TagEditor.prepare(Set.empty, project.value(), tagLookup)._1
    val mkPropsImp      = ImplicationEditor.prepare(None, Column.ImplicationSrc, project, textSearch, impLookup)._1

    def render(p: Props) = {
      val state = p._1.state.greq

      val propsReqCodes = mkPropsReqCodes(VUCA.vu(state.reqCodes, setReqCodes))
      val propsTitle    = mkPropsTitle   (VUCA.vu(state.title,    setTitle))
      val propsTags     = mkPropsTags    (VUCA.vu(state.tags,     setTags))
      val propsImp      = mkPropsImp     (VUCA.vu(state.imp,      setImp))

      val create: Option[IO[Unit]] =
        for {
          codes   <- propsReqCodes.parseResult.toOption
          title   <- propsTitle   .parseResult.toOption
          tags    <- propsTags    .parseResult.toOption
          impSrcs <- propsImp     .parseResult.toOption
          if state.status ≠ Locked
        } yield
          ajax(p._1, setStatus, CreateContentCmd.CreateGenericReq(p._2, title, codes.added, tags.added, impSrcs.added))

      <.table(
        <.thead(
          <.tr(
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th(UiText.ColumnNames.tags),
            <.th(UiText.ColumnNames.implicationSrc),
            <.th())),
        <.tbody(
          <.tr(
            <.td(propsReqCodes.render),
            <.td(propsTitle   .render),
            <.td(propsTags    .render),
            <.td(propsImp     .render),
            <.td(ctrls(create, state.status, setStatus)))))
    }

    val Component = ReactComponentB[Props]("CreateGR")
      .stateless
      .render($ => render($.props))
      .build
  }
}
