package shipreq.webapp.member.project.library

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackOption, CallbackTo}
import java.time.{Duration, Instant}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.member.project.util.DataReusability.reusabilityProject

final class MutableProjectLibrary[PL <: ProjectLibrary](initialState: PL,
                                                        clock: CallbackTo[Instant]) extends MutableProjectLibrary.Staleness {
  import MutableProjectLibrary.OrdPromise

  private var _state: PL =
    initialState

  val get: CallbackTo[PL] =
    CallbackTo(_state)

  private val _pxProject: Px.ThunkM[Project] =
    Px.apply(_state.latest).withReuse.manualRefresh

  val pxProject: Px[Project] =
    _pxProject

  def set(p: PL): Callback =
    Callback {
      _state = p
    }

  def setOption(o: Option[PL]): Callback =
    Callback.traverseOption(o)(set)

  def update(ves: VerifiedEvent.Seq): Callback =
    Callback.unless(ves.isEmpty) {
      updateBy(_.update(ves, _))
    }

  def update(p: Project): Callback =
    updateBy(_.update(p, _))

  def update(u: Project \/ VerifiedEvent.Seq): Callback =
    u.fold(update, update)

  private def updateBy(f: (PL, Instant) => Option[ProjectLibrary.UpdateFor[PL#This]]): Callback =
    for {
      s1 <- get
      now <- clock
      _ <- Callback.traverseOption(f(s1, now))(u => Callback {

        // Update state
        _state = u.newLibrary.asInstanceOf[PL] // cbf jumping through hoops for type-level proof of this

        // Refresh Pxs
        val projectChanged = u.newlyAppliedEvents.nonEmpty
        if (projectChanged)
          _pxProject.refresh()

        // Complete ord promises
        if (projectChanged && _ordPromises.nonEmpty) {
          val newOrd = _state.latest.ordAsInt

          // Remove releasable promises
          val (releasable, pending) = _ordPromises.partition(_.ord.value <= newOrd)
          _ordPromises = pending

          // Execute releasable promises
          for (p <- releasable)
            p.complete.attempt.runNow() match {
              case Right(_) =>
              case Left(e) => LoggerJs.exception(e)
            }
        }

      })
    } yield ()

  private var _ordPromises: List[OrdPromise] =
    Nil

  def projectAt(ord: Option[EventOrd.Latest]): AsyncCallback[Project] =
    ord match {
      case Some(o) => projectAt(o.asEventOrd)
      case None    => AsyncCallback.pure(Project.empty)
    }

  def projectAt(ord: EventOrd): AsyncCallback[Project] =
    AsyncCallback.suspend {
      if (ord <= _state.ord)
        AsyncCallback.delay(_state.projectAt(ord).get)
      else
        AsyncCallback.barrier.asAsyncCallback.flatMap { barrier =>
          val ordPromise = OrdPromise(ord, barrier.complete)
          val save = AsyncCallback.delay(_ordPromises ::= ordPromise)
          save >> barrier.await >> projectAt(ord)
        }
    }

  override protected def _addStalenessListener(handle   : NonEmptySet[EventOrd] => Callback,
                                               interval : Duration,
                                               tolerance: Duration): Callback = {

    val task: Callback =
      for {
        pl      <- get.toCBO
        now     <- clock.toCBO
        missing <- CallbackOption.option(pl.missingEventsIfStale(now, tolerance))
        _       <- handle(missing).toCBO
       } yield ()

    task.setInterval(interval).void
  }

  // For tests
  def pendingPromiseCount(): Int =
    _ordPromises.size
}

object MutableProjectLibrary {

  def apply[PL <: ProjectLibrary](initialState: PL,
                                  clock: CallbackTo[Instant] = CallbackTo.now): MutableProjectLibrary[PL] =
    new MutableProjectLibrary(initialState, clock)

  def empty(clock: CallbackTo[Instant] = CallbackTo.now): MutableProjectLibrary[ProjectLibrary] =
    apply(ProjectLibrary.empty(CacheJs()), clock)

  trait Staleness {
    final def addStalenessListener(handle   : NonEmptySet[EventOrd] => Callback,
                                   interval : Duration = Duration.ofSeconds(60),
                                   tolerance: Duration = Duration.ofSeconds(20),
                                  ): Callback =
      _addStalenessListener(handle, interval, tolerance)

    protected def _addStalenessListener(handle   : NonEmptySet[EventOrd] => Callback,
                                        interval : Duration,
                                        tolerance: Duration): Callback
  }

  private final case class OrdPromise(ord: EventOrd, complete: Callback)
}
