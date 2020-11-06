package shipreq.webapp.member.test.project

import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.{ApplyEvent, Event}
import shipreq.webapp.member.test.project.ApplicableEventGen.ObserveFn

object EventStats {

  private[EventStats] val reportLineFmt = s"| %-${EventName.maxNameLen}s | %7s | %4s |"
  private[EventStats] val reportLineHdr = reportLineFmt.format("EVENT", "GOOD", "BAD")
  private[EventStats] val reportLineSep = s"+-${"-" * EventName.maxNameLen}-+-${"-"*7}-+-${"-"*4}-+"

  val empty = new EventStats(Map.empty, Map.empty)

  val observeFn: ObserveFn[EventStats] =
    _.add(_, _)
}

class EventStats(val ok: Map[EventName, Int], val ko: Map[EventName, Int]) {
  import EventStats._

  type M = Map[EventName, Int]

  private def inc(m: M, s: EventName, i: Int = 1): M =
    m.updated(s, m.get(s).fold(i)(_ + i))

  private def append(big: M, small: M): M =
    small.foldLeft(big)((q, e) => inc(q, e._1, e._2))

  def add(e: Event, r: ApplyEvent.Result): EventStats = {
    val n = EventName(e)
    r.fold(err => {
//        if (!err.contains("\n"))
        if (n.value.contains("Reposi"))
          println(s"[Event Application Failure] $err\n$e\n")
        new EventStats(ok = ok, ko = inc(ko, n))
      }, (_: Project) =>
        new EventStats(ok = inc(ok, n), ko = ko))
  }

  def +(smaller: EventStats): EventStats =
    new EventStats(
      ok = append(ok, smaller.ok),
      ko = append(ko, smaller.ko))

  private def lookup(m: M): EventName => String =
    m.mapValuesNow(_.toString)
      .withDefaultValue("")
      .apply

  def report: String = {
    val mOK = lookup(ok)
    val mKO = lookup(ko)
    val content = EventName.allList.map(s => reportLineFmt.format(s, mOK(s), mKO(s)))

    val (cOK, cKO) = (ok, ko).mapEach(_.valuesIterator.sum)
    val c = cOK + cKO
    def per(i: Int) = (i.toDouble / c.toDouble * 100).toInt.toString + "%"
    val total = reportLineFmt.format("Σ", cOK.toString, cKO.toString)
    val totalP = reportLineFmt.format("Σ%", per(cOK), per(cKO))

    val (dOK, dKO) = (ok, ko).mapEach(_.keys.size)
    def distPer(i: Int) = (i.toDouble / EventName.size.toDouble * 100).toInt.toString + "%"
    val dist = reportLineFmt.format("Distribution", distPer(dOK), per(dKO))

    ( reportLineSep :: reportLineHdr ::
      reportLineSep :: content :::
      reportLineSep :: total :: totalP :: dist ::
      reportLineSep :: Nil
    ) mkString "\n"
  }
}
