package shipreq.benchmark.data

import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, RandomEventStream}

object EventStreamSample {

//  def main(args: Array[String]): Unit = {
//    println()
//    val ves = RandomEventStream.verifiedEvents(2).run(Project.empty).sample._2
//    ves.foreach(println)
//    println()
//  }

  def sample = RandomEventStream.verifiedEvents(200).run((Project.empty, EventOrd.first)).withSeed(0).sample

  val ((p, _), ves) = sample

  assert(ves.toString == sample._2.toString)
}
