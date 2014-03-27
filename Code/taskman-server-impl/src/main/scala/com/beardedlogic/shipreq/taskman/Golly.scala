package com.beardedlogic.shipreq.taskman
//
//import akka.actor.{ActorRef, Actor, Props, ActorLogging}
//import com.beardedlogic.shipreq.taskman.CrossActorMessages.{HaveSomeWork, GimmeWork}
//import shipreq.base.db.DbTemplate
//import com.googlecode.flyway.core.Flyway
//import java.util.Properties
//import shipreq.base.util.{JPropertiesValueReader, ExternalValueReader}
//import java.util.regex.Pattern
//import com.jolbox.bonecp.hooks.{AbstractConnectionHook, ConnectionHook}
//import com.jolbox.bonecp.ConnectionHandle
//
//object CrossActorMessages {
//
//  case object GimmeWork
//  case object HaveSomeWork
//
//}
//
//class Source extends Actor with ActorLogging {
//  override def preStart() = {    log.info("STARTED!")  }
//
//  val rnd = new scala.util.Random
//
//  override def receive = {
//    case GimmeWork =>
//      log.info("Checking for work...")
////      if (rnd.nextBoolean())
//      (1 to 10).foreach(_ =>
//        sender() ! HaveSomeWork
//      )
//
//  }
//}
//
//class Manager(source: ActorRef, workerProps: Props) extends Actor with ActorLogging {
//  import akka.routing.{Router, FromConfig}
//
//  import context.dispatcher
//  import scala.concurrent.duration._
//
//  def cfg = context.system.settings.config
//  val pollEvery = Duration(cfg.getString("manager.poll")) match {
//    case d: FiniteDuration => d
//    case x => throw new RuntimeException(s"Invalid polling time, not finite: $x")
//  }
//
//  case object PollForWork
//  val ticker = context.system.scheduler.schedule(0 millis, pollEvery, self, PollForWork)
//
//  val router: ActorRef =
//    context.actorOf(FromConfig.props(workerProps), "router")
//
//  override def preStart() = {    log.info("STARTED!")  }
//
//  override def receive = {
//    case PollForWork =>
//      log.info("")
//      source ! GimmeWork
//    case HaveSomeWork =>
//      log.info("Got some work!!")
//      router ! HaveSomeWork
////      source ! GimmeWork
//  }
//}
//
//class Worker extends Actor with ActorLogging {
//  override def preStart() = {    log.info("STARTED!")  }
//  override def receive = {case x => log.info("----> {}", x); Thread.sleep(2000); log.info("<----")}
//}
//
//// =======================================================================================================================
//
//object MyDb extends DbTemplate {
//  import shipreq.base.db._
//
//  lazy val schema = "awesome"
//
//  override protected def establishConnection() = {
//    val p = new Properties
//    p.load(getClass.getResourceAsStream("/dev.props"))
//    val r = JPropertiesValueReader(p)
//    import r._
//    new BaseDbConnection(DbTemplate setSearchPath schema)
//  }
//
//  override protected def flywayCfg = DbTemplate setSchema schema
//
////  @inline def DataSource = baseConn.DataSource
//  import baseConn.Slick
//
////  object DaoProvider extends DaoProvider {
////    override def withRawSession[T](f: Session => T): T = Slick.withSession(f)
////    override protected def rawSession(): Session       = Slick.createSession()
////  }
//
//  def blah() {
//    import scala.slick.session.Session
//    import scala.slick.jdbc.{StaticQuery => Q}
//    import Q.interpolation
//
//    Slick.withSession { implicit s: Session =>
//      val count= sql"SELECT count(1) FROM yay".as[Int].first
//      println(s"-------- COUNT = $count")
//    }
//  }
//
//}
//
//
//object Main {
//  import akka.actor.ActorSystem
//  import akka.actor.Terminated
//  import akka.actor.ActorRef
//
//  val DedicatedDispatcher = "dedicated"
////  val WorkDispatcher = "work-dispatcher"
//
//  def main(args: Array[String]): Unit = {
//    MyDb.init()
//    MyDb.blah()
//  }
//
//  def main2(args: Array[String]): Unit = {
//    val system = ActorSystem("Main")
//    val workerProps = Props[Worker] //.withDispatcher(WorkDispatcher)
//    val source = system.actorOf(Props[Source].withDispatcher(DedicatedDispatcher), "source")
//    val manager = system.actorOf(Props(classOf[Manager], source, workerProps).withDispatcher(DedicatedDispatcher), "manager")
//    // system.actorOf(Props(classOf[Terminator], producer), "terminator")
//  }
//
//  class Terminator(app: ActorRef) extends Actor with ActorLogging {
//    context watch app
//    def receive = {
//      case Terminated(_) ⇒
//        log.info("application supervisor has terminated, shutting down")
//        context.system.shutdown()
//    }
//  }
//}
