package shipreq.webapp.server.redis

import boopickle.Pickler
import com.typesafe.scalalogging.StrictLogging
import org.redisson.api.RedissonClient
import org.redisson.api.listener.MessageListener
import org.redisson.client.codec.ByteArrayCodec
import scala.util.{Failure, Success}
import shipreq.base.util.BinaryData
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.{BinaryJvm, Protocol}
import shipreq.webapp.server.logic.Redis
import shipreq.webapp.base.protocol.BinCodecEvents

final class RedisViaRedisson(client: RedissonClient, schema: RedisSchema) extends Redis.ProjectAlgebra[Fx] with StrictLogging {
  import Redis._

  override protected def F = fxInstance

  private val protocolEvent: Protocol.Of[Pickler, VerifiedEvent.NonEmptySeq] =
    Protocol(BinCodecEvents.pickleVerifiedEventNES)

  private val byteArrayClass = classOf[Array[Byte]]

  override def subscribe(id: ProjectId, listener: VerifiedEvent.NonEmptySeq => Fx[Unit]) = {
    val topicName = schema.topic(id)

    val msgListener: MessageListener[Array[Byte]] = (_, msg) => {
      val bin = BinaryData.unsafeFromArray(msg)
      BinaryJvm.decode(bin, protocolEvent) match {
        case Success(ves) =>
          logger.debug(s"Received topic msg: project #${id.value}, events ${ves.describeEvents}")
          listener(ves).unsafeRun()
        case Failure(e) =>
          logger.warn(s"Failed to decode topic $topicName msg: $bin", e)
      }
    }

    Fx[Subscription[Fx]] {
      val topicRef = client.getTopic(topicName, ByteArrayCodec.INSTANCE)
      val listenerId = topicRef.addListener(byteArrayClass, msgListener)
      val unsub = Fx[Unit] {
        topicRef.removeListener(listenerId)
      }
      Subscription[Fx](unsub)
    }
  }

  override def read(id: ProjectId) = Fx[ProjectCache] {
    ProjectCache.empty // TODO
  }

  override def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]) = Fx[VerifiedEvent.Seq] {
    VerifiedEvent.Seq.empty // TODO
  }

  override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) = Fx[Boolean] {
    publishEvents(id, publishOnly).unsafeRun()
    true // TODO
  }

  override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) = Fx[Boolean] {
    publishEvents(id, cacheAndPublish).unsafeRun()
    true // TODO
  }

  override def publishEvents(id: ProjectId, events: VerifiedEvent.NonEmptySeq) = Fx[Unit] {
    val topicName = schema.topic(id)
    val topicRef = client.getTopic(topicName, ByteArrayCodec.INSTANCE)
    val bin = BinaryJvm.encode(protocolEvent)(events)
    topicRef.publish(bin.unsafeArray)
  }
}
