package shipreq.webapp.server.redis

import com.typesafe.scalalogging.StrictLogging
import java.lang.{Boolean => JBool}
import java.util.{ArrayList, Collections, List => JList}
import org.redisson.api.RScript.Mode
import org.redisson.api.listener.MessageListener
import org.redisson.api.{RScript, RedissonClient}
import org.redisson.client.codec.{ByteArrayCodec, StringCodec}
import scala.Predef.classOf
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scalaz.std.option._
import scalaz.syntax.traverse._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.member.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.server.logic.algebra.Redis
import shipreq.webapp.server.logic.algebra.Redis.ProjectSnapshot
import shipreq.webapp.server.logic.protocol.RedisProtocol._

object RedisViaRedisson {

  private[RedisViaRedisson] object Internals {

    final class Keys(val keys: JList[AnyRef]) extends AnyVal

    object Keys {
      val none: Keys =
        new Keys(Collections.unmodifiableList(Collections.emptyList()))

      def apply(k1: RedisKey): Keys = {
        val keys = new ArrayList[AnyRef](1)
        keys.add(k1.value)
        new Keys(keys)
      }

      def apply(k1: RedisKey, k2: RedisKey): Keys = {
        val keys = new ArrayList[AnyRef](2)
        keys.add(k1.value)
        keys.add(k2.value)
        new Keys(keys)
      }
    }

    final class Args(val args: mutable.Builder[Array[Byte], Array[Array[Byte]]]) extends AnyVal {
      @inline def +=(s: String         ): Unit = args += s.getBytes
      @inline def +=(i: Int            ): Unit = args += i.toString.getBytes
      @inline def +=(a: Array[Byte]    ): Unit = args += a
      @inline def +=(c: RedisChannel   ): Unit = this += c.value
      @inline def +=(e: VerifiedEvent  ): Unit = args += picklerEvent.encode(e).unsafeArray
      @inline def +=(s: ProjectSnapshot): Unit = args += picklerProjectSnapshot.encode(s).unsafeArray
      @inline def ++=(es: VerifiedEvent.Seq): Unit = es.foreach(+=(_))
      @inline def seq: Seq[Array[Byte]] = ArraySeq.unsafeWrapArray(args.result())
    }

    object Args {
      def apply() = new Args(Array.newBuilder)
    }

    final case class ReadEventsInput(id: ProjectId, beyond: Option[EventOrd.Latest])

    final case class WriteSnapshotInput(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq)

    final case class WriteEventsInput(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq)

    final case class PublishEventsInput(id: ProjectId, events: VerifiedEvent.NonEmptySeq)
  }
}

// =====================================================================================================================

final class RedisViaRedisson(client: RedissonClient, schema: RedisSchema) extends Redis.ProjectAlgebra[Fx] with StrictLogging {
  import Redis._
  import RedisViaRedisson.Internals._

  override protected def F = fxScalazInstance

  private val byteArrayClass = classOf[Array[Byte]]
  private val scriptString   = client.getScript(StringCodec.INSTANCE)
  private val scriptBinary   = client.getScript(ByteArrayCodec.INSTANCE)
  private val scriptRegistry = new ScriptRegistry

  private def defineScript[I, O](lua: Lua)(execUnsafe: (ScriptSha, I) => O): I => Fx[O] = {
    val deploy = Fx(ScriptSha(scriptString.scriptLoad(lua.processed)))
    scriptRegistry.register(deploy)((sha, i) => Fx(execUnsafe(sha, i)))
  }

  private def evalSha[A](mode: Mode, sha: ScriptSha, returnType: RScript.ReturnType, keys: Keys): A =
    scriptBinary.evalSha[A](mode, sha.value, returnType, keys.keys)

  private def evalSha[A](mode: Mode, sha: ScriptSha, returnType: RScript.ReturnType, keys: Keys, args: Args): A =
    scriptBinary.evalSha[A](mode, sha.value, returnType, keys.keys, args.seq: _*)

  override def subscribe(id: ProjectId, listener: Redis.Listener[Fx]): Fx[Subscription[Fx]] = {
    val topicName = schema.topic(id)

    val msgListener: MessageListener[Array[Byte]] = (_, msg) => {
      import Redis.ListenerError
      try {
        val decoded = picklerEvent.decodeBytes(msg).leftMap(ListenerError.DecodingFailure)
        listener(decoded).unsafeRun()
      } catch {
        case NonFatal(err) =>
          listener(-\/(ListenerError.RedisLibraryException(err))).unsafeRun()
      }
    }

    Fx[Subscription[Fx]] {
      val topicRef = client.getTopic(topicName.value, ByteArrayCodec.INSTANCE)
      val listenerId = topicRef.addListener(byteArrayClass, msgListener)
      val unsub = Fx[Unit] {
        topicRef.removeListener(listenerId)
      }
      Subscription[Fx](unsub)
    }
  }

//  private def logResult(a: Any) =
//    a match {
//      case l: JList[_] =>
//        println("-"*120)
//        println(s"RESULT: [${a.getClass}] ${l.size()} elements:")
//        l.asScala.map {
//          case r: Array[Byte] => s"${BinaryData.unsafeFromArray(r)} -- ${r.map(_.toChar).mkString}"
//          case a => s"[${if (a == null) "null" else a.getClass.getCanonicalName}] $a"
//        }.map("* " + _).foreach(println)
//        println("-"*120)
//      case r: Array[Byte] => println("RESULT: " + BinaryData.unsafeFromArray(r))
//      case _ => println(s"RESULT: [${if (a == null) "null" else a.getClass.getCanonicalName}] ${a}")
//    }

  private def fxWithFallback[@specialized(Boolean) A](name: String, default: A)(body: Fx[A]): Fx[A] =
    Fx {
      try
        body.unsafeRun()
      catch {
        case NonFatal(t) =>
          logger.warn(s"Exception during $name: $t", t)
          default
      }
    }

  private[this] val emptyEventResult = SafePickler.success(VerifiedEvent.Seq.empty)

  private def decodeEvents(raw: JList[Array[Byte]]): SafePickler.Result[VerifiedEvent.Seq] =
    raw.asScala
      .iterator
      .map(picklerEvent.decodeBytes)
      .foldLeft(emptyEventResult)((q, r) => q.flatMap(e1 => r.map(e2 => e1 + e2)))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val scriptRead = defineScript[ProjectId, SafePickler.Result[ProjectCache]](Lua.read) { (sha, id) =>

    // Call Redis
    val keys = Keys(schema.snapshot(id), schema.events(id))
    val result = evalSha[JList[_]](Mode.READ_ONLY, sha, RScript.ReturnType.MULTI, keys)

    // Parse results
    val binSS = Option(result.get(0).asInstanceOf[Array[Byte]])
    val binES = result.get(1).asInstanceOf[JList[Array[Byte]]]
    val decSS = binSS.traverse(picklerProjectSnapshot.decodeBytes)
    val decES = decodeEvents(binES)

    // Done
    for {
      ss <- decSS
      es <- decES
    } yield ProjectCache(ss, es)
  }

  private[this] val emptyCacheResult = SafePickler.success(ProjectCache.empty)

  override protected def _read(id: ProjectId) =
    fxWithFallback("read", emptyCacheResult)(scriptRead(id))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val scriptReadEvents = defineScript[ReadEventsInput, SafePickler.Result[VerifiedEvent.Seq]](Lua.readEvents) { (sha, in) =>
    import in._

    val keys = Keys(schema.events(id))

    val args = Args()
    args += beyond.fold(0)(_.value)

    val result = evalSha[JList[Array[Byte]]](Mode.READ_ONLY, sha, RScript.ReturnType.MULTI, keys, args)

    decodeEvents(result)
  }

  override def readEvents(id: ProjectId, beyond: Option[EventOrd.Latest]) =
    fxWithFallback("readEvents", emptyEventResult)(scriptReadEvents(ReadEventsInput(id, beyond)))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val scriptWriteSnapshot = defineScript[WriteSnapshotInput, Boolean](Lua.writeSnapshot) { (sha, in) =>
    import in._

    val keys = Keys(schema.snapshot(id), schema.events(id))

    val args = Args()
    args += schema.topic(id)
    args += snapshot.ord.value
    args += snapshot
    args ++= publishOnly

    evalSha[JBool](Mode.READ_WRITE, sha, RScript.ReturnType.BOOLEAN, keys, args)
  }

  override def writeSnapshot(id: ProjectId, snapshot: ProjectSnapshot, publishOnly: VerifiedEvent.Seq) =
    fxWithFallback("writeSnapshot", true)(scriptWriteSnapshot(WriteSnapshotInput(id, snapshot, publishOnly)))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val scriptWriteEvents = defineScript[WriteEventsInput, Boolean](Lua.writeEvents) { (sha, in) =>
    import in._

    val shouldPublish: VerifiedEvent => Boolean =
      if (cacheOnly.isEmpty)
        _ => true
      else if (cacheAndPublish.isEmpty)
        _ => false
      else {
        val pubOrds = cacheAndPublish.iterator.map(_.ord).toSet
        e => pubOrds.contains(e.ord)
      }

    val keys = Keys(schema.snapshot(id), schema.events(id))

    val args = Args()
    args += schema.topic(id)
    for (e <- cacheOnly ++ cacheAndPublish) {
      val key = if (shouldPublish(e)) -e.ord.value else e.ord.value
      args += key
      args += e
    }

    evalSha[JBool](Mode.READ_WRITE, sha, RScript.ReturnType.BOOLEAN, keys, args)
  }

  override def writeEvents(id: ProjectId, cacheOnly: VerifiedEvent.Seq, cacheAndPublish: VerifiedEvent.Seq) =
    if (cacheOnly.isEmpty && cacheAndPublish.isEmpty)
      Fx.pure(false)
    else
      fxWithFallback("writeEvents", true)(scriptWriteEvents(WriteEventsInput(id, cacheOnly, cacheAndPublish)))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val scriptPublishEvents = defineScript[PublishEventsInput, Unit](Lua.publishEvents) { (sha, in) =>
    import in._
    // logger.info(s"Publishing project #${id.value} events ${events.describeEvents}")

    val args = Args()
    args += schema.topic(id)
    args ++= events

    evalSha(Mode.READ_WRITE, sha, RScript.ReturnType.STATUS, Keys.none, args)
  }

  override def publishEvents(id: ProjectId, events: VerifiedEvent.NonEmptySeq) =
    fxWithFallback("publishEvents", ())(scriptPublishEvents(PublishEventsInput(id, events)))
}
