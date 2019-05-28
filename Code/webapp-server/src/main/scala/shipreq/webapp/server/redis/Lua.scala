package shipreq.webapp.server.redis

final case class Lua(input: String) {
  val processed =
    input
      .split("\n")
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .filterNot(_ startsWith "--")
      .map(_.replaceFirst("^local +(\\S+) += +(\\S+)", "local $1=$2"))
      .mkString("\n")

  // println("="*120); println(processed); println("="*120)
}

private[redis] object Lua {

  /** This is how data is stored in Redis:
    *
    * Snapshot - ver :: data :: Nil
    * Events   - Sorted set {score = ver, message = data}
    */
  private object Internals {
    final case class KeySnapshot(name: String) { override def toString = name}
    final case class KeyEvents(name: String) { override def toString = name}
    final case class Channel(name: String) { override def toString = name}

    def declareKeySnapshot(i: Int)(implicit k: KeySnapshot) = s"local $k = KEYS[$i]"
    def declareKeyEvents  (i: Int)(implicit k: KeyEvents  ) = s"local $k = KEYS[$i]"
    def declareChannel    (i: Int)(implicit k: Channel    ) = s"local $k = ARGV[$i]"

    object snapshot {
      def getVer(implicit k: KeySnapshot) = s"(tonumber(redis.call('LINDEX',$k,0)) or 0)"
      def getBin(implicit k: KeySnapshot) = s"redis.call('LINDEX',$k,1)"

      def set(ver: String, bin: String)(implicit k: KeySnapshot) =
        s"""
           |redis.call('LPOP',$k)
           |redis.call('LPOP',$k)
           |redis.call('LPUSH',$k,$bin,$ver)
       """.stripMargin
    }

    object events {
      def isEmpty                       (implicit k: KeyEvents) = s"(redis.call('EXISTS',$k)==0)"
      def getMinVer                     (implicit k: KeyEvents) = s"tonumber(redis.call('ZRANGE',$k,0,0,'WITHSCORES')[2])"
      def getMaxVer                     (implicit k: KeyEvents) = s"tonumber(redis.call('ZRANGE',$k,-1,-1,'WITHSCORES')[2])"
      def getAll                        (implicit k: KeyEvents) = s"redis.call('ZRANGE',$k,0,-1)"
      def getBeyond(ord: String)        (implicit k: KeyEvents) = s"redis.call('ZRANGEBYSCORE',$k,$ord+1,'+inf')"
      def add(ver: String, data: String)(implicit k: KeyEvents) = s"redis.call('ZADD',$k,'NX',$ver,$data)"
      def remove_<=(ver: String)        (implicit k: KeyEvents) = s"redis.call('ZREMRANGEBYSCORE',$k,1,$ver)"
    }

    object total {
      def isComplete(snapshotVer: String)(implicit k: KeyEvents) = {
        val follows = s"($snapshotVer + 1 == ${events.getMinVer})"
        s"(${events.isEmpty} or $follows)"
      }

      def getVer(implicit ks: KeySnapshot, ke: KeyEvents): String =
        getVer(snapshot.getVer)

      def getVer(snapshotVer: String)(implicit k: KeyEvents): String = {
        def ver = s"(${events.getMaxVer} or $snapshotVer)"
        s"${isComplete(snapshotVer)} and $ver or 0" // (a ? b : c) is (a and b or c) in Lua
      }
    }

    object publish {
      def one(es: String)(implicit c: Channel) = s"redis.call('publish',$c,$es)"
    }

    object debug {
      def log(msg: String) = s"redis.log(redis.LOG_WARNING,$msg)"
      def logExprs(vars: String*) = log(vars.map(v => s"'$v='..(tostring($v))").mkString("..', '.."))
      def logExprsT(vars: String*) = log(vars.map(v => s"'$v:'..(type($v))..'='..(tostring($v))").mkString("..', '.."))
    }
  }

  import Internals._

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val read = Lua {
    implicit val keySnapshot = KeySnapshot("ks")
    implicit val keyEvents = KeyEvents("ke")
    s"""
       |${declareKeySnapshot(1)}
       |${declareKeyEvents(2)}
       |return {${snapshot.getBin}, ${events.getAll}}
     """.stripMargin
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val readEvents = Lua {
    implicit val keyEvents = KeyEvents("ke")
    val beyond = "b"
    s"""
       |${declareKeyEvents(1)}
       |local $beyond = tonumber(ARGV[1])
       |if $beyond == 0 then
       |  return ${events.getAll}
       |else
       |  return ${events.getBeyond(beyond)}
       |end
     """.stripMargin
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val writeSnapshot = Lua {
    implicit val keySnapshot = KeySnapshot("ks")
    implicit val keyEvents   = KeyEvents("ke")
    implicit val channel     = Channel("c")
    val snapshotVer          = "ver"
    val snapshotBin          = "bin"
    val ok                   = "ok"
    s"""
       |${declareKeySnapshot(1)}
       |${declareKeyEvents(2)}
       |
       |${declareChannel(1)}
       |local $snapshotVer = tonumber(ARGV[2])
       |local $snapshotBin = ARGV[3]
       |
       |local $ok = $snapshotVer > ${snapshot.getVer}
       |if $ok then
       |  ${snapshot.set(ver = snapshotVer, bin = snapshotBin)}
       |  ${events.remove_<=(snapshotVer)}
       |end
       |
       |for i = 4,#ARGV do
       |  ${publish.one("ARGV[i]")}
       |end
       |
       |return $ok
     """.stripMargin
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // writeEvents [key] [key] [channel] ([cmd] [event data] …)
  // where math.abs(cmd) = event ord
  //   and cmd < 0       = publish
  val writeEvents = Lua {
    implicit val keySnapshot = KeySnapshot("ks")
    implicit val keyEvents   = KeyEvents("ke")
    implicit val channel     = Channel("c")
    val redisTotalVer        = "v"
    val startVer             = "s"
    val n                    = "n"
    val ok                   = "ok"
    val firstCmdArg          = 2
    s"""
       |${declareKeySnapshot(1)}
       |${declareKeyEvents(2)}
       |${declareChannel(1)}
       |local $redisTotalVer = ${total.getVer}
       |
       |local $n = ${firstCmdArg - 2}
       |local $startVer = 0
       |repeat
       |  $n = $n + 2
       |  $startVer = tonumber(ARGV[$n])
       |  if $startVer ~= nil then
       |    $startVer = math.abs($startVer)
       |  end
       |until $startVer == nil or $startVer > $redisTotalVer
       |
       |local $ok = $startVer == $redisTotalVer + 1
       |
       |if $ok then
       |  local prev,j = $redisTotalVer,0
       |  for i = $n,#ARGV,2 do
       |    j=math.abs(tonumber(ARGV[i]))
       |    if j ~= (prev + 1) then break end
       |    ${events.add("j", "ARGV[i+1]")}
       |    prev=j
       |  end
       |end
       |
       |for i = $firstCmdArg,#ARGV,2 do
       |  if tonumber(ARGV[i]) < 0 then
       |    ${publish.one("ARGV[i+1]")}
       |  end
       |end
       |
       |return $ok
     """.stripMargin
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val publishEvents = Lua {
    implicit val channel = Channel("c")
    s"""
       |${declareChannel(1)}
       |
       |for i = 2,#ARGV do
       |  ${publish.one("ARGV[i]")}
       |end
     """.stripMargin
  }
}
