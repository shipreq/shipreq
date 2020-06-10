package shipreq.benchmark

import io.jsonwebtoken.Jwts
import java.nio.charset.Charset
import java.security.Key
import java.util.concurrent.TimeUnit
import io.jsonwebtoken.security.Keys
import org.openjdk.jmh.annotations._

/** [info] Benchmark     (method)  Mode  Cnt  Score   Error  Units
  *
  * [info] JwtBM.decode     hs256  avgt   24  5.877 ± 0.172  us/op
  * [info] JwtBM.decode     hs512  avgt   24  6.638 ± 0.234  us/op
  * [info] JwtBM.decode     hs768  avgt   24  6.774 ± 0.147  us/op
  *
  * [info] JwtBM.encode     hs256  avgt   24  5.586 ± 0.158  us/op
  * [info] JwtBM.encode     hs512  avgt   24  6.184 ± 0.087  us/op
  * [info] JwtBM.encode     hs768  avgt   24  6.135 ± 0.052  us/op
  */
//@Warmup(iterations = 8, time = 1, timeUnit = TimeUnit.SECONDS)
//@Measurement(iterations = 8, time = 1, timeUnit = TimeUnit.SECONDS)
//@Fork(3)
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JwtBM {
  import JwtBM.Cfg

  @Param(Array("hs256", "hs512", "hs768"))
  var method: String = _

  private[this] var cfg: Cfg = _

  @Setup def setup(): Unit = {
    cfg = method match {
      case "hs256" => Cfg.hs256
      case "hs512" => Cfg.hs512
      case "hs768" => Cfg.hs768
    }
  }

  @Benchmark def encode = cfg.encode()
  @Benchmark def decode = cfg.decode()
}

// =====================================================================================================================

object JwtBM {

  val ascii = Charset.forName("ASCII")

  val expirationMs = TimeUnit.MILLISECONDS.convert(24, TimeUnit.HOURS)

  final case class Cfg(keyText: String, len: Int) {
    assert(keyText.length == len)

    val keyBytes = keyText.getBytes(ascii)
    assert(keyBytes.length == len)

    val key: Key = Keys.hmacShaKeyFor(keyBytes)
    
    val parser = Jwts.parserBuilder().setSigningKey(key).build()

    def encode() = {
      val now = System.currentTimeMillis()
      val exp = new java.util.Date(now + expirationMs)
      Jwts.builder
        .setExpiration(exp)
        .setSubject("Aiden")
        .signWith(key)
        .compact
    }

    def decode() =
      parser
        .parseClaimsJws(sample)
        .getBody
        .getSubject

    val sample = encode()

    def take(n: Int) = Cfg(keyText take n, n)
  }

  object Cfg {
    val hs768 = Cfg("='.0PC]423!$qS@&--;)7mC(vKE|F;2*jSXc5gbU<Mt1&j-Ud3H6F)SZCV>N':ZR,IrMn~*lVF6RR|_y_4JUQ0@vUP&T46b9", 96)
    val hs512 = hs768.take(64)
    val hs256 = hs512.take(32)
  }
}