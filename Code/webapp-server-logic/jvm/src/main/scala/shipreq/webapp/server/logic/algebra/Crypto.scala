package shipreq.webapp.server.logic.algebra

import java.security._
import scalaz.Applicative
import shipreq.base.util.BinaryData
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey
import shipreq.webapp.server.logic.data.{ProjectEncryptionKey, UserEncryptionKey}

trait Crypto[F[_]] {

  /** Generate a 256-bit key */
  def generateKey256: F[BinaryData]

  def sha256(input: BinaryData): BinaryData

  final def clientSideProjectEncryptionKey(u: UserEncryptionKey,
                                           p: ProjectEncryptionKey): ClientSideProjectEncryptionKey = {
    val b =  u.value ++ p.value
    ClientSideProjectEncryptionKey(sha256(b))
  }

}

object Crypto {

  def default[F[_]](implicit F: Applicative[F]): Crypto[F] =
    // Details documented in https://shipreq.com/project/d6My#/reqs/IV-42
    new Crypto[F] {
      import DefaultInstances._

      override def generateKey256: F[BinaryData] =
        F.point {
          val a = new Array[Byte](32)
          rnd256.nextBytes(a)
          BinaryData.unsafeFromArray(a)
        }

      override def sha256(input: BinaryData): BinaryData = {
        // New instance every time because it's stateful and not thread-safe. New instances are cheap.
        val md = MessageDigest.getInstance("SHA-256")
        md.update(input.unsafeArray)
        BinaryData.unsafeFromArray(md.digest())
      }
    }

  private object DefaultInstances {

    // SecureRandom is thread-safe
    val rnd256: SecureRandom = {
      val drbgParams = DrbgParameters.instantiation(256, DrbgParameters.Capability.PR_AND_RESEED, null)
      SecureRandom.getInstance("DRBG", drbgParams)
    }

  }

}