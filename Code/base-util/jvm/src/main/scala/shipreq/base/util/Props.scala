package shipreq.base.util

import japgolly.clearconfig._
import japgolly.clearconfig.internals.Key
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util.FxModule._

object Props {

  def fileSources: ConfigSources[Fx] =
    ConfigSource.propFileOnClasspath[Fx]("shipreq.properties", optional = true) >
    ConfigSource.propFileOnClasspath[Fx]("secret.properties", optional = true)

  def sources: ConfigSources[Fx] =
    InlineProperties(ConfigSource.environment[Fx]).mapKeyQueries(acceptExternalKeyFormat) >
    fileSources >
    ConfigSource.system[Fx].mapKeyQueries(acceptExternalKeyFormat)

  private def acceptExternalKeyFormat(k: Key): List[Key] =
    k :: k.map(_.toUpperCase.replace('.', '_')) :: Nil

  object InlineProperties { // TODO Remove after clear-config v1.4.0 release
    def key = Key("SHIPREQ_INLINE_PROPERTIES")

    def apply[F[_]](source: ConfigSource[F])(implicit F: Monad[F]): ConfigSource[F] = {

      val prepare2: F[String \/ ConfigStore[F]] =
        source.prepare.flatMap {

          case \/-(store) =>
            store.all.flatMap { map =>
              map.get(key) match {

                case Some(value) =>
                  val is = new ByteArrayInputStream(value.getBytes(StandardCharsets.UTF_8))
                  val store2 = ConfigStore.ofJavaPropsFromInputStream[F](is, close = true)
                  for {
                    map2 <- store2.all
                  } yield {
                    val common = (map.keySet & map2.keySet).filter(k => map(k) != map2(k))
                    if (common.nonEmpty) {
                      val hdr = s"The following keys are defined at both the top-level and in ${key.value}: "
                      val keys = common.iterator.map(_.value).toList.sorted
                      -\/(keys.mkString(hdr, ", ", "."))
                    } else
                      \/-(ConfigStore[F](F.pure(map ++ map2 - key)))
                  }

                case None =>
                  F.pure(\/-(store))
              }
            }

          case e@ -\/(_) =>
            F.pure(e)
        }

      source.copy(prepare = prepare2)
    }
  }
}