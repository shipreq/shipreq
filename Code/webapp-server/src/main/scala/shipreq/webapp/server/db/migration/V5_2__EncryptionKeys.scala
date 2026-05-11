package shipreq.webapp.server.db.migration

import cats.instances.list._
import cats.syntax.traverse._
import com.typesafe.scalalogging.StrictLogging
import doobie._
import doobie.implicits._
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.util.BinaryData
import shipreq.webapp.server.logic.algebra.Crypto

class V5_2__EncryptionKeys extends DbMigration with StrictLogging {

  override protected def migration =
    for {
      _ <- addEncryptionKeys("usrd", "usr_id")
      _ <- addEncryptionKeys("project", "id")
    } yield ()

  private val crypto = Crypto.default[ConnectionIO]

  private def addEncryptionKeys(table: String, id: String): ConnectionIO[Unit] = {
    val col           = "encryption_key"
    val addColumn     = execute(s"ALTER TABLE $table ADD COLUMN $col BYTEA")
    val makeMandatory = execute(s"ALTER TABLE $table ALTER COLUMN $col SET NOT NULL")
    val makeUnique    = execute(s"ALTER TABLE $table ADD CONSTRAINT ${table}_unique_$col UNIQUE ($col)")
    val getIds        = Query0[Long](s"SELECT $id FROM $table").to[List]
    val setKey        = Update[(BinaryData, Long)](s"UPDATE $table SET $col = ? WHERE $id = ?")
    val generateKey   = (id: Long) => crypto.generateKey256.flatMap(k => setKey.run((k, id)))

    for {
      _   <- addColumn
      ids <- getIds
      _   <- ids.traverse(generateKey)
      _   <- makeMandatory
      _   <- makeUnique
    } yield ()
  }
}
