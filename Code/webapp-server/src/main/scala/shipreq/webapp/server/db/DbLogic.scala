package shipreq.webapp.server.db

//import doobie.imports._
//import java.time.Instant
//import shipreq.base.db.SqlHelpers._
//
//object DbLogic {
//  object admin {
//
//    val diagSelectNow: ConnectionIO[Instant] =
//      Query0[Instant]("select now()").unique
//
//    val statsCountUsers: ConnectionIO[UsrCount] =
//      Query0[(Long, Long)]("select count(username), count(1) from usr")
//        .unique
//        .map((UsrCount.apply _).tupled)
//
//    final case class UsrCount(registered: Long, total: Long) {
//      def pendingRegistration = total - registered
//    }
//
//    private val sqlStatsSizesByTypes = Query[String, (String, Long)]("""
//      WITH a as (
//          SELECT
//            relname "name"
//            ,pg_total_relation_size(C.oid) "size"
//          FROM pg_class C
//          LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
//          WHERE nspname NOT IN ('pg_catalog', 'information_schema')
//           AND nspname !~ '^pg_toast'
//           AND C.relkind = ?
//        ), b AS (
//          SELECT * FROM a WHERE size != 0
//          UNION SELECT '*', sum(size) FROM a
//        )
//        SELECT * FROM b WHERE size != 0 ORDER BY 1;
//      """.sql)
//
//    val statsTableSizes: ConnectionIO[List[(String, Long)]] =
//      sqlStatsSizesByTypes.toQuery0("r").list
//
//    val statsIndexSizes: ConnectionIO[List[(String, Long)]] =
//      sqlStatsSizesByTypes.toQuery0("i").list
//
//    def statsDatabaseSize(dbName: String): ConnectionIO[Long] =
//      Query[String, Long]("SELECT pg_database_size(?)")
//        .toQuery0(dbName.replaceFirst("^.*/", ""))
//        .unique
//  }
//}
