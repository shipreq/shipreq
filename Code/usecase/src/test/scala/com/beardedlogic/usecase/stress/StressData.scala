package com.beardedlogic.usecase
package stress

import org.scalacheck.Gen
import scala.slick.jdbc.{StaticQuery => Q}
import model.DataType
import test.TestDatabaseSupport
import StressTestHelpers._

object StressData {
  System.setProperty("run.mode", "test")
  TestDatabaseSupport.init()

  def create(dataRows: Int) {

    val dataType = Gen.frequency(
      (1, DataType.FieldKey),
      (1, DataType.FieldList),
      (5, DataType.FieldValue),
      (9, DataType.Step),
      (1, DataType.UseCase))
    val revisions = Gen.choose(1, 20)
    val dataStream = Stream.continually((dataType.sample.get, revisions.sample.get))

    val threadLocalDb = new ThreadLocalDb
    val threadLocalQ1 = threadLocalDb.query {Q.update[(Long, Short)]("insert into data values(?,?)")}
    val threadLocalQ2 = threadLocalDb.query {Q.update[(Long, Long, Int)]("insert into value values(?,?,?)")}

    // Create data rows
    val dataIds = time(s"Created $dataRows data rows") {
      val nextId = new IdWithProgress("Created %d data rows...")
      (for ((dt, revs) <- dataStream.take(dataRows).par) yield {
        val id = nextId.get
        threadLocalQ1.execute(id, dt.ordinal)
        (id, revs)
      }).toList
    }

    // Create values
    val totalValueRows = dataIds.map(_._2).reduce(_ + _)
    println(s"Creating $totalValueRows value rows...")
    time(s"Created $totalValueRows values rows") {
      val nextId = new IdWithProgress("Created %d value rows...")
      for ((dataId, revs) <- dataIds.par) {
        for (rev <- 1 to revs) {
          threadLocalQ2.execute(nextId.get, dataId, rev)
        }
      }
    }

    println("Done.")
    threadLocalDb.closeAll
  }
}