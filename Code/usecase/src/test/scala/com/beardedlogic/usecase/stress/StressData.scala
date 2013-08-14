package com.beardedlogic.usecase
package stress
/*
import org.scalacheck.Gen
import scala.slick.jdbc.{StaticQuery => Q}

import lib.Misc
import lib.security.PasswordAndSalt
import model.DataType
import test.TestDatabaseSupport
import StressTestHelpers._

object StressData {
  System.setProperty("run.mode", "test")
  TestDatabaseSupport.init()

  def populateValue(dataRows: Int = 50000) {

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

  def populateUser(pendingRows: Int = 10000, confirmedRows: Int = 100000) {

    val tokens = Stream.continually(Misc.randomConfirmationToken)

    val threadLocalDb = new ThreadLocalDb
    val Q1 = threadLocalDb.query {Q.update[(Long, String, String)]("INSERT INTO usr(id, email, confirmation_token, confirmation_sent_at) VALUES(?,?,?,NOW())")}
    val Q2 = threadLocalDb.query {Q.update[(Long, String, String, String, String)]("INSERT INTO usr(id, username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,?,?,?,NOW(),NOW(),NOW())")}

    // Create unconfirmed users
    time(s"Created $pendingRows unconfirmed users") {
      val nextId = new IdWithProgress("Created %d unconfirmed users...")
      for (token <- tokens.take(pendingRows).par) {
        val id = nextId.get
        Q1.execute(id, s"u.$id@stress.com", token)
      }
    }

    // Create unconfirmed users
    time(s"Created $confirmedRows confirmed users") {
      val nextId = new IdWithProgress("Created %d confirmed users...", 100000 + pendingRows)
      for (p <- tokens.take(confirmedRows).par) {
        val id = nextId.get
        val ps = PasswordAndSalt.hashWithRandomSalt(p)
        Q2.execute(id, s"u$id", s"u.$id@stress.com", ps.hashedPassword, ps.salt)
      }
    }

    println("Done.")
    threadLocalDb.closeAll
  }
}
*/