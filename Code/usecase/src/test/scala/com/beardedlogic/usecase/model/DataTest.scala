package com.beardedlogic.usecase.model

import org.scalatest.FunSpec
import com.beardedlogic.usecase.test.TestDatabaseSupport

class DataTest extends FunSpec with TestDatabaseSupport {

  describe("Data") {
    it("should insert and read back") {
      val id1 = db.createData(DataType.UseCase).id
      val id2 = db.createData(DataType.FieldList).id
      db.findData(id1) should be(Some(Data(id1, DataType.UseCase)): Option[Data[_]])
      db.findData(id2) should be(Some(Data(id2, DataType.FieldList)): Option[Data[_]])
    }
  }
}