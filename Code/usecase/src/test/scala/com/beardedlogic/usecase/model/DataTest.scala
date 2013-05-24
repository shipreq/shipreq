package com.beardedlogic.usecase.model

import org.scalatest.FunSpec
import com.beardedlogic.usecase.test.TestDatabaseSupport

class DataTest extends FunSpec with TestDatabaseSupport {

  describe("Data") {
    it("should insert and read back") {
      val id1 = Data.create(DataType.UseCase).id
      val id2 = Data.create(DataType.FieldList).id
      Data.find(id1) should be(Some(Data(id1, DataType.UseCase)): Option[Data[_]])
      Data.find(id2) should be(Some(Data(id2, DataType.FieldList)): Option[Data[_]])
    }
  }
}