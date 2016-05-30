package shipreq.base.util

/**
 * Taken from
 * https://github.com/dsl-platform/101-dsl-examples/blob/master/001-periodic-table-of-elements/scala/src/main/scala/com/dslplatform/examples/AsciiTable.scala
 */
object AsciiTable {
  // Characters that make up the table.
  private val CrossBorder  = '+'
  private val RowBorder    = '-'
  private val ColumnBorder = '|'
  private val HeaderBorder = '-'
  private val NL           = System.getProperty("line.separator")

  /** An entry (a row) is a list of strings (representing columns). A table is
   *  a list of rows.
   */
  def apply(table: Seq[Seq[String]]): String = {
    // Number of rows
    val rowCount = table.size
    // Maximum text length for each column
    val maxColLengths = table.transpose.map(_.map(s => if (s eq null) 4 else s.length).max)

    // String builder which will be filled row by row as we progress through
    // the table.
    val sb = new StringBuilder

    /** Main loop.
     *  `rowNum` indicates the row we are currently on.
     *  `isSeparator` indicates weather we are on a separator between the rows.
     *  +--------+------+    <- row -1, separator
     *  | number | name |    <- row  0, not separator
     *  +--------+------+    <- row  0, separator
     *  |  ...   | ...  |    <- row  1, not separator
     */
    def doMake(rowNum: Int, isSeparator: Boolean): String = {
      val isEnd = rowNum == rowCount

      (isEnd, isSeparator) match {
        case (true, _) =>
          // End, so return the result
          sb.toString
        case (false, true) =>
          // Separator.
          addSeparator(rowNum == 0)
          doMake(rowNum + 1, false)
        case (false, false) =>
          // Not separator.
          val row = table(rowNum)
          addRow(row)
          doMake(rowNum, true)
      }
    }

    def addSeparator(isHeader: Boolean) {
      val border = if (isHeader) HeaderBorder else RowBorder
      sb.append(CrossBorder)
      maxColLengths foreach { length =>
        sb.append(border.toString * (length + 2))
        sb.append(CrossBorder)
      }
      sb.append(NL)
    }

    def addRow(row: Seq[String]) {
      sb.append(ColumnBorder)
      for (column <- maxColLengths.indices) {
        var cell = row(column)
        if (cell eq null)
          cell = "null"
        val ws = maxColLengths(column) - cell.length + 2

        sb.append(" " * (ws/2))
        sb.append(cell)
        sb.append(" " * (ws/2 + ws%2))
        sb.append(ColumnBorder)
      }
      sb.append(NL)
    }

    doMake(-1, true)
  }
}