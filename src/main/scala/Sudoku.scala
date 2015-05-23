class Sudoku (val grid: Array[Int]){
  val numCols = 9
  val numRows = 9
  val numSubCols = 3
  val numSubRows = 3

  override def toString(): String = {
    gridToString(grid)
  }

  private def gridToString(grid: Array[Int]): String = {
    val rawRows = grid.grouped(numCols)
    val rowStrings = rawRows.map((row) => rowToString(row))
    val partitionedRowStrings = rowStrings.grouped(numSubRows)
    val combinedRowStrings = partitionedRowStrings.map((part) => part.mkString("\n"))
    combinedRowStrings.mkString("\n" + ("-"*numCols) + ("-"*((numCols/numSubCols)-1)) + "\n")
  }

  private def rowToString(row: Array[Int]): String = {
    val partitioned = row.grouped(numSubCols)
    val stringPartitioned = partitioned.map((part) => part.mkString(""))
    stringPartitioned.mkString("|")
  }

  def coordOneDimensionToTwo(i: Int) : Tuple2[Int,Int] = {
    (i % numCols, i / numCols)
  }

  def coordTwoDimensionToOne(x: Int, y: Int) : Int = {
    x + (y * numCols)
  }

  def getValueAtIndex(x: Int, y: Int): Int = {
    grid(coordTwoDimensionToOne(x,y))
  }
}
