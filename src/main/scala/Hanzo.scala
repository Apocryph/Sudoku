object Hanzo {
  def main(args: Array[String]) = {
    val testPuzzle = Array(
      0,0,6,0,0,7,3,0,0,
      0,1,8,0,0,9,0,5,0,
      5,0,0,0,0,0,0,6,4,
      9,2,0,0,8,0,0,0,0,
      0,0,0,7,6,3,0,0,0,
      0,0,0,0,9,0,0,7,5,
      6,3,0,0,0,0,0,0,8,
      0,9,0,3,0,0,5,2,0,
      0,0,2,4,0,0,6,0,0
    )
    val sudoku = new Sudoku(testPuzzle)
    println(sudoku.toString)
    
    val hanzo = new Hanzo
    val sudoku2 = hanzo.stepSolve(sudoku)
    println("After one pass from Hazno")
    println(sudoku2.toString)
  }
}

class Hanzo {
  val validValues = (1 to 9).toSet
  val colsInASection = 3
  val rowsInASection = 3

  def getNeighborIndices(x: Int, y: Int): Set[Tuple2[Int,Int]] = {
    //Neighbors come from three "locations"
    //Row-wise neighbors
    //Column-wise neighbors
    //And section-wise neighbors
    getRowiseNeighbors(x,y) | getColumnwiseNeighbors(x,y) | getSectionNeighbors(x,y)
  }

  private def getRowiseNeighbors(x: Int, y: Int): Set[Tuple2[Int,Int]] = {
    val xs = (0 to 8).toSet - x
    xs.map(n => (n,y))
  }

  private def getColumnwiseNeighbors(x: Int, y: Int): Set[Tuple2[Int,Int]] = {
    val ys = (0 to 8).toSet - y
    ys.map(n => (x,n))
  }

  private def getSectionNeighbors(x: Int, y: Int): Set[Tuple2[Int,Int]] = {
    //first need to determine which section we're in
    val sectionX = x / 3
    val sectionY = y / 3

    val xValues = ((sectionX * colsInASection)
      until ((sectionX + 1) * colsInASection)).toSet
    val yValues = ((sectionY * rowsInASection)
      until ((sectionY + 1) * rowsInASection)).toSet

    //TODO: Figure out how to get an actual cross product working
    //xValues cross yValues
    var neighbors = Set[Tuple2[Int,Int]]()
    for (xVal <- xValues)
      for (yVal <- yValues)
        if (!(xVal == x && yVal == y))
          neighbors += Tuple2[Int,Int](xVal, yVal)

    return neighbors
  }

  def getPossibleValuesForIndex(sudoku: Sudoku, x: Int, y: Int) : Set[Int] = {
    validValues &~ getNeighborIndices(x,y)
      .map(index => sudoku.getValueAtIndex(index._1,index._2))
  }

  def stepSolve(sudoku: Sudoku): Sudoku = {
    //First step - figure out which indexes we can solve immediately
    val possibleIndices = (0 until (sudoku.numRows * sudoku.numCols))
    val mappedIndices = possibleIndices.map(i => sudoku.coordOneDimensionToTwo(i))
    val possibleValues = mappedIndices.map(index =>
        index -> getPossibleValuesForIndex(sudoku, index._1, index._2))

    //Second step - create a new sudoku as a clone of the original,
    //and apply the changes we've figured
    val rawGrid = sudoku.grid.clone
    for (value <- possibleValues)
    {
       if (value._2.size == 1)
         rawGrid(sudoku.coordTwoDimensionToOne(value._1._1, value._1._2)) = value._2.head
    }

    new Sudoku(rawGrid)
  }
}
