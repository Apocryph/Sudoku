import scala.collection.breakOut

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
    var sudoku = new Sudoku(testPuzzle)
    println("Initial state:")
    println(sudoku.toString)
    println("\nPress any key to step solve")
    val hanzo = new Hanzo

    var input = readLine()
    while (input != "q"){
      sudoku = hanzo.stepSolve(sudoku)
      println("\n" + sudoku.toString)
      println("\nPress any key to continue.  Press q to quit")
      input = readLine()
    }
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

    neighbors
  }

  def getPossibleValuesForIndex(sudoku: Sudoku, x: Int, y: Int) : Set[Int] = {
    validValues &~ getNeighborIndices(x,y)
      .map(index => sudoku.getValueAtIndex(index._1,index._2))
  }

  def getAllRowIndices(sudoku: Sudoku): Set[Set[Tuple2[Int,Int]]] = {
    (0 to 8).map(n => getRowIndices(sudoku, n)).toSet
  }

  def getRowIndices(sudoku: Sudoku, y: Int): Set[Tuple2[Int,Int]] = {
    (0 to 8).map(n => (n,y)).toSet
  }

  def getAllColumnIndices(sudoku: Sudoku): Set[Set[Tuple2[Int,Int]]] = {
    (0 to 8).map(n => getColumnIndices(sudoku,n)).toSet
  }

  def getColumnIndices(sudoku: Sudoku, x: Int): Set[Tuple2[Int,Int]] = {
    (0 to 8).map(n => (x,n)).toSet
  }

  def getAllSectionIndices(sudoku: Sudoku): Set[Set[Tuple2[Int,Int]]] = {
    val numSectionCols = sudoku.numCols / colsInASection
    val numSectionRows = sudoku.numRows / rowsInASection
    val xValues = (0 until numSectionCols)
    val yValues = (0 until numSectionRows)

    //TODO: Proper cross product
    var sectionIndices = Set[Set[Tuple2[Int,Int]]]()
    for (xVal <- xValues)
      for (yVal <- yValues)
        sectionIndices += getSectionIndices(sudoku, xVal, yVal)

    sectionIndices
  }

  def getSectionIndices(sudoku: Sudoku, sectionX: Int, sectionY: Int): Set[Tuple2[Int,Int]] = {
    val xValues = ((sectionX * colsInASection) until ((sectionX + 1) * colsInASection)).toSet
    val yValues = ((sectionY * rowsInASection) until ((sectionY + 1) * rowsInASection)).toSet

    //TODO: Proper cross product
    var indices = Set[Tuple2[Int,Int]]()
    for (xVal <- xValues)
      for (yVal <- yValues)
        indices += Tuple2[Int,Int](xVal,yVal)

    indices
  }

  def stepSolve(sudoku: Sudoku): Sudoku = {
    //First step - figure out which indexes we can solve immediately
    val possibleIndices = (0 until (sudoku.numRows * sudoku.numCols))
    val mappedIndices = possibleIndices.map(i => sudoku.coordOneDimensionToTwo(i))
    val possibleValues: Map[Tuple2[Int,Int],Set[Int]] =
      mappedIndices.map(index =>
        index -> getPossibleValuesForIndex(sudoku, index._1, index._2))(breakOut)

    //Indices where only one value is possible
    val singlePossibilitySolutions =
      possibleValues.filter(value => value._2.size == 1)

    //TODO: Indices who are the only possible index for a particular value

    val rawGrid = sudoku.grid.clone
    for (value <- singlePossibilitySolutions)
      rawGrid(sudoku.coordTwoDimensionToOne(value._1._1, value._1._2)) = value._2.head

    new Sudoku(rawGrid)
  }
}
