import org.scalatest.FlatSpec

class SudokuSpec extends FlatSpec {
    val puzzle = Array(
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
    val sudoku = new Sudoku(puzzle)

  "A Sudoku puzzle" should "properly convert a grid to the correct string output" in {
        val expectedOutput = 
"""006|007|300
018|009|050
500|000|064
-----------
920|080|000
000|763|000
000|090|075
-----------
630|000|008
090|300|520
002|400|600"""

    assert(sudoku.toString == expectedOutput)
  }

  it should "correctly convert indices from one to two dimensions" in {
    val initialValue = 10
    val calculatedValue = sudoku.coordOneDimensionToTwo(initialValue)
    val expectedValue = (1,1)
    assert(calculatedValue == expectedValue)
  }

  it should "correctly convert indices from two to one dimension" in {
    val initialValue = (5, 4)
    val calculatedValue = sudoku.coordTwoDimensionToOne(initialValue._1, initialValue._2)
    val expectedValue = 41
    assert(calculatedValue == expectedValue)
  }

  it should "return the correct cell value at a given index" in {
     assert(sudoku.getValueAtIndex(0,0) == 0)
     assert(sudoku.getValueAtIndex(0,2) == 5)
     assert(sudoku.getValueAtIndex(7,7) == 2)
  }

  //it should "produce NoSuchElementException when head is invoked" in {
    //intercept[NoSuchElementException]{
      //Set.empty.head
    //}
  //}
}
