import org.scalatest.FlatSpec

class HanzoSpec extends FlatSpec {
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
    val hanzo = new Hanzo()

  "Hanzo" should "correctly identify the neighbors of a given index" in {
    val calculatedNeighbors = hanzo.getNeighborIndices(2, 5)
    val expectedNeighbors =
      Set(
        (0,5),(1,5),(3,5),(4,5),(5,5),(6,5),(7,5),(8,5),//Row neighbors
        (2,0),(2,1),(2,2),(2,3),(2,4),(2,6),(2,7),(2,8),//Column neighbors
        (0,3),(1,3),(0,4),(1,4)//Section neighbors
      )
    //println(calculatedNeighbors &~ expectedNeighbors)
    assert(calculatedNeighbors == expectedNeighbors)
  }

  it should "identify the possible values for a given index" in {
    val calculatedValues = hanzo.getPossibleValuesForIndex(sudoku,0,0)
    val expectedValues = Set(2,4)
    assert(calculatedValues == expectedValues)
  }
}
