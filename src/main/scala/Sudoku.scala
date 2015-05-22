object Sudoku {
  def main(args: Array[String]) = {
    val state = new Sudoku
    println(state.toString())
    println("Eventually this will do stuff with Sudoku Puzzles.")
  }
}

class Sudoku {

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

  override def toString(): String = {
    "Betcha wish this was a sudoku"
  }
}
