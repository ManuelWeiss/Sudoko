package io.weiss.sudoku

object SudokuApp {
  def main(args: Array[String]) = {
    val input = Vector(
      Vector(0, 0, 6, 0, 5, 0, 7, 0, 3),
      Vector(9, 7, 0, 4, 6, 0, 0, 0, 0),
      Vector(0, 0, 3, 0, 0, 0, 6, 9, 0),
      Vector(5, 4, 0, 2, 1, 6, 0, 0, 0),
      Vector(0, 0, 1, 5, 0, 7, 2, 0, 0),
      Vector(0, 0, 0, 3, 4, 8, 0, 1, 7),
      Vector(0, 6, 8, 0, 0, 0, 1, 0, 0),
      Vector(0, 0, 0, 0, 2, 1, 0, 3, 8),
      Vector(3, 0, 4, 0, 7, 0, 9, 0, 0)
    )

    println("input:")
    println(SimpleSolver.gridToString(input))

    SimpleSolver.solve(input) match {
      case Some(g) =>
        println("found solution:")
        println(SimpleSolver.gridToString(g))
      case None => print("no solution :(")
    }
  }
}
