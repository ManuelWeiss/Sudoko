package io.weiss.sudoku

object Utils {

  def gridFromVectors(g: Vector[Vector[Int]]) =
    g.map(_ map {
      case 0 => (1 to 9).toSet
      case x => Set(x)
    })

  def gridFromString(s: String) =
    s.split("\n").take(9).map(rowToVectorOfSets).toVector

  def rowToVectorOfSets(s: String) =
    s.filter(x => " 0123456789".contains(x))
      .take(9)
      .map {
        case ' ' | '0' => (1 to 9).toSet
        case x => Set(s"$x".toInt)
      }

}
