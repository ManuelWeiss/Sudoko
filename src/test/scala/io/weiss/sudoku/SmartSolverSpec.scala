package io.weiss.sudoku

import org.scalatest._
import io.weiss.sudoku.Utils._

class SmartSolverSpec extends FlatSpec with Matchers {

  val validTestGrid = gridFromVectors(Vector(
    Vector(0, 0, 6, 0, 5, 0, 7, 0, 3),
    Vector(9, 7, 0, 4, 6, 0, 0, 0, 0),
    Vector(0, 0, 3, 0, 0, 0, 6, 9, 0),
    Vector(5, 4, 0, 2, 1, 6, 0, 0, 0),
    Vector(0, 0, 1, 5, 0, 7, 2, 0, 0),
    Vector(0, 0, 0, 3, 4, 8, 0, 1, 7),
    Vector(0, 6, 8, 0, 0, 0, 1, 0, 0),
    Vector(0, 0, 0, 0, 2, 1, 0, 3, 8),
    Vector(3, 0, 4, 0, 7, 0, 9, 0, 0)
  ))

  "The solver" should "accept valid rows" in {
    SmartSolver.validRows(validTestGrid) shouldBe true
  }

  it should "be able to convert input" in {
    val testInput =
      """  6| 5 |7 3|
        |97 |46 |   |
        |  3|   |69 |
        |54 |216|   |
        |  1|5 7|2  |
        |   |348| 17|
        | 68|   |1  |
        |   | 21| 38|
        |3 4| 7 |9  |""".stripMargin

    gridFromString(testInput) shouldBe validTestGrid
  }

  it should "reject invalid rows" in {
    val invalidRowsGrid = gridFromVectors(Vector(
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(1, 1, 0, 0, 0, 0, 0, 0, 0)
    ))
    SmartSolver.validRows(invalidRowsGrid) shouldBe false
  }

  it should "accept valid columns" in {
    SmartSolver.validColumns(validTestGrid) shouldBe true
  }

  it should "reject invalid columns" in {
    val invalidColumnsGrid = gridFromVectors(Vector(
      Vector(0, 1, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 1, 0, 0, 0, 0, 0, 0, 0)
    ))
    SmartSolver.validColumns(invalidColumnsGrid) shouldBe false
  }

  it should "accept valid cells" in {
    SmartSolver.validCells(validTestGrid) shouldBe true
  }

  it should "reject invalid cells" in {
    val invalidCellsGrid = gridFromVectors(Vector(
      Vector(0, 1, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(1, 0, 0, 0, 0, 0, 0, 0, 0)
    ))
    val invalidCellsGrid2 = gridFromVectors(Vector(
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 1, 0, 2, 0, 3, 0, 0, 0),
      Vector(2, 0, 0, 0, 3, 0, 0, 0, 0),
      Vector(0, 3, 0, 0, 0, 4, 0, 0, 0)
    ))

    SmartSolver.validCells(invalidCellsGrid) shouldBe false
    SmartSolver.validCells(invalidCellsGrid2) shouldBe false
  }

  it should "recognise an imcomplete grid" in {
    SmartSolver.solved(validTestGrid) shouldBe false
  }

  it should "recognise a solved grid" in {
    val solvedGrid =  gridFromVectors(Vector(
      Vector(4, 1, 5),
      Vector(2, 6, 7),
      Vector(8, 3, 9)
    ))
    SmartSolver.solved(solvedGrid) shouldBe true
  }

  it should "find existing entries in a row" in {
    SmartSolver.alreadyInRow(validTestGrid, 0) shouldBe Set(3, 5, 6, 7)
    SmartSolver.alreadyInRow(validTestGrid, 3) shouldBe Set(1, 2, 4, 5, 6)
  }

  it should "find existing entries in a column" in {
    SmartSolver.alreadyInCol(validTestGrid, 0) shouldBe Set(3, 5, 9)
    SmartSolver.alreadyInCol(validTestGrid, 4) shouldBe Set(1, 2, 4, 5, 6, 7)
  }

  it should "find existing entries in a cell" in {
    SmartSolver.alreadyInCell(validTestGrid, 0, 0) shouldBe Set(3, 6, 7, 9)
    SmartSolver.alreadyInCell(validTestGrid, 4, 4) shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8)
  }

  it should "update all allowed values" in {
    SmartSolver.updateAllEntries(validTestGrid) shouldBe gridFromVectors(
      Vector(Vector(4, 8, 6, 1, 5, 9, 7, 2, 3),
             Vector(9, 7, 2, 4, 6, 3, 8, 5, 1),
             Vector(1, 5, 3, 7, 8, 2, 6, 9, 4),
             Vector(5, 4, 7, 2, 1, 6, 3, 8, 9),
             Vector(8, 3, 1, 5, 9, 7, 2, 4, 6),
             Vector(6, 2, 9, 3, 4, 8, 5, 1, 7),
             Vector(2, 6, 8, 9, 3, 4, 1, 7, 5),
             Vector(7, 9, 5, 6, 2, 1, 4, 3, 8),
             Vector(3, 1, 4, 8, 7, 5, 9, 6, 2))
    )
  }

  it should "update all allowed values in an underdefined grid" in {
    val underdefinedGrid = gridFromVectors(Vector(
      Vector(0, 0, 6, 0, 5, 0, 7, 0, 3),
      Vector(0, 7, 0, 4, 0, 0, 0, 0, 0),
      Vector(0, 0, 3, 0, 0, 0, 6, 9, 0),
      Vector(5, 4, 0, 2, 1, 6, 0, 0, 0),
      Vector(0, 0, 1, 5, 0, 7, 2, 0, 0),
      Vector(0, 0, 0, 3, 4, 8, 0, 1, 7),
      Vector(0, 6, 8, 0, 0, 0, 1, 0, 0),
      Vector(0, 0, 0, 0, 2, 1, 0, 3, 8),
      Vector(3, 0, 4, 0, 7, 0, 9, 0, 0)
    ))
    SmartSolver.updateAllEntries(underdefinedGrid) shouldBe Vector(
      Vector(Set(2,8,4), Set(2,8), Set(6),     Set(1), Set(5), Set(9), Set(7), Set(2,4),   Set(3)),
      Vector(Set(1,9,2), Set(7),   Set(5,9,2), Set(4), Set(6), Set(3), Set(8), Set(5,2),   Set(5,1,2)),
      Vector(Set(1,4),   Set(5,1), Set(3),     Set(7), Set(8), Set(2), Set(6), Set(9),     Set(5,1,4)),
      Vector(Set(5),     Set(4),   Set(7),     Set(2), Set(1), Set(6), Set(3), Set(8),     Set(9)),
      Vector(Set(6,8),   Set(3,8), Set(1),     Set(5), Set(9), Set(7), Set(2), Set(6,4),   Set(6,4)),
      Vector(Set(6,9,2), Set(9,2), Set(9,2),   Set(3), Set(4), Set(8), Set(5), Set(1),     Set(7)),
      Vector(Set(2,7),   Set(6),   Set(8),     Set(9), Set(3), Set(4), Set(1), Set(5,2,7), Set(5,2)),
      Vector(Set(9,7),   Set(5,9), Set(5,9),   Set(6), Set(2), Set(1), Set(4), Set(3),     Set(8)),
      Vector(Set(3),     Set(1,2), Set(4),     Set(8), Set(7), Set(5), Set(9), Set(6,2),   Set(6,2))
    )
  }

  it should "find next entry to process" in {
    val all = (1 to 9).toSet
    val row = Vector.fill(9)(all)
    val x = 3
    val y = 5
    val grid = Vector.fill(9)(row).updated(x, row.updated(y, Set(1,2,3)))
    SmartSolver.nextTry(grid) shouldBe Some((x, y))
  }

  it should "solve a sudoku" in {
    SmartSolver.solve(validTestGrid).get shouldBe gridFromVectors(
      Vector(Vector(4, 8, 6, 1, 5, 9, 7, 2, 3),
             Vector(9, 7, 2, 4, 6, 3, 8, 5, 1),
             Vector(1, 5, 3, 7, 8, 2, 6, 9, 4),
             Vector(5, 4, 7, 2, 1, 6, 3, 8, 9),
             Vector(8, 3, 1, 5, 9, 7, 2, 4, 6),
             Vector(6, 2, 9, 3, 4, 8, 5, 1, 7),
             Vector(2, 6, 8, 9, 3, 4, 1, 7, 5),
             Vector(7, 9, 5, 6, 2, 1, 4, 3, 8),
             Vector(3, 1, 4, 8, 7, 5, 9, 6, 2))
    )
  }
}
