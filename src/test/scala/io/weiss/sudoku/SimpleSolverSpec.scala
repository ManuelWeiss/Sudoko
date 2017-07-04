package io.weiss.sudoku

import org.scalatest._

class SimpleSolverSpec extends FlatSpec with Matchers {

  val validTestGrid = Vector(
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

  "The solver" should "accept valid rows" in {
    SimpleSolver.validRows(validTestGrid) shouldBe true
  }

  it should "reject invalid rows" in {
    val invalidRowsGrid = Vector(
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(1, 1, 0, 0, 0, 0, 0, 0, 0)
    )
    SimpleSolver.validRows(invalidRowsGrid) shouldBe false
  }

  it should "accept valid columns" in {
    SimpleSolver.validColumns(validTestGrid) shouldBe true
  }

  it should "reject invalid columns" in {
    val invalidColumnsGrid = Vector(
      Vector(0, 1, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 1, 0, 0, 0, 0, 0, 0, 0)
    )
    SimpleSolver.validColumns(invalidColumnsGrid) shouldBe false
  }

  it should "accept valid cells" in {
    SimpleSolver.validCells(validTestGrid) shouldBe true
  }

  it should "reject invalid cells" in {
    val invalidCellsGrid = Vector(
      Vector(0, 1, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(1, 0, 0, 0, 0, 0, 0, 0, 0)
    )
    val invalidCellsGrid2 = Vector(
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0),
      Vector(0, 1, 0, 2, 0, 3, 0, 0, 0),
      Vector(2, 0, 0, 0, 3, 0, 0, 0, 0),
      Vector(0, 3, 0, 0, 0, 4, 0, 0, 0)
    )

    SimpleSolver.validCells(invalidCellsGrid) shouldBe false
    SimpleSolver.validCells(invalidCellsGrid2) shouldBe false
  }

  it should "find the fullest row in the grid" in {
    SimpleSolver.fullestRow(validTestGrid) shouldBe (5, 3)
  }

  it should "find the fullest column in the grid" in {
    SimpleSolver.fullestCol(validTestGrid) shouldBe (6, 4)
  }

  it should "find the fullest cell in the grid" in {
    SimpleSolver.fullestCell(validTestGrid) shouldBe (8, 1, 1)
  }

  it should "find a free entry in cells" in {
    SimpleSolver.freeEntryInCell(validTestGrid, 0, 0) shouldBe (0, 0)
    SimpleSolver.freeEntryInCell(validTestGrid, 1, 1) shouldBe (4, 4)
    SimpleSolver.freeEntryInCell(validTestGrid, 0, 2) shouldBe (0, 7)
    SimpleSolver.freeEntryInCell(validTestGrid, 2, 2) shouldBe (6, 7)
  }

  it should "recognise an imcomplete grid" in {
    SimpleSolver.solved(validTestGrid) shouldBe false
  }

  it should "recognise a solved grid" in {
    val solvedGrid =  Vector(
      Vector(4, 1, 5),
      Vector(2, 6, 7),
      Vector(8, 3, 9)
    )
    SimpleSolver.solved(solvedGrid) shouldBe true
  }

  it should "solve a sudoku" in {
    SimpleSolver.solved(SimpleSolver.solve(validTestGrid).get) shouldBe true
  }
}
