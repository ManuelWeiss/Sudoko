package io.weiss.sudoku

import com.typesafe.scalalogging.LazyLogging

object Solver extends LazyLogging {

  def validGrid(g: Vector[Vector[Int]]) =
    validRows(g) && validColumns(g) && validCells(g)

  def validRows(g: Vector[Vector[Int]]) =
    g forall noDoubleEntries

  def validColumns(g: Vector[Vector[Int]]) =
    validRows(g.transpose)

  def validCells(g: Vector[Vector[Int]]) = {
    val groupedRows = g grouped 3
    groupedRows forall { g =>
      val groupedCols = g.transpose grouped 3
      groupedCols forall { c =>
        noDoubleEntries(c.flatten)
      }
    }
  }

  def noDoubleEntries(v: Vector[Int]) = {
    val elements = nonZeroes(v)
    elements.distinct.size == elements.size
  }

  def solved(g: Vector[Vector[Int]]) =
    validGrid(g) && !(g.flatten contains 0)

  def gridToString(g: Vector[Vector[Int]]) =
    g.map(_.mkString(" ")).mkString("\n")

  def nextTry(g: Vector[Vector[Int]]) = {
    val (rowSize, row) = fullestRow(g)
    val (colSize, col) = fullestCol(g)
    val (cellSize, cellx, celly) = fullestCell(g)

    logger.debug(s"rowSize: $rowSize, colSize: $colSize, cellSize: $cellSize")

    if (rowSize > colSize)
      if (rowSize > cellSize) {
        val r = (row, g(row) indexWhere (_ == 0))
        logger.debug(s"row: $r")
        r
      } else {
        val c = freeEntryInCell(g, cellx, celly)
        logger.debug(s"cell: $c")
        c
      }
    else if (colSize > cellSize) {
      val c = (g.transpose.apply(col) indexWhere (_ == 0), col)
      logger.debug(s"col: $c")
      c
    }
    else {
      val c = freeEntryInCell(g, cellx, celly)
      logger.debug(s"cell: $c")
      c
    }
  }

  def freeEntryInCell(g: Vector[Vector[Int]], cellx: Int, celly: Int): (Int, Int) = {
    (0 to 2).map(_ + cellx * 3) foreach { x =>
      (0 to 2).map(_ + celly * 3) foreach { y =>
        if (g(x)(y) == 0)
          return (x, y)
      }
    }
    throw new IllegalStateException(s"no free entry in cell $cellx, $celly")
  }


  def fullestRow(g: Vector[Vector[Int]]) = {
    val rowsWithIndex = g.map(nonZeroes).map(_.size).zipWithIndex
    rowsWithIndex.filter(_._1 < 9).maxBy(_._1)
  }

  def fullestCol(g: Vector[Vector[Int]]) =
    fullestRow(g.transpose)

  def fullestCell(g: Vector[Vector[Int]]): (Int, Int, Int) = {
    val groupedRows = g grouped 3
    groupedRows.zipWithIndex flatMap { case (g, rowGroup) =>
      val groupedCols = g.transpose grouped 3
      groupedCols.zipWithIndex map { case (c, colGroup) =>
        (nonZeroes(c.flatten).size, rowGroup, colGroup)
      }
    } filter (_._1 < 9) maxBy (_._1)
  }

  def nonZeroes(v: Vector[Int]) =
    v.filter(_ != 0)

  def solve(g: Vector[Vector[Int]]): Option[Vector[Vector[Int]]] = {
    logger.debug(gridToString(g))
    if (solved(g)) {
      return Some(g)
    } else {
      val (i, j) = nextTry(g)
      logger.debug(s"next: $i, $j")
      (1 to 9) foreach { n =>
        logger.debug(s"n=$n")
        val updated = g.updated(i, g(i).updated(j, n))
        if (validGrid(updated)) {
          val res = solve(updated)
          if (res.nonEmpty)
            return res
        }
      }
    }
    None
  }
}
