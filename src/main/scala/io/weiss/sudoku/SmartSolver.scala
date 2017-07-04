package io.weiss.sudoku

import com.typesafe.scalalogging.LazyLogging

object SmartSolver extends LazyLogging {

  type Entry = Set[Int]

  def validGrid(g: Vector[Vector[Entry]]) =
    validRows(g) && validColumns(g) && validCells(g)

  def validRows(g: Vector[Vector[Entry]]) =
    g forall noDoubleEntries

  def validColumns(g: Vector[Vector[Entry]]) =
    validRows(g.transpose)

  def validCells(g: Vector[Vector[Entry]]) = {
    val groupedRows = g grouped 3
    groupedRows forall { r =>
      val groupedCols = r.transpose grouped 3
      groupedCols forall { c =>
        noDoubleEntries(c.flatten)
      }
    }
  }

  def noDoubleEntries(v: Vector[Entry]) = {
    val elements = fixedEntries(v)
    elements.distinct.size == elements.size
  }

  def fixedEntries(v: Vector[Entry]) =
    v.filter(_.size == 1)

  def solved(g: Vector[Vector[Entry]]) =
    validGrid(g) && (g.flatten forall(_.size == 1))

  def gridToString(g: Vector[Vector[Entry]]) =
    g.map(rowToString).mkString("\n")

  def rowToString(r: Vector[Entry]) =
    (r map { e: Entry =>
      if (e.size == 1)
        e.head
      else
        "0"
    }).mkString(",")

  def nextTry(g: Vector[Vector[Entry]]) = {
    val filtered = g.zipWithIndex flatMap { case (row, rowIdx) =>
      row.zipWithIndex map { case (entry, colIdx) =>
        (entry.size, rowIdx, colIdx)
      }
    } filter (_._1 > 1)
    if (filtered.isEmpty)
      None
    else {
      val (_, x, y) = filtered minBy (_._1)
      Some((x, y))
    }
  }

  def updateAllEntries(g: Vector[Vector[Entry]]): Vector[Vector[Entry]] = {
    val updated = g.zipWithIndex map { case (row, rowIdx) =>
        row.zipWithIndex map { case (entry, colIdx) =>
          allowedValues(g, rowIdx, colIdx)
        }
      }
    if (updated == g)
      updated
    else
      updateAllEntries(updated)
  }

  def allowedValues(g: Vector[Vector[Entry]], rowIdx: Int, colIdx: Int): Entry = {
    if (g(rowIdx)(colIdx).size == 1)
      g(rowIdx)(colIdx)
    else {
      val alreadyInNeighbours = alreadyInRow(g, rowIdx) ++ alreadyInCol(g, colIdx) ++ alreadyInCell(g, rowIdx, colIdx)
      g(rowIdx)(colIdx) -- alreadyInNeighbours
    }
  }

  def alreadyInRow(g: Vector[Vector[Entry]], rowIdx: Int) =
    g(rowIdx).filter(_.size == 1).fold(Set())((a, b) => a ++ b)

  def alreadyInCol(g: Vector[Vector[Entry]], colIdx: Int) =
    alreadyInRow(g.transpose, colIdx)

  def alreadyInCell(g: Vector[Vector[Entry]], rowIdx: Int, colIdx: Int) = {
    val cellx = rowIdx / 3
    val celly = colIdx / 3
    val entries = for {
      x <- (0 to 2).map(_ + cellx * 3)
      y <- (0 to 2).map(_ + celly * 3)
      if (g(x)(y)).size == 1
      } yield g(x)(y)

    entries.fold(Set())((a, b) => a ++ b)
  }

  def solve(g: Vector[Vector[Entry]]): Option[Vector[Vector[Entry]]] = {
    logger.debug(gridToString(g))
    if (solved(g)) {
      return Some(g)
    } else {
      val updated = updateAllEntries(g)
      if (solved(updated)) {
        return Some(updated)
      } else {
        nextTry(g) foreach { case (i, j) =>
          logger.debug(s"next: $i, $j")
          (1 to 9) foreach { n =>
            logger.debug(s"n=$n")
            val withNextTry = g.updated(i, g(i).updated(j, Set(n)))
            if (validGrid(withNextTry)) {
              val res = solve(withNextTry)
              if (res.nonEmpty)
                return res
            }
          }
        }
      }
    }
    None
  }
}
