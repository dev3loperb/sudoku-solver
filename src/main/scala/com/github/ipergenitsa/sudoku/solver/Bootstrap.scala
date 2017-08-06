package com.github.ipergenitsa.sudoku.solver

import com.github.ipergenitsa.sudoku.solver.Game.{Board, EmptyCell, ValueCell}

import scala.io.Source

object Bootstrap extends App {
  val width = 9
  val height = 9

  val lines = Source.fromResource("input.txt").getLines().toSeq

  var board: Board = for (line <- lines) yield
    for (symbol <- line) yield {
      if (symbol.asDigit == 0) {
        new EmptyCell
      } else {
        new ValueCell(symbol.asDigit)
      }
    }
  for (k <- 0 until 10) {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        board(i)(j) match {
          case value: ValueCell =>
            board = Utils.fill(board, value.number, i, j)
          case _ =>
        }
      }
    }

    for (i <- 0 until width) {
      for (j <- 0 until height) {
        board(i)(j) match {
          case emptyCell: EmptyCell =>
            if (emptyCell.notValues.size == 8) {
              val a: Int = (1 to 9).toSet.diff(emptyCell.notValues).head
              board = board.updated(i, board(i).updated(j, new ValueCell(a)))
            }
          case _ =>
        }
      }
    }
  }

  board.zipWithIndex.foreach {
    case (row, rIndex) => {
      row.zipWithIndex.foreach {
        case (cell, cIndex) => {
          println(s"cell: $rIndex; $cIndex. Value: ${cell.getValue}. Non Values: ${cell.getNonValues}" )
        }
      }
    }
  }

  Utils.printBoard(board)
}

object Game {
  type Board = Seq[Seq[MultiLayerCell]]
  trait MultiLayerCell {
    def addNotValue(notValue: Int): MultiLayerCell
    def getValue: String
    def getNonValues: Set[Int]
  }
  class EmptyCell(val notValues: Set[Int] = Set()) extends MultiLayerCell {
    override def addNotValue(notValue: Int): MultiLayerCell = new EmptyCell(notValues + notValue)
    override def getValue: String = "*"
    override def getNonValues: Set[Int] = notValues
  }
  class ValueCell(val number: Int, val notValues: Set[Int] = Set()) extends MultiLayerCell {
    override def addNotValue(notValue: Int) = new ValueCell(number, notValues + notValue)
    override def getValue: String = number.toString
    override def getNonValues: Set[Int] = notValues
  }
}

object Utils {
  def fill(board: Board, value: Int, x: Int, y: Int) = {
    var updatedBoard = board.updated(x, board(x).zipWithIndex.map { case (cell, index) => {
      if (index != y) {
        cell.addNotValue(value)
      } else {
        cell
      }
    }
    })
    for (i <- 0 until 9; if i != y) {
      updatedBoard = updatedBoard.updated(i, updatedBoard(i).updated(y, updatedBoard(i)(y).addNotValue(value)))
    }
    updatedBoard
  }

  def printBoard(board: Board): Unit = {
    board.foreach(row => println(row.map(_.getValue).mkString(" ")))
  }
}
