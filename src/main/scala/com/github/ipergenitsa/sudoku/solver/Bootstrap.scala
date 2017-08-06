package com.github.ipergenitsa.sudoku.solver

import com.github.ipergenitsa.sudoku.solver.Game.{Board, EmptyCell, ValueCell}

import scala.io.Source

object Bootstrap extends App {
  val width = 9
  val height = 9

  val lines = Source.fromResource("input.txt").getLines().toSeq

  val board: Board = for (line <- lines) yield
    for (symbol <- line) yield {
      if (symbol.asDigit == 0) {
        new EmptyCell
      } else {
        new ValueCell(symbol.asDigit)
      }
    }

  val resolved = Game.resolve(board)

  resolved.zipWithIndex.foreach {
    case (row, rIndex) =>
      row.zipWithIndex.foreach {
        case (cell, cIndex) =>
          println(s"cell: $rIndex; $cIndex. Value: ${cell.getValue}. Non Values: ${cell.getNonValues}")
      }
  }

  Utils.printBoard(resolved)
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

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: EmptyCell => notValues.equals(that.notValues)
      case _ => false
    }
  }
  class ValueCell(val number: Int, val notValues: Set[Int] = Set()) extends MultiLayerCell {
    override def addNotValue(notValue: Int) = new ValueCell(number, notValues + notValue)
    override def getValue: String = number.toString
    override def getNonValues: Set[Int] = notValues

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ValueCell => number.equals(that number) && notValues.equals(that notValues)
      case _ => false
    }
  }

  def resolve(board: Board): Board = {
    var nextBoard = nextStep(board)
    while (!nextStep(nextBoard).equals(nextBoard)) {
      nextBoard = nextStep(nextBoard)
    }
    nextBoard
  }

  def nextStep(board: Board): Board = {
    resolveNotValues(fillInNotValues(board))
  }

  def fillInNotValues(board: Board): Board = {
    var filledBoard: Board = board
    for (i <- 0 until 9; j <- 0 until 9) {
      filledBoard(i)(j) match {
        case value: ValueCell =>
          filledBoard = Utils.fill(filledBoard, value.number, i, j)
        case _ =>
      }
    }
    filledBoard
  }

  private def resolveNotValues(board: Board): Board = {
    var updatedBoard: Board = board
    for (i <- 0 until 9; j <- 0 until 9) {
      updatedBoard(i)(j) match {
        case emptyCell: EmptyCell =>
          if (emptyCell.notValues.size == 8) {
            val a: Int = (1 to 9).toSet.diff(emptyCell.notValues).head
            updatedBoard = updatedBoard.updated(i, updatedBoard(i).updated(j, new ValueCell(a)))
          }
        case _ =>
      }
    }
    updatedBoard
  }
}

object Utils {
  def fill(board: Board, value: Int, x: Int, y: Int): Board = {
    fillRow(board, value, x).map(row => {
      row.updated(y, row(y).addNotValue(value))
    })
  }

  private def fillRow(board: Board, value: Int, x: Int): Board = {
    board.updated(x, board(x).map(_.addNotValue(value)))
  }

  def printBoard(board: Board): Unit = {
    board.foreach(row => println(row.map(_.getValue).mkString("")))
  }
}
