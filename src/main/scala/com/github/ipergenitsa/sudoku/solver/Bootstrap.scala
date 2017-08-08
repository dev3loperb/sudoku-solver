package com.github.ipergenitsa.sudoku.solver

import com.github.ipergenitsa.sudoku.solver.Game.{Board, EmptyCell, ValueCell}

import scala.io.Source

object Bootstrap extends App {
  object const {
    val blockSize = 3
    val size = 9
  }

  val lines = Source.fromResource("input.txt").getLines().toSeq

  var board: Board = Map()
  for ((line, lineIndex) <- lines.zipWithIndex;
                          (symbol, symbolIndex) <- line.zipWithIndex) {
    val point = Game.Point(lineIndex, symbolIndex)
      if (symbol.asDigit == 0) {
        board = board + (point -> new EmptyCell)
      } else {
        board = board + (point -> new ValueCell(symbol.asDigit))
      }
    }

  val resolved = Game.resolve(board)

  Utils.printBoard(resolved)
}

object Game {
  case class Point(x: Int, y: Int)

  type Board = Map[Point, MultiLayerCell]

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
    resolveNotValues(fillInNotValues(fillInNotValuesByBlock(board)))
  }

  def fillInNotValuesByBlock(board: Board): Board = {
    var filledBoard: Board = board
    board.foreach { case (point, cell) =>
        cell match {
          case value: ValueCell =>
            val xBlock = point.x / Bootstrap.const.blockSize
            val yBlock = point.y / Bootstrap.const.blockSize
            for (i <- 0 until 3; j <- 0 until 3) {
              val key = Game.Point(xBlock * 3 + i, yBlock * 3 + j)
              filledBoard = filledBoard.updated(key, filledBoard(key).addNotValue(value.number))
            }
          case _ =>
        }
    }
    filledBoard
  }

  def fillInNotValues(board: Board): Board = {
    var filledBoard: Board = board
    board.foreach { case (point, cell) =>
      cell match {
        case value: ValueCell =>
          filledBoard = Utils.fillNotValue(filledBoard, value.number, point.x, point.y)
        case _ =>
      }
    }
    filledBoard
  }

  private def resolveNotValues(board: Board): Board = {
    var updatedBoard: Board = board
    for (i <- 0 until 9; j <- 0 until 9) {
      updatedBoard(Game.Point(i, j)) match {
        case emptyCell: EmptyCell =>
          if (emptyCell.notValues.size == 8) {
            val a: Int = (1 to 9).toSet.diff(emptyCell.notValues).head
            updatedBoard = updatedBoard.updated(Game.Point(i, j), new ValueCell(a))
          }
        case _ =>
      }
    }
    updatedBoard
  }
}

object Utils {
  def fillNotValue(board: Board, value: Int, x: Int, y: Int): Board = {
    fillAux(board, value, x, y, 0)
  }

  private def fillAux(board: Board, value: Int, x: Int, y: Int, step: Int): Board = {
    if (step > Bootstrap.const.size - 1 || step < 0) {
      board
    } else {
      val xKey = Game.Point(x, step)
      val yKey = Game.Point(step, y)
      val updatedBoard = board.updated(xKey, board(xKey).addNotValue(value))
        .updated(yKey, board(yKey).addNotValue(value))
      fillAux(updatedBoard, value, x, y, step + 1)
    }
  }

  def printBoard(board: Board): Unit = {
    for (i <- 0 until Bootstrap.const.size) {
      val row = (0 until Bootstrap.const.size).map(j => board(Game.Point(i, j)))
      println(row.map(_.getValue).mkString(""))
    }
    val emptyCells = board.count {_._2 match {
      case _: EmptyCell => true
      case _ => false
    }}
    println(emptyCells)
  }
}
