package com.github.ipergenitsa.sudoku.solver

import com.github.ipergenitsa.sudoku.solver.Solver.Board

object Solver {
  object Const {
    val blockSize = 3
    val size = 9
  }

  case class Point(x: Int, y: Int)

  type Board = Map[Point, Cell]

  def resolve(board: Board): Board = {
    var currentBoard = board
    var nextBoard = nextStep(board)
    while (!currentBoard.equals(nextBoard)) {
      currentBoard = nextBoard
      nextBoard = nextStep(nextBoard)
    }
    currentBoard
  }

  def nextStep(board: Board): Board = {
    resolveNotValuesByEmptyCells(fillInNotValues(fillInNotValuesByBlock(resolveNotValues(board))))
  }

  def fillInNotValuesByBlock(board: Board): Board = {
    var filledBoard: Board = board
    board.foreach { case (point, cell) =>
      cell match {
        case value: ValueCell =>
          val xBlock = point.x / Solver.Const.blockSize
          val yBlock = point.y / Solver.Const.blockSize
          for (i <- 0 until 3; j <- 0 until 3) {
            val key = Solver.Point(xBlock * 3 + i, yBlock * 3 + j)
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
    board.foreach { case (point, cell) =>
      cell match {
        case emptyCell: EmptyCell =>
          if (emptyCell.notValues.size == 8) {
            val valueToInsert = (1 to 9).toSet.diff(emptyCell.notValues).head
            updatedBoard = fillInNotValues(fillInNotValuesByBlock(updatedBoard.updated(point, new ValueCell(valueToInsert))))
          }
        case _ =>
      }
    }
    updatedBoard
  }

  private def resolveNotValuesByEmptyCells(board: Board): Board = {
    var updatedBoard = board
    for (i <- 0 until Solver.Const.size) {
      val row = (0 until Solver.Const.size).map { j =>
        val key = Solver.Point(i, j)
        key -> updatedBoard(key)
      }
      resolveNotValuesByEmptyCellsAux(row).foreach { itemToInsert =>
        updatedBoard = fillInNotValues(fillInNotValuesByBlock(updatedBoard.updated(itemToInsert._2._1, new ValueCell(itemToInsert._1))))
      }

      val column = (0 until Solver.Const.size).map { j =>
        val key = Solver.Point(j, i)
        key -> updatedBoard(key)
      }
      resolveNotValuesByEmptyCellsAux(column).foreach { itemToInsert =>
        updatedBoard = fillInNotValues(fillInNotValuesByBlock(updatedBoard.updated(itemToInsert._2._1, new ValueCell(itemToInsert._1))))
      }
    }
    for (i <- 0 until 3; j <- 0 until 3) {
      val block = for {
        k <- 0 until 3
        n <- 0 until 3
        x = 3 * i + k
        y = 3 * j + n
        key = Point(x, y)
      } yield key -> updatedBoard(key)

      resolveNotValuesByEmptyCellsAux(block).foreach { itemToInsert =>
        updatedBoard = fillInNotValues(fillInNotValuesByBlock(updatedBoard.updated(itemToInsert._2._1, new ValueCell(itemToInsert._1))))
      }
    }
    updatedBoard
  }

  private def resolveNotValuesByEmptyCellsAux(cells: Seq[(Point, Cell)]): Seq[(Int, (Point, Cell))] = {
    val emptyCells = cells.filterNot(_._2.hasValue)
    (1 to 9).map { i =>
      i -> emptyCells.filterNot(_._2.getNonValues.contains(i))
    }.filter(_._2.size == 1).map(item => item._1 -> item._2.head)
  }
}

object Utils {
  def fillNotValue(board: Board, value: Int, x: Int, y: Int): Board = {
    fillAux(board, value, x, y, 0)
  }

  private def fillAux(board: Board, value: Int, x: Int, y: Int, step: Int): Board = {
    if (step > Solver.Const.size - 1 || step < 0) {
      board
    } else {
      val xKey = Solver.Point(x, step)
      val yKey = Solver.Point(step, y)
      val updatedBoard = board.updated(xKey, board(xKey).addNotValue(value))
        .updated(yKey, board(yKey).addNotValue(value))
      fillAux(updatedBoard, value, x, y, step + 1)
    }
  }

  def valueToCell(value: Int): Cell = {
    value match {
      case 0 => new EmptyCell
      case notEmpty => new ValueCell(notEmpty)
    }
  }

  def createBoard(input: Seq[Seq[Int]]): Board = {
    def rowToBoard(row: Seq[Int], rowIndex: Int): Board = {
      row.zipWithIndex.map {
        case (element, index) => Solver.Point(rowIndex, index) -> element
      }.foldLeft(Map(): Board)((board: Board, pointToElement) => {
          board.updated(pointToElement._1, valueToCell(pointToElement._2))
      })
    }
    input.zipWithIndex
      .foldLeft(Map(): Board)((board, seq) => board ++ rowToBoard(seq._1, seq._2))
  }

  def printBoard(board: Board): Unit = {
    for (i <- 0 until Solver.Const.size) {
      val row = (0 until Solver.Const.size).map(j => board(Solver.Point(i, j)))
      println(row.map(_.getValue).mkString(""))
    }
    val emptyCells = board.count {_._2 match {
      case _: EmptyCell => true
      case _ => false
    }}
    println(emptyCells)
  }
}
