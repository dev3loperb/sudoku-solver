package com.github.ipergenitsa.sudoku.gamefield

import com.github.ipergenitsa.sudoku.solver.Solver
import com.github.ipergenitsa.sudoku.{Board, Point}

trait GameField {

}

object GameFieldUtils {
  def createBoard(input: Seq[Seq[Int]]): Board = {
    def rowToBoard(row: Seq[Int], rowIndex: Int): Board = {
      row.zipWithIndex.map {
        case (element, index) => Point(rowIndex, index) -> element
      }.foldLeft(Map(): Board)((board: Board, pointToElement) => {
        board.updated(pointToElement._1, CellUtils.valueToCell(pointToElement._2))
      })
    }
    input.zipWithIndex
      .foldLeft(Map(): Board)((board, seq) => board ++ rowToBoard(seq._1, seq._2))
  }

  def printBoard(board: Board): Unit = {
    for (i <- 0 until Solver.Const.size) {
      val row = (0 until Solver.Const.size).map(j => board(Point(i, j)))
      println(row.map(_.getValue).mkString(""))
    }
    val emptyCells = board.count {_._2 match {
      case _: EmptyCell => true
      case _ => false
    }}
    println(emptyCells)
  }
}