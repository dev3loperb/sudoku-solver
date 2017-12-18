package com.github.ipergenitsa.sudoku.solver

import com.github.ipergenitsa.sudoku.Board
import com.github.ipergenitsa.sudoku.gamefield.GameFieldUtils

import scala.io.Source

object Bootstrap extends App {
  val lines = Source.fromResource("input2.txt").getLines().toSeq

  var board: Board = GameFieldUtils.createBoard(lines.map(_.map(_.asDigit)))
  val resolved = Solver.resolve(board)

  GameFieldUtils.printBoard(resolved)
}
