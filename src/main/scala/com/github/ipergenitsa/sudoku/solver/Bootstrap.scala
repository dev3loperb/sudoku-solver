package com.github.ipergenitsa.sudoku.solver

import com.github.ipergenitsa.sudoku.solver.Solver.Board

import scala.io.Source

object Bootstrap extends App {
  val lines = Source.fromResource("input.txt").getLines().toSeq

  var board: Board = Utils.createBoard(lines.map(_.map(_.asDigit)))
  val resolved = Solver.resolve(board)

  Utils.printBoard(resolved)
}
