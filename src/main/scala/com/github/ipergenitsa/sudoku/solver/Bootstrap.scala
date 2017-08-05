package com.github.ipergenitsa.sudoku.solver

import scala.io.Source
import scala.util.Try

object Bootstrap extends App {
  val width = 9
  val height = 9

  val lines = Source.fromResource("input.txt").getLines()

  val board = for ((line) <- lines) yield
    for (symbol <- line) yield Try(symbol.asDigit).toOption.filter(_ != 0)

  board.foreach(row => println(row.map(_.getOrElse("*")).mkString))
}

object Game {
  trait MultiLayerCell
  case class NumberCell(number: Int)
  case object EmptyCell
  type Board = Seq[Seq[Option[MultiLayerCell]]]
}
