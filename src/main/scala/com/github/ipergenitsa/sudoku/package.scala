package com.github.ipergenitsa

import com.github.ipergenitsa.sudoku.gamefield.Cell

package object sudoku {
  case class Point(x: Int, y: Int)
  type Board = Map[Point, Cell]
}
