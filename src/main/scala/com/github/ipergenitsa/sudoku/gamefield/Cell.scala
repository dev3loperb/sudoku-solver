package com.github.ipergenitsa.sudoku.gamefield

trait Cell {
  def addNotValue(notValue: Int): Cell
  def getValue: String
  def getNonValues: Set[Int]
  def hasValue: Boolean
}

class EmptyCell(val notValues: Set[Int] = Set()) extends Cell {
  override def addNotValue(notValue: Int): Cell = new EmptyCell(notValues + notValue)
  override def getValue: String = "*"
  override def getNonValues: Set[Int] = notValues
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: EmptyCell => notValues.equals(that.notValues)
    case _ => false
  }

  override def toString: String = "*"
  val hasValue = false
}

class ValueCell(val number: Int, val notValues: Set[Int] = Set()) extends Cell {
  override def addNotValue(notValue: Int) = new ValueCell(number, notValues + notValue)
  override def getValue: String = number.toString
  override def getNonValues: Set[Int] = notValues
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ValueCell => number.equals(that number) && notValues.equals(that notValues)
    case _ => false
  }
  override def toString: String = number.toString
  val hasValue = true
}

object CellUtils {
  def valueToCell(value: Int): Cell = {
    value match {
      case 0 => new EmptyCell
      case notEmpty => new ValueCell(notEmpty)
    }
  }
}