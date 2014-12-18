package scalax.util

import scala.util.parsing.input.Positional

trait Variable extends Positional {
  def getName():String
}

trait IndexedVariable extends Variable {
  def getIdx():Int
}

import scalax.visitor.Type

trait TypeVariable extends Type with Variable