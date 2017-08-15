package scalax.util

import scala.util.parsing.input.Positional
import scalax.ttype.Type

trait Variable extends Positional {
  def getName():String
}

trait IndexedVariable extends Variable {
  def getIdx():Int
}

trait TypeVariable extends Type with Variable