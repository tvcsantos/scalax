package scalax.util

import scala.util.parsing.input.Positional

trait Pattern extends Positional {
  def getName(): String
}