package scalax.util

import scala.util.parsing.input.Positional

case class Positioned[T](val elem:T) extends Positional