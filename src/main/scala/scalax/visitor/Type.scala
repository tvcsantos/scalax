package scalax.visitor

import scala.util.parsing.input.Positional

trait Type extends Positional {  
  def isBaseType():Boolean
  
  def getRefinedType():Type
}