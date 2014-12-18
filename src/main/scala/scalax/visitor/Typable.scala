package scalax.visitor

import scalax.util.Environment

trait Typable[T, U <: Type] {  
  def isTyped():Boolean
  
  import scalax.util.Environment  
  
  def typeCheck[L](c:TypeChecker[T, U, L], e:(Environment[String, U], L)):T
  
  def getType():Option[U]
}
