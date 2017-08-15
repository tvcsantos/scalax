package scalax.ttype

import scalax.visitor.TypeChecker

trait Typable[T, U <: Type] {  
  def isTyped():Boolean
  
  import scalax.util.Environment
  
  def typeCheck[L](c:TypeChecker[T, U, L], e:(Environment[String, U], L)):T
  
  def getType():Option[U]
}
