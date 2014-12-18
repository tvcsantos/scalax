package scalax.visitor

trait Typed[U <: Type] {  
  def isTyped():Boolean
    
  def getType():Option[U]
}