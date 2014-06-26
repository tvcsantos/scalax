package scalax.util

trait StackContext[T] {
  def add(e:T):Boolean
  
  def pushContext():StackContext[T]
  
  def popContext():Option[StackContext[T]]
  
  def clearContext()
  
  def isEmpty():Boolean
  
  def elements():Set[T]
  
  def allElements():Set[T]  
}

import scala.collection.mutable.{ 
  Set => MutableSet,
  HashSet => MutableHashSet
}

case class StackCtx[T](elems:MutableSet[T] = MutableHashSet[T](), 
      prev:Option[StackContext[T]] = None) extends StackContext[T] {
    
    def add(e:T):Boolean = {
      if (elems contains(e)) false
      else {
        elems += e
        true
      }
    }
    
    def pushContext():StackContext[T] = StackCtx(MutableSet(), Some(this))
    
    def popContext():Option[StackContext[T]] = prev
    
    def clearContext() = elems.clear
    
    def isEmpty():Boolean = 
      if (!elems.isEmpty) false
      else prev match {
        case None => true
        case Some(ctx) => ctx.isEmpty
      }
    
    def elements():Set[T] = 
      elems.toSet
      
    def allElements():Set[T] = {
      val res = MutableSet[T]()
      var pv:Option[StackContext[T]] = Some(this)
      var as:Set[T] = null
      do {
        val pvc = pv.get 
        as = pvc.elements
        res ++= as
        pv = pvc.popContext
      } while (pv != None)
      res.toSet
    }
  }