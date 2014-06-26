package scalax.util

trait Environment[T] {
  
  def assoc(id:String, t:T):Boolean
    
  def assoc(elem:(String, T)):Boolean
  
  def beginScope():Environment[T]
  
  def endScope():Option[Environment[T]]
  
  def find(id:String):Option[T]
}

trait SeqEnvironment[T] extends Environment[T] {
  
  def findIdx(id:String):Option[Int]
  
  def findPair(id:String):Option[(T, Int)]
  
  def beginScope():SeqEnvironment[T]
  
  def endScope():Option[SeqEnvironment[T]]
  
  def getSize():Int
}

import scala.collection.mutable.{ 
  Map => MutableMap,
  HashMap => MutableHashMap
}

/*case class Env[T](assocs:MutableMap[String, T] = MutableHashMap[String, T](), 
    prev:Option[Environment[T]] = None) extends Environment[T] {
  
  def assoc(id:String, t:T) = {
    if (assocs contains(id)) false
    else {
      assocs put(id, t)
      true
    }
  }
    
  def assoc(elem:(String, T)) = assoc(elem._1, elem._2)
    
  def beginScope():Environment[T] = 
    Env(MutableMap[String, T](), Some(this))
        
  def endScope():Option[Environment[T]] = prev
    
  def find(id:String):Option[T] = {
    val fthis = assocs get id
    fthis match {
      case None => 
        prev match {
          case None => None
          case Some(e) => e find id
        }
      case _ => fthis
    }
  }
}*/

object Env {
  def apply[T]() = new Env[T]()
}

class Env[T] extends Environment[T] {
  
  var assocs:MutableMap[String, T] = MutableHashMap[String, T]()
  var prev:Option[Environment[T]] = None
  
  protected def this(prev:Option[Environment[T]]) = {
    this()
    this.prev = prev
  }
  
  def assoc(id:String, t:T) = {
    if (assocs contains(id)) false
    else {
      assocs put(id, t)
      true
    }
  }
    
  def assoc(elem:(String, T)) = assoc(elem._1, elem._2)
    
  def beginScope():Environment[T] = 
    new Env(Some(this:Environment[T]))
        
  def endScope():Option[Environment[T]] = prev
    
  def find(id:String):Option[T] = {
    val fthis = assocs get id
    fthis match {
      case None => 
        prev match {
          case None => None
          case Some(e) => e find id
        }
      case _ => fthis
    }
  }
}

object SeqEnv {
  def apply[T]() = new SeqEnv[T]()
}

class SeqEnv[T] extends SeqEnvironment[T] {
  
  var assocs:MutableMap[String, (T, Int)] = MutableHashMap[String, (T, Int)]()
  var prev:Option[SeqEnvironment[T]] = None
  var currIdx = 0
  
  protected def this(prev:Option[SeqEnvironment[T]]) = {
    this()
    this.prev = prev
  }
  
  def assoc(id:String, t:T) = {
    if (assocs contains(id)) false
    else {
      assocs put(id, (t, {val r = currIdx; currIdx = currIdx + 1; r}))
      true
    }
  }
    
  def assoc(elem:(String, T)) = assoc(elem._1, elem._2)
    
  def beginScope():SeqEnvironment[T] = 
    new SeqEnv(Some(this:SeqEnvironment[T]))
        
  def endScope():Option[SeqEnvironment[T]] = prev
  
  def findPair(id:String):Option[(T, Int)] = {
    val fthis = assocs get id
    fthis match {
      case None => 
        prev match {
          case None => None
          case Some(e) => 
            (e findPair id) match {
              case None => None
              case Some((x, y)) => Some((x, y + e.getSize))
            }
        }
      case _ => fthis
    }
  }
  
  def getSize():Int = assocs.size
    
  def find(id:String):Option[T] = { 
    findPair(id) match {
      case None => None
      case Some(x) => Some(x._1)
    }
  }
    
  def findIdx(id:String):Option[Int] = {
    findPair(id) match {
      case None => None
      case Some(x) => Some(x._2)
    }
  }
}