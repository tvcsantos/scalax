package scalax.util

trait SeqEnvironment[T] extends Environment[String, T] {
  
  def findIdx(id:String):Option[Int]
  
  def findPair(id:String):Option[(T, Int)]
  
  def beginScope():SeqEnvironment[T]
  
  def endScope():Option[SeqEnvironment[T]]
  
  def getSize():Int
}

object SeqEnv {
  def apply[T]() = new SeqEnv[T]()
}

class SeqEnv[T] extends SeqEnvironment[T] {
  
  import scala.collection.mutable.{ 
    Map => MutableMap,
    HashMap => MutableHashMap
  }
  
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
  
  def getCurrent():Set[(String, T)] = {
    assocs.toSet[(String, (T, Int))].map(x => (x._1, x._2._1))
  }
  
  def getCurrentKeys():Set[String] = {
    assocs.keySet.toSet
  }
}