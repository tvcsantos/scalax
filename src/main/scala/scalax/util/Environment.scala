package scalax.util

trait Environment[K, V] {
  def assoc(id:K, t:V):Boolean
    
  def assoc(elem:(K, V)):Boolean
  
  def beginScope():Environment[K, V]
  
  def endScope():Option[Environment[K, V]]
  
  def find(id:K):Option[V]
  
  def getCurrent():Set[(K, V)]
  
  def getCurrentKeys():Set[K]
}

object Env {
  def apply[K, V]() = new Env[K, V]()
}

class Env[K, V] extends Environment[K, V] {
  
  import scala.collection.mutable.{ 
    Map => MutableMap,
    HashMap => MutableHashMap
  }
  
  var assocs:MutableMap[K, V] = MutableHashMap[K, V]()
  var prev:Option[Environment[K, V]] = None
  
  protected def this(prev:Option[Environment[K, V]]) = {
    this()
    this.prev = prev
  }
  
  def assoc(id:K, t:V) = {
    if (assocs contains(id)) false
    else {
      assocs put(id, t)
      true
    }
  }
    
  def assoc(elem:(K, V)) = assoc(elem._1, elem._2)
    
  def beginScope():Environment[K, V] = 
    new Env(Some(this:Environment[K, V]))
        
  def endScope():Option[Environment[K, V]] = prev
    
  def find(id:K):Option[V] = {
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
  
  def getCurrent():Set[(K, V)] = {
    assocs.toSet[(K, V)]
  }
  
  def getCurrentKeys():Set[K] = {
    assocs.keySet.toSet
  }
  
  override def toString() = {
    import scala.collection.mutable.Stack
    val stack:Stack[Set[(K, V)]] = Stack()
    var sentinel:Option[Environment[K, V]] = Some(this) 
    while (sentinel != None) {
      stack.push(sentinel.get.getCurrent())
      sentinel = sentinel.get.endScope()
    }
    val sb:StringBuilder = StringBuilder.newBuilder
    var spaces = 0
    while (!stack.isEmpty) {
      val assocs = stack.pop
      sb.append(
          assocs.map(x => " " * spaces + 
              s"- ${x._1} -> ${x._2}").mkString("\n")
      )
      if (assocs isEmpty) sb.append(" " * spaces + "- empty")
      if (!stack.isEmpty) sb.append("\n")
      spaces += 2      
    }
    sb.toString
  }
}


/*trait Environment[T] {
  
  def assoc(id:String, t:T):Boolean
    
  def assoc(elem:(String, T)):Boolean
  
  def beginScope():Environment[T]
  
  def endScope():Option[Environment[T]]
  
  def find(id:String):Option[T]
  
  def getCurrent():Set[(String, T)]
  
  def getCurrentNames():Set[String]
}

object Env {
  def apply[T]() = new Env[T]()
}

class Env[T] extends Environment[T] {
  
  import scala.collection.mutable.{ 
    Map => MutableMap,
    HashMap => MutableHashMap
  }
  
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
  
  def getCurrent():Set[(String, T)] = {
    assocs.toSet[(String, T)]
  }
  
  def getCurrentNames():Set[String] = {
    assocs.keySet.toSet
  }
  
  override def toString() = {
    import scala.collection.mutable.Stack
    val stack:Stack[Set[(String, T)]] = Stack()
    var sentinel:Option[Environment[T]] = Some(this) 
    while (sentinel != None) {
      stack.push(sentinel.get.getCurrent())
      sentinel = sentinel.get.endScope()
    }
    val sb:StringBuilder = StringBuilder.newBuilder
    var spaces = 0
    while (!stack.isEmpty) {
      val assocs = stack.pop
      sb.append(
          assocs.map(x => " " * spaces + 
              s"- ${x._1} -> ${x._2}").mkString("\n")
      )
      if (assocs isEmpty) sb.append(" " * spaces + "- empty")
      if (!stack.isEmpty) sb.append("\n")
      spaces += 2      
    }
    sb.toString
  }
}*/

object ScopedMap {
  def apply[K, V]() = new ScopedMap[K, V]()
}

class ScopedMap[K, V] {

  import scala.collection.mutable.{
  Map => MutableMap,
  HashMap => MutableHashMap
  }

  var assocs:MutableMap[K, V] = MutableHashMap[K, V]()
  var prev:Option[ScopedMap[K, V]] = None

  protected def this(prev:Option[ScopedMap[K, V]]) = {
    this()
    this.prev = prev
  }

  def put(id:K, t:V):Option[V] =
    assocs.put(id, t)

  //def put(elem:(K, V)):Option[V] = put(elem._1, elem._2)

  def beginScope():ScopedMap[K, V] =
    new ScopedMap(Some(this:ScopedMap[K, V]))

  def endScope():Option[ScopedMap[K, V]] = prev

  def get(id:K):Option[V] = {
    val fthis = assocs get id
    fthis match {
      case None =>
        prev match {
          case None => None
          case Some(e) => e get id
        }
      case _ => fthis
    }
  }

  def getCurrent():Set[(K, V)] = {
    assocs.toSet[(K, V)]
  }

  def getCurrentKeys():Set[K] = {
    assocs.keySet.toSet
  }

  override def toString() = {
    import scala.collection.mutable.Stack
    val stack:Stack[Set[(K, V)]] = Stack()
    var sentinel:Option[ScopedMap[K, V]] = Some(this)
    while (sentinel != None) {
      stack.push(sentinel.get.getCurrent())
      sentinel = sentinel.get.endScope()
    }
    val sb:StringBuilder = StringBuilder.newBuilder
    var spaces = 0
    while (!stack.isEmpty) {
      val assocs = stack.pop
      sb.append(
        assocs.map(x => " " * spaces +
          s"- ${x._1} -> ${x._2}").mkString("\n")
      )
      if (assocs isEmpty) sb.append(" " * spaces + "- empty")
      if (!stack.isEmpty) sb.append("\n")
      spaces += 2
    }
    sb.toString
  }
}

object ScopedSet {
  def apply[K]() = new ScopedSet[K]()
}

class ScopedSet[K] {

  import scala.collection.mutable.{
  Set => MutableSet,
  HashSet => MutableHashSet
  }

  var assocs:MutableSet[K] = MutableHashSet[K]()
  var prev:Option[ScopedSet[K]] = None

  protected def this(prev:Option[ScopedSet[K]]) = {
    this()
    this.prev = prev
  }

  def add(id:K):Boolean = assocs.add(id)

  def beginScope():ScopedSet[K] =
    new ScopedSet(Some(this:ScopedSet[K]))

  def endScope():Option[ScopedSet[K]] = prev

  def contains(id:K):Boolean = {
    val fthis = assocs contains id
    if (!fthis) {
      prev match {
        case None => false
        case Some(e) => e contains id
      }
    } else true
  }

  def getCurrent():Set[K] = {
    assocs.toSet[K]
  }

  override def toString() = {
    import scala.collection.mutable.Stack
    val stack:Stack[Set[K]] = Stack()
    var sentinel:Option[ScopedSet[K]] = Some(this)
    while (sentinel != None) {
      stack.push(sentinel.get.getCurrent())
      sentinel = sentinel.get.endScope()
    }
    val sb:StringBuilder = StringBuilder.newBuilder
    var spaces = 0
    while (!stack.isEmpty) {
      val assocs = stack.pop
      sb.append(
        assocs.map(x => " " * spaces +
          s"- $x").mkString("\n")
      )
      if (assocs isEmpty) sb.append(" " * spaces + "- empty")
      if (!stack.isEmpty) sb.append("\n")
      spaces += 2
    }
    sb.toString
  }
}