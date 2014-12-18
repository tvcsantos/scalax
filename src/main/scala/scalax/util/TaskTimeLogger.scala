package scalax.util

class TaskTimeLogger[T] {
  import scala.collection.mutable.{
    Map => MutableMap,
    HashMap => MutableHashMap
  }
  
  protected var logger:MutableMap[T, Long] =
    MutableHashMap[T, Long]()
    
  def add(task:T, time:Long):Option[Long] =
    logger.put(task, time)
    
  def sum():Option[Long] =
    if (logger.isEmpty) None
    else 
      Some(logger.foldLeft(0L)((r, e) => r + e._2))

  def isEmpty =
    logger.isEmpty
  
  def remove(task:T):Option[Long] =
    logger.remove(task)
    
  def get(task:T):Option[Long] =
    logger.get(task)
    
  def contains(task:T):Boolean =
    logger.contains(task)
}