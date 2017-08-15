package scalax.util

trait ObjectPool[T] {

  import scala.collection.mutable.ListBuffer

  val locked, unlocked: ListBuffer[T] = new ListBuffer[T]()

  //def create(): Option[T]

  final def checkOut(): T = {
    //var res: T = null
    unlocked.synchronized {
      while(unlocked.isEmpty) {
        unlocked.wait()
      }
      val t = unlocked.remove(0)
      //res = Some(t)
      locked.append(t)
      return t
    }
    //return res
  }

  final def checkIn(t:T) = {
    unlocked.synchronized {
      locked -= t
      unlocked.append(t)
      unlocked.notifyAll()
    }
  }
}