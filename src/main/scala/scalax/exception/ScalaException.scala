package scalax.exception

object ScalaException {
  
  def apply[T <: Throwable](cause: Throwable, args:Object*)
  	(implicit m: scala.reflect.Manifest[T]) : T = {
    apply(args:_*).initCause(cause).asInstanceOf[T]
  }
   
  def apply[T <: Throwable](args:Object*)
  	(implicit m: scala.reflect.Manifest[T]):T = {
    val classes = args.map(_.getClass)
    m.runtimeClass.getConstructor(classes:_*).newInstance(args:_*).asInstanceOf[T]
  } 
  
  import scala.util.parsing.input.{NoPosition, Positional}
  
  def getPositionalMessage[T <: Positional](positional:T, msg:String) = {
    positional.pos match {
      case NoPosition =>
        s"failure: $msg"
      case pos =>
        s"[${pos}] failure: $msg\n\n${pos.longString}"
    }
  }

}
