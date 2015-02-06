package scalax.util

trait Debuggable {

  protected var debuggerOpt:Option[DebuggerWriter] = None

  def setDebugger(debuggerWriter: DebuggerWriter) = {
    debuggerOpt = Some(debuggerWriter)
  }

  import scalax.util.Level._

  def print(s:String, level:Level):Unit =
    debuggerOpt.foreach(_.print(s, level))

  def println(s:String, level:Level):Unit =
    debuggerOpt.foreach(_.println(s, level))

  def print(x:AnyRef, level:Level):Unit =
    debuggerOpt.foreach(_.print(x, level))

  def println(x:AnyRef, level:Level):Unit =
    debuggerOpt.foreach(_.println(x, level))

  def print(b:Boolean, level:Level):Unit =
    debuggerOpt.foreach(_.print(b, level))

  def println(b:Boolean, level:Level):Unit =
    debuggerOpt.foreach(_.println(b, level))

  def println(level:Level):Unit =
    debuggerOpt.foreach(_.println(level))

  def setLevel(level:Level):Unit =
    debuggerOpt.foreach(_.setLevel(level))

  def print(s:String):Unit = print(s, NONE)

  def println(s:String):Unit = println(s, NONE)

  def print(x:AnyRef):Unit = print(x, NONE)

  def println(x:AnyRef):Unit = println(x, NONE)

  def print(b:Boolean):Unit = print(b, NONE)

  def println(b:Boolean):Unit = println(b, NONE)

}