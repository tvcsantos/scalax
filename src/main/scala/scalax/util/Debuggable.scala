package scalax.util

import java.io.OutputStream

trait Debuggable {

  import java.io.PrintWriter

  protected var debuggerOpt:Option[PrintWriter] = None

  final def setDebugger(debuggerOut: OutputStream):Unit = {
    setDebugger(new PrintWriter(debuggerOut, true))
  }

  def setDebugger(debuggerOut: PrintWriter):Unit = {
    debuggerOpt = Some(debuggerOut)
  }

  import scalax.util.Level._

  final def print[T](s:T):Unit = print(s, NONE)

  final def println[T](s:T):Unit = println(s, NONE)

  private var currentLevel = NONE

  final def print[T](s:T, level:Level):Unit = {
    if (level <= currentLevel)
      debuggerOpt.foreach(_.print(s))
  }

  final def println[T](s:T, level:Level):Unit = {
    print(s, level)
    println(level)
  }

  final def println(level: Level):Unit = {
    if (level <= currentLevel) debuggerOpt.foreach(_.println)
  }

  final def setLevel(level: Level):Level = {
    val oldLevel = currentLevel
    currentLevel = level
    oldLevel
  }

}