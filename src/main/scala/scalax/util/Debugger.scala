package scalax.util

import java.io.PrintWriter

object Debugger {

  import scala.util.Properties
    
  def isOn():Boolean = {
    Properties.propIsSet("debug")
  }
  
  final def isOff():Boolean = !isOn()
  
  import scala.collection.mutable.{
    Map => MutableMap, 
    HashMap => MutableHashMap
  }
   
  def getDebugger(name:String) = 
    new java.io.PrintWriter(new java.io.File(s"debug/$name.debug"), "UTF-8")

  def getDebugger(out:java.io.OutputStream) = {
    new PrintWriter(out)
  }
}

object Level extends Enumeration {
  type Level = Value
  val ERROR, NONE, INFO, DEBUG = Value
}

class DebuggerWriter(out:java.io.OutputStream) extends java.io.PrintWriter(out, true) {

  import Level._

  protected var currentLevel = NONE

  def print[T](s:T, level:Level):Unit = {
    if (level <= currentLevel) print(if (s == null) "null"  else s.toString())
  }

  def println[T](s:T, level:Level):Unit = {
    print(s, level)
    println(level)
  }

  /*def print(s:String, level: Level):Unit = {
      if (level <= currentLevel) print(s)
  }

  def println(s:String, level: Level):Unit = {
    if (level <= currentLevel) {
      print(s, level)
      println(level)
    }
  }

  def print(x:AnyRef, level:Level):Unit = {
    if (level <= currentLevel) print(x)
  }

  def println(x:AnyRef, level:Level):Unit = {
    if (level <= currentLevel) {
      print(x, level)
      println(level)
    }
  }

  def print(b:Boolean, level:Level):Unit = {
    if (level <= currentLevel) print(b)
  }

  def println(b:Boolean, level:Level):Unit = {
    if (level <= currentLevel) {
      print(b, level)
      println(level)
    }
  }*/

  def println(level: Level):Unit = {
      if (level <= currentLevel) println()
  }

  def setLevel(level: Level):Level = {
    val oldLevel = currentLevel
    currentLevel = level
    oldLevel
  }
}