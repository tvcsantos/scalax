package scalax.util

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
}