package scalax.util

trait Debuggable {
  
  private var debug = false
  private var debuggerCounter = 0
  private var name:String = null
  private var dir:String = null
  
  import java.io.PrintWriter
  
  private var debugger:PrintWriter = null
  
  def getDebugger():PrintWriter = {
    if (!isDebug())
      throw new DebuggerException(
          "Cannot retrieve debugger: debugger current state is 'off'")
    initDebugger(dir, name)
  }
  
  def setDebug(b:Boolean):Unit = debug = b
  
  def setName(name:String):Unit =
    this.name = name
    
  def setDir(dir:String):Unit =
    this.dir = dir
  
  def isDebug():Boolean = debug
  
  final def clearDebug():Unit = {
    if (isDebug()) {
      if (debugger != null) {
        clearDebugImpl()
        debuggerCounter += 1
        debugger.flush
        debugger.close
        debugger = null
      }
    }
  }
    
  protected def clearDebugImpl():Unit
  
  protected def initDebugger(dir:String, name:String):PrintWriter = {
    if (debugger == null)
      debugger = 
          makeDebugger(dir, f"$name${debuggerCounter}%03d_" + 
    		  Integer.toHexString(System.identityHashCode(this)) + ".debug")
    debugger
  } 
  
  protected def makeDebugger(dir:String, name:String) = {
    if (dir == null || name == null)
      throw new NullPointerException
    new java.io.PrintWriter(new java.io.File(
        s"$dir${java.io.File.separator}$name"), "UTF-8")
  }

}

class DebuggerException(msg:String) extends Exception(msg)