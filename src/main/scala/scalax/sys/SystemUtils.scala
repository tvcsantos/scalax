package scalax.sys

object SystemUtils {

  def readMultipleLinesFromIterator(lines: Iterator[String],
    suffix: String): (String, String) = {
    import scala.collection.mutable.StringBuilder
    val sb: StringBuilder = StringBuilder.newBuilder
    var idx = -1
    var ok = true
    var rem = ""
    val suffixSize = suffix.size
    while (lines.hasNext && ok) {
      val line = lines.next
      val idx = line.indexOf(suffix)
      if (idx == -1) sb.append(line + "\n")
      else {
        sb.append(line.substring(0, idx))
        rem = line.substring(idx + suffixSize)
        ok = false
      }
    }
    return (sb.mkString, rem)
  }

  def readMultipleLines(start: String,
    suffix: String): (String, String) = {
    var line = readLine(start)
    import scala.collection.mutable.StringBuilder
    val sb: StringBuilder = StringBuilder.newBuilder
    var idx = -1
    while ( line != null && { idx = line.indexOf(suffix); idx == -1 }) {
      sb.append(line + "\n")
      line = readLine
    }
    val suffixSize = suffix.size
    if (line != null) sb.append(line.substring(0, idx))
    return (sb.mkString, if (line != null)
      line.substring(idx + suffixSize) else "")
  }
  
  import scala.util.Properties
  
  lazy val is32Bit = osArch != null &&
    (osArch == "i386" || osArch == "x86")

  lazy val is64Bit = osArch != null &&
    (osArch == "amd64" || osArch == "x86_64")

  lazy val isMac = osName != null &&
  	osName.contains("mac")

  lazy val isWin = osName != null &&
  	osName.contains("win")

  lazy val isUnix = osName != null && 
  	(osName.contains("nix") || osName.contains("nux") ||
  		osName.contains("aix"))

  lazy val osName = {
    val x = Properties.propOrNull("os.name")
    if (x == null) null else x.toLowerCase
  }

  lazy val osArch = {
    val x = Properties.propOrNull("os.arch")
    if (x == null) null else x.toLowerCase
  }
  
  /** Get CPU time in nanoseconds. */
  private def getCpuTime(): Long = {
    import java.lang.management._;
    val bean: ThreadMXBean = ManagementFactory.getThreadMXBean();
    if (bean.isCurrentThreadCpuTimeSupported())
      bean.getCurrentThreadCpuTime()
    else 0L
  }
  
  private def getCpuTimeMilis(): Long = {
    Math.round(getCpuTime()/1000.0/1000.0)    
  }

  def crono[A](a: => A): (A, Long) = {
    var time:Option[Long] = None
    var res:Option[A] = None
    val thread = new Thread() {
      override def run() = {
        val start = getCpuTimeMilis()
        val r = a
        val end = getCpuTimeMilis()
        time = Some((end - start))
        res = Some(r)
      }
    }
    thread.start
    thread.join
    (res.get, time.get)
    /*val start = getCpuTimeMilis()
    val r = a
    val end = getCpuTimeMilis()
    (r, (end - start))*/
  }
  
  import java.io.FilenameFilter
  import java.io.FileFilter
  import java.io.File

  lazy val acceptAllFilenameFilter =
    new FilenameFilter() {
      def accept(dir: File, name: String): Boolean = true
    }
  
  lazy val acceptAllFileFilter =
    new FileFilter() {
      def accept(dir: File): Boolean = true
    }
  
  def listFiles(file: File, recursive: Boolean): Array[File] =
    listFiles(file, acceptAllFileFilter, recursive)
      
  def listFiles(file: File, filter:FilenameFilter, recursive:Boolean): 
	  Array[File] = {
    if (!file.exists()) return null
    if (!file.isDirectory()) return null
    if (!recursive) {
      return file.listFiles(filter)
    }
    import scala.collection.mutable.{
      Stack => MutableStack
    }
    val result = scala.collection.mutable.ListBuffer[File]()
    val stack: MutableStack[File] = MutableStack[File]()
    stack.pushAll(file.listFiles())
    while (!stack.isEmpty) {
      val file = stack.pop()
      if (filter.accept(file, file.getName())) 
          result.append(file)
      if (file.isDirectory()) {
        for (f <- file.listFiles()) {
          if (filter.accept(f, f.getName()) || f.isDirectory())
            stack.push(f)
        }
      }
    }
    result.toArray
  }
  
  def listFiles(file: File, filter: FileFilter, recursive:Boolean): 
	  Array[File] = {
    if (!file.exists()) return null
    if (!file.isDirectory()) return null
    if (!recursive) {
      return file.listFiles(filter)
    }
    import scala.collection.mutable.{
      Stack => MutableStack
    }
    val result = scala.collection.mutable.ListBuffer[File]()
    val stack: MutableStack[java.io.File] = MutableStack[File]()
    stack.pushAll(file.listFiles())
    while (!stack.isEmpty) {
      val file = stack.pop()
      if (filter.accept(file)) 
          result.append(file)
      if (file.isDirectory()) {
        for (f <- file.listFiles()) {
          if (filter.accept(f) || f.isDirectory())
            stack.push(f)
        }
      }
    }
    result.toArray
  }
  
  final class FileNameExtensionFilter(
      description:String, extensions:String*) extends FileFilter {
    private val internal = new javax.swing.filechooser.FileNameExtensionFilter(
        description, extensions:_*)

    def accept(file:File):Boolean = {
      if (file != null) {
        if (file.isDirectory()) false
        else internal.accept(file)
      } else false
    }
    
    def getDescription():String = internal.getDescription()
    
    def getExtensions():Array[String] = internal.getExtensions()
    
    def getPrettyExtensions():String =
      getExtensions().map(x => s"'.$x'").mkString(", ")
  }

  def main(args: Array[String]) {
    println(Properties.propOrNull("os.arch"))
    println(isMac)
    println(isWin)
    println(isUnix)
    println(new File("../funspec/test").listFiles().toSeq)
    println(listFiles(new File("../funspec/test"), false).toSeq)
  }
}