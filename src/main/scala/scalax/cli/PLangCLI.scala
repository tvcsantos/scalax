package scalax.cli

import java.io.{FilterWriter, FileOutputStream}

import org.rogach.scallop.ScallopConf

import scala.util.parsing.combinator.Parsers
import scalax.sys.SystemUtils.FileNameExtensionFilter
import scalax.util.{DebuggerWriter, Debuggable}

abstract class PLangCLI[T <: Parsers](override val args: Seq[String], 
    val filter:FileNameExtensionFilter) 
  	extends ScallopConf(args) with Debuggable {
  
  import scalax.sys.SystemUtils
  import scalax.util.Level._

  type Parser = T
  type U
  type V
  
  lazy val pLangName:String =
    getPLangName
  
  lazy val execName:String =
    getExecName
    
  protected def getPLangName:String
  
  protected def getExecName:String
      
  printedName = pLangName
  
  protected var shortVersionOpt:Option[String] = None
  
  def shortVersion(short:String) =
    shortVersionOpt = Some(short)
   
  //val debug = opt[Boolean]("debug", descr = "Debug mode")

  val debug = opt[String]("debug", descr = "Debug mode",
    argName = "mode", default = Some("NONE"))

  //val log = tally()

  val interactive = opt[Boolean]("interactive",
    descr = "Interactive mode")

  val files = opt[List[String]]("files",
    descr = "Input file list", argName = "file")

  val output = opt[String]("output",
    descr = "Redirect program output to a file", argName = "file")

  /*val debugout = opt[String]("debugOutput",
    descr = "Redirect debug to a file", argName = "file", short = 'c')*/

  val fileInteractive = opt[Boolean]("fileInteractive",
    descr = "File interactive mode", short = 'r')

  val test = opt[Boolean]("test",
    descr = "Test mode")
    
  val shortVersion = opt[Boolean]("shortVersion", short = 'v',
      descr = "Version number")
      
  val stdin = opt[Boolean]("stdin", descr = "Read from standard input")

  import java.io.File

import scala.collection.mutable.ListBuffer

  validateOpt(files) {
    case None | Some(Nil) if args.contains("--files") ||
      args.contains("-f") => Left(
      s"When using '--files' option, at least one filename must be provided")
    case Some(list) =>
      val bufferInexistent = ListBuffer[String]()
      val bufferWrongExt = ListBuffer[String]()
      for (name <- list) {
        val file = new File(name)
        if (!file.exists())
          bufferInexistent.append(name)
        else if (file.isFile() && !filter.accept(file))
          bufferWrongExt.append(name)
      }
      if (!bufferInexistent.isEmpty) {
        Left(s"${bufferInexistent.mkString(", ")} (No such file${
          if (bufferInexistent.size > 1) "(s)" else ""
        } or directory${if (bufferInexistent.size > 1) "(s)" else ""})")
      } else if (!bufferWrongExt.isEmpty) {
        Left(s"${bufferWrongExt.mkString(", ")} (Wrong extension${
          if (bufferWrongExt.size > 1) "(s)" else ""
        })")
      } else Right(Unit)
    case _ => Right(Unit)
  }

  //dependsOnAll(debugout, List(debug))

  mutuallyExclusive(interactive, fileInteractive, files, stdin)

  conflicts(test, List(fileInteractive, files, interactive, stdin))
    
  def preStart()
   
  final def start(parser: Parser) {
    val start = "> "
    val quit = "quit(\\s|\r\n?|\n)*"

    val outStream = if (output.isSupplied)
        new FileOutputStream(output())
        else Console.out

    setDebugger(outStream)

    if (debug.isSupplied) {
      val level = scalax.util.Level.withName(debug())
      setLevel(level)
    } else setLevel(scalax.util.Level.INFO)
      
    Console.withOut(outStream) {
      
	    if (shortVersion()) {
	      println(shortVersionOpt.getOrElse("unknown"))
	      return;
	    }
	      
	    preStart()
	        
	    if (test())
	      startTest(parser, start, quit)
	    else if (interactive())
	      startInteractive(parser, start, quit)
	    else if (fileInteractive())
	      startFileInteractive(parser, start, quit)
	    else if (stdin())
	      startStdin(parser)
	    else if (files.isSupplied)
	      startFiles(parser, files().map(
	          new java.io.File(_)).toArray)
	    else if (matchOtherOptions())
	      startOtherOptions(parser, start, quit)
	    else startInteractive(parser, start, quit)
	    
	    postStart()
    
    }
  }
  
  def matchOtherOptions():Boolean
  
  def startOtherOptions(parser: Parser, 
      start:String, quit:String)
  
  def postStart()

  protected def startTest(parser:Parser,
      start:String, quit:String)

  final protected def startStdin(parser: Parser) {
    import scalax.sys.SystemUtils.readMultipleLines

    val suffix = ";;"

    val pair = readMultipleLines("", suffix)

    val res = runProgram(parser, pair._1, makeArgumentsForRun())

  }
  
  final protected def startInteractive(parser:Parser,
      start:String, quit:String) {
    import scalax.sys.SystemUtils.readMultipleLines
    
    val suffix = ";;"
         
    var extra = makeArgumentsForRun()

    while (true) {
      val pair = readMultipleLines(start, suffix)
      
      if (pair._1.trim.matches(quit)) sys.exit(0)
      
      val res = runProgram(parser, pair._1, extra)
      extra = res._2
      
    }
  }
  
  final protected def startFileInteractive(
      parser:Parser, start:String, quit:String) {
    import scalax.sys.SystemUtils.listFiles
    
    val suffix = ";;"

    while (true) {
      val name = readLine(start)
      
      if (name.trim.matches(quit + suffix)) sys.exit(0)
      
      val file = new File(name)
      
      if (!file.exists()) {
        println(getErrorFromString(
            file.getPath() + " (No such file or directory)"), ERROR)
      } else {
        if (file.isFile()) {
          if (filter.accept(file))
            startFile(parser, file)
          else println(getErrorFromString(file.getPath() +
              s" (Wrong file extension. Expected ${
            filter.getPrettyExtensions()})"), ERROR)
        } else {
          val files = listFiles(file, filter, true)
          if (!files.isEmpty) startFiles(parser, files)        	  
        }
      }
    }
  }
  
  final protected def startFiles(parser:Parser, files:Array[File]) {
    import scalax.sys.SystemUtils.listFiles
    for (f <- files) {
      if (f.isFile()) startFile(parser, f)
      else {
        val files = listFiles(f, filter, true)
        //println(files.toSeq)
        if (!files.isEmpty) startFiles(parser, files)
      }
    }
  }
  
  final protected def startFile(parser:Parser, file:File) {    
    println(s"Processing file $file", INFO)
    lazy val testFileLines =
      scala.io.Source.fromFile(file).getLines
      
    val extra = makeArgumentsForRun()
    
    val r = SystemUtils.crono(
        runProgram(parser, testFileLines.mkString("\n"), extra))
    println(s"Test completed in ${r._2 / 1000.0}s", INFO)

    println(s"Finished file $file", INFO)
  }
  
  protected def runProgram(parser:Parser,
      source:String, extra:U):(V, U)
      
  protected def makeArgumentsForRun():U
  
  /*def getErrorFromException(x:Exception):String = {
    x.printStackTrace()
    s"[${printedName}] Error: ${
      if (x.getMessage() == null) x.getStackTraceString
      else x.getMessage()      
      }"
  }*/
  
  def getErrorFromException(x:Throwable):String = {
    //x.printStackTrace()
    s"[${printedName}] Error: ${
      if (x.getMessage() == null) x.getStackTraceString
      else x.getMessage()      
      }"
  }
  
  def getErrorFromParser(error: Parser#NoSuccess): String = {
    s"[${printedName}] Error: ${error}"
  }
  
  def getErrorFromString(msg: String): String = {
    s"[${printedName}] Error: ${msg}"
  }
  
  protected def osIsSupported():Boolean
  
  protected def archIsSupported():Boolean
  
  import scalax.sys.SystemUtils._
  
  def getBannerModel(descriptionOpt:Option[String]) = {
    s"""Usage: ${if (isMac || isUnix) "sh " else ""}${execName} [OPTION]...
            |${descriptionOpt match { case None => "" case Some(x) => x}}     
            |Options:
            |""".stripMargin
  }
  
  import java.text.SimpleDateFormat
  import java.util.Calendar
  
  def getVersionModel(versionOpt:Option[String], 
      calendarOpt:Option[Calendar]) = {
    s"${pLangName}${
      versionOpt match {
        case None => ""
        case Some(x) => s" [version $x]"
      }
    }. (C) Copyright${
      calendarOpt match {
        case None => ""
        case Some(x) => s" ${
          val formatter = new SimpleDateFormat("yyyy")
          formatter.format(x.getTime())
          }"
      }
    }"
  }
    
  def getFooterModel(footerOpt:Option[String]) = {
    s"""|
    	|Notes:
        |
        |For option '-f, --files', if <file> is a directory all files with extension 
        |${filter.getPrettyExtensions()} in the directory <file> are executed
    	|
    	|By default if you don't provide arguments the program will execute in interactive mode${
        footerOpt match {case None => "" 
          case Some(footer) => s"\n\n$footer"}}""".stripMargin     
  }
      
}