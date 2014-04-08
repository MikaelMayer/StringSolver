/**
 *     _____ _       _         _____     _             
 *    |   __| |_ ___|_|___ ___|   __|___| |_ _ ___ ___ 
 *    |__   |  _|  _| |   | . |__   | . | | | | -_|  _|
 *    |_____|_| |_| |_|_|_|_  |_____|___|_|\_/|___|_|  
 *                        |___|      
 * File name: Implicits.scala
 * Author   : Mikaël Mayer
 * Date     : 14.02.2014
 * Function : Provides routines for command-line interaction.
 *            mv, auto, partition, filter
 */
package ch.epfl.lara.synthesis.stringsolver

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.InputStreamReader
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.sql.Timestamp

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.mapAsScalaMap
import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{HashMap => MMap}

import Implicits.AugString
import Program.Concatenate
import Program.ConstStr
import Program.Program
import Program.Program
import Program.TraceExpr
import ProgramSet.ProgramSet

/**
 * Main object for dealing with files manipulation
 */
object Main {
  import Implicits._
  //import ProgramsSet._
  
  /**
   * Main function
   */
  def main(args: Array[String]): Unit = {
    val cmd = args.toList
    cmd match {
      case Auto()::q =>    AutoLogFile.parseCmd(q)
      case Move()::q =>    MvLogFile.parseCmd(q)
      case Partition()::q => PartitionLogFile.parseCmd(q)
      case Filter()::q => FilterLogFile.parseCmd(q)
      case MapCmd()::q => MapLogFile.parseCmd(q)
      case Server()::q => Service.start(q.toArray)
      case _ => println(s"Unknown command: " + cmd.mkString(" ") + "\n Try auto, mv, filter, partition, map or server")
    }
  }
  
  var debug = false // Can be set by the flag -d
  var useLogFileOnDisk = true // Set to false to use memory logging for service

  val HISTORY_DIR = "StringSolverRenaming"
  val HISTORY_MV_FILE = "mv.log"
  val HISTORY_AUTO_FILE = "auto.log"
  val HISTORY_PARTITION_FILE = "partition.log"
  val HISTORY_FILTER_FILE = "filter.log"
  val HISTORY_MAP_FILE = "map.log"
  final val NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED = 3
  
    /**
     * Different kind of inputs
     */
  final val INPUT_DIRECTORY = "INPUT_DIRECTORY"
  final val INPUT_FILE = "INPUT_FILE"
  final val INPUT_FILE_EXTENSION = "INPUT_FILE_EXTENSION"
  final val INPUT_LINE = "INPUT_LINE"
  trait ParameterInput { def prefix: String
    def apply(n: Int) = prefix+n.toString
    def unapply(s: String): Option[Int] = if(s.startsWith(prefix)) Some(s.substring(prefix.length).toInt) else None
  }
  object MultipleInputs {
    def unapply(s: String): Option[Int] = s match {
      case INPUT_FILE_LIST(n) => Some(n)
      case INPUT_FILE_CONTENT(n) => Some(n)
      case INPUT_DIR_CONTENT(n) => Some(n)
      case INPUT_FILE_PROPERTIES(n) => Some(n)
      case INPUT_FILE_EXTENSION => Some(2)
      case _ => None
    }
  }
    
  object INPUT_FILE_LIST extends ParameterInput{
    val prefix = "INPUT_FILE_LIST"
  }
  object INPUT_FILE_CONTENT extends ParameterInput {
    val prefix = "INPUT_FILE_CONTENT"
  }
  object INPUT_DIR_CONTENT extends ParameterInput {
    val prefix = "INPUT_DIR_CONTENT"
  }
  object INPUT_FILE_PROPERTIES extends ParameterInput {
    val prefix = "INPUT_FILE_PROPERTIES"
  }
  implicit class NatureString(s: LogLine[_]) {
    def onlyFiles: Boolean = {
      s.nature match {
        case INPUT_FILE | INPUT_FILE_LIST(_) | INPUT_FILE_CONTENT(_) | INPUT_FILE_PROPERTIES(_) | INPUT_FILE_EXTENSION => true
        case INPUT_DIR_CONTENT(_) | INPUT_DIRECTORY => false
        case _ => true
      }
    }
    def isProperties: Boolean = {
      s.nature match {
        case INPUT_FILE_PROPERTIES(_) => true
        case _ => false
      }
    }
    def isExtension: Boolean = {
      s.nature match {
        case INPUT_FILE_EXTENSION => true
        case _ => false
      }
    }
  }
  type Nature = String
  type FileName = String
  type Performed = Boolean
  type ContentFlag = Boolean
  
  final val RETRY = false
  final val OK = true
  
  
  final object Move {
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == apply() || s.toLowerCase() == "move") {
        Some(())
      } else None
    }
    def apply(): String = "mv"
  }
  trait StringWrapper {
    def apply(): String
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == apply()) {
        Some(())
      } else None
    }
  }
  final object Partition extends StringWrapper {
    def apply(): String = "partition"
  }
  final object Auto extends StringWrapper {
    def apply(): String = "auto"
  }
  final object Filter extends StringWrapper {
    def apply(): String = "filter"
  }
  final object Server extends StringWrapper {
    def apply(): String = "server"
  }
  final object MapCmd extends StringWrapper {
    def apply(): String = "map"
  }
  
  def readProperties(path: String): List[String] = {
    readProperties(new File(workingDirAbsFile, path))
  }
  
  def readProperties(file: File): List[String] = {
    Files.getOwner(Paths.get(file.getAbsolutePath())).getName().replaceAll(".*\\\\", "")::
    getDate(file.lastModified, "yyyy"):: 
    getDate(file.lastModified, "MM"):: 
    getDate(file.lastModified, "dd")::
    getDate(file.lastModified, "hh")::
    getDate(file.lastModified, "mm")::
    getDate(file.lastModified, "ss")::Nil
  }
  
  def getDate(milliseconds: Long, format: String): String = {
    val sdf = new java.text.SimpleDateFormat(format)
    return sdf.format(milliseconds)
  }
  
  def readFile(path: String): String = {
    readFile(new File(workingDirAbsFile, path))
  }
  def readFile(file: File): String = {
    if(file.isDirectory()) {
      file.list().mkString("\n")
    } else {
      val encoded = Files.readAllBytes(Paths.get(file.getAbsolutePath()));
      val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
      content
    }
  }
  def readLines(content: String): List[String] = {
    content.split("\r?\n")/*.map(_.trim)*/.toList
  }
  
  //val path = Main.getClass().getProtectionDomain().getCodeSource().getLocation().getPath()
  var workingDirAbsFile = System.getProperty("user.dir");//URLDecoder.decode(path, "UTF-8");
  def workingDirAbsFileFile = new File(workingDirAbsFile)
  def fileListerString() = fileLister().map(_.getName())
  var fileLister = () => workingDirAbsFileFile.listFiles()
  
  trait LogWrapper[A <: LogLine[A]] {
    def delete()
    def getContent: Seq[A]
    def setContent(s: Seq[A]): Unit
    def addLine(s: A)
  }
  case class Lines[A <: LogLine[A]](var lines: List[A]) extends LogWrapper[A] {
    def delete() = lines = List()
    override def toString = "internal log"
    def getContent: Seq[A] = lines
    def setContent(s: Seq[A]) = {
      lines = s.toList
    }
    def addLine(s: A) = {
      lines = lines ++ List(s)
    }
  }
  case class FileWrapper[A <: LogLine[A] : LogFileFactory](var file: File) extends LogWrapper[A] {
    def delete() = file.delete
    override def toString = file.getAbsolutePath()
    def getContent: Seq[A] = {
      val s = readLines(readFile(file))
      s flatMap { line => implicitly[LogFileFactory[A]].extractor(line, None) }
    }
    def setContent(s: Seq[A]) = {
      val out = new PrintWriter(new BufferedWriter(new FileWriter(file.getAbsoluteFile(), false)));
      out.println(s.mkString("\n"))
      out.close
    }
    def addLine(s: A) = {
      val out = new PrintWriter(new BufferedWriter(new FileWriter(file.getAbsoluteFile(), true)));
      out.println(s.mkString)
      out.close
    }
  }
  
  var timeStampGiver = () => new Timestamp(new java.util.Date().getTime).toString

  /**
   * Decodes options given on the command line
   */
  object OptionsDecoder {
    def unapply(l: (List[String], Options)): Option[(Options, List[String])] = {
      l match {
        case (l, options) if options.noMoreDecoding =>
          None
        case ("--"::remaining, options) =>
          Some((options.copy(noMoreDecoding=true), remaining))
        case (("-w" | "--workingdir")::dir::remaining, options) => // Explain the algorithm
          workingDirAbsFile = new File(dir).getAbsolutePath()
          Some((options, remaining))
        case (("-e" | "--explain")::remaining, options) => // Explain the algorithm
          Some((options.copy(perform=false, explain=true), remaining))
        case (("-t" | "--test")::remaining, options) => // Show what the algorithm would do
          Some((options.copy(perform=false, test=true), remaining))
        case (("-a" | "--auto" | "--all")::remaining, options) => // Generalizes the command
          Some((options.copy(performAll=true), remaining))
        case (("-f" | "--filter")::remaining, options) => // Generalizes the command
          Some((options.copy(filter=true), remaining))
        case (("-l" | "--lines" | "--files" | "--list")::remaining, options) => // Takes input from the content of the file/folder
          Some((options.copy(contentFlag=true), remaining))
        case (("-b" | "--bash")::remaining, options) => // Produces a bash script for the mapping.
          Some((options.copy(produceBash=true), remaining))
        case (("-p" | "--properties")::remaining, options) => // Includes the file's properties as the input (not compatible with content)
          Some((options.copy(properties=true), remaining))
        case (("-s" | "--simple")::remaining, options) => // do not perform suggestions
          Some((options.copy(generalize=false), remaining))
        case (("-m" | "--mintwoexamples")::remaining, options) => // Suggests mappings only if there are at least two examples
          Some((options.copy(minTwoExamples=true), remaining))
        case (("-d" | "--debug")::remaining, options) => // activates debug
          debug = true
          Some((options.copy(debug=true), remaining))
        case _ => None
      }
    }
  }
  
  /**
   * Options to decode the command.
   */
  case class Options(
    perform: Boolean = true,
    explain: Boolean = false,
    test: Boolean = false,
    performAll: Boolean = false,
    minTwoExamples: Boolean = false,
    contentFlag: Boolean = false,
    properties: Boolean = false,
    produceBash: Boolean = false,
    debug: Boolean = false,
    filter: Boolean = false,
    generalize: Boolean = true,
    noMoreDecoding: Boolean = false,
    file: Option[File] = None
  ) {
    
  }
  
  
  /**
   * LogLine facilities
   */
  trait LogLine[A <: LogLine[A]] {
    def mkString: String
    def dir: String
    def time: String
    def performed: Boolean
    def nature: String
    def input: List[String]
    def setPerformed(b: Boolean): A
    private var programs: Option[Seq[ProgramSet[TraceExpr]]] = None
    def setPrograms(p: Option[Seq[ProgramSet[TraceExpr]]]): this.type = {programs = p; this}
    def getPrograms: Option[Seq[ProgramSet[TraceExpr]]] = programs
  }
  
  /**
   * LogFileFactory simulates interaction with a log file
   */
  trait LogFileFactory[A <: LogLine[A]] {
    def history_filename: String
    def extractor: (String, Option[Seq[ProgramSet[TraceExpr]]]) => Option[A]
    private var mHistoryFile: Option[LogWrapper[A]] = None
    def history_file: LogWrapper[A] = {
      mHistoryFile match {
        case Some(z) => z
        case None =>
          if(useLogFileOnDisk) {
            val tmpDir = System.getProperty("java.io.tmpdir")
            val history_dir = new File(tmpDir, HISTORY_DIR)
            if(!history_dir.exists()) {
              history_dir.mkdirs()
            }
            mHistoryFile = Some(FileWrapper(new File(history_dir, history_filename))(this))
            mHistoryFile.get
          } else {
            mHistoryFile = Some(Lines(List()))
            mHistoryFile.get
          }
        }
    }
    /**
     * Recovers all actions in history which happened in this folder
     */
    def getHistory(folder: File): Seq[A] = {
      val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
      val checkDir = (s: String) => s == dir
      getHistoryFile() map { history_file =>
        if(debug) println(s"history file : $history_file")
        val lines = history_file.getContent
        val res = 
          for(log <- lines;
              if(checkDir(log.dir))) yield log
        if(debug) println(s"content : $res")
        res
      } getOrElse (List[A]())
    }
    
    /**
     * Returns an history file.
     */
    private def getHistoryFile(): Option[LogWrapper[A]] = {
      val name = this.history_filename
      this.history_file match {
        case g@FileWrapper(history_file) => 
          if(if(!history_file.exists) history_file.createNewFile() else true) {
            Some(g)
          } else None
        case l@Lines(lines) => Some(l)
      }
    }
    
    /**
     * Remove the directory from a given command.
     */
    def removeDirectoryFromHistory(folder: File): Unit = {
      val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
      val checkDir = (s: String) => s != dir
      try {
        getHistoryFile() map { history_file =>
          val content = history_file.getContent
          val lines = 
            for(log <- content;
                if(checkDir(log.dir))) yield log
           history_file.setContent(lines)
        }
      } catch  {
        case e: IOException =>println("ioexception")
      }
    }
    
    /**
     * Stores a transaction for this folder in the history
     */
    def storeHistory(log: A): Unit = {
      getHistoryFile() foreach { history_file =>
        try {
          if(debug) println(s"Writing in history")
          //val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          history_file.addLine(log)
          //out.println(line);
          //out.close();
        } catch  {
          case e: IOException =>println("ioexception")
        }
      }
    }
    
    /**
     * Sets a line in the history to performed state.
     */
    def setHistoryPerformed(folder: File, files: List[String]): Unit = {
      val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
      val checkDir = (s: String) => s == dir
      try {
        getHistoryFile() foreach { history_file =>
          val content = history_file.getContent
          val lines: Seq[A] = 
            for(log <- content) yield {
              if(checkDir(log.dir) && log.input.startsWith(files) && !log.performed) {
                log.setPerformed(true): A
              } else log
          }
          history_file.setContent(lines)
        }
      } catch  {
        case e: IOException =>println("ioexception")
      }
    }

    /**
     * Deletes the log history.
     */
    def deleteHistory() = {
      getHistoryFile() map { history_file =>
        history_file.delete()
        if(debug) println("Deleted " + history_file.toString())
      }
    }
    
    def parseCmd(cmd: List[String], options: Options): Unit
    
    //def automateCmd(cmd):
  }
  trait Companion[T] {
    type C
    def apply() : C
  }
  object Companion {
    implicit def companion[T](implicit comp : Companion[T]) = comp()
  }
  trait LogFactory[C <: LogLine[C]] {
    implicit def companion: Companion[C]
  }
  
  /**
   * Renaming log
   */
  object MvLog extends LogFactory[MvLog] {
    def unapply(s: String): Option[MvLog] = {
      val a = s.split(";")
      try {
        val nature = a(3)
        val filesAndOutput = a.drop(4).toList
        val (input_files, output): (List[String], String) = nature match {
          case MultipleInputs(n) => (filesAndOutput.take(n), filesAndOutput.drop(n).head)
          case _ => (List(filesAndOutput.head), filesAndOutput.tail.head)
        }
        Some(MvLog(a(1), a(2).toBoolean, nature, input_files, output, a(0)))
      } catch {
        case e: java.lang.ArrayIndexOutOfBoundsException => None
      }
    }
    // Per-companion boilerplate for access via implicit resolution
    implicit def companion = new Companion[MvLog] {
      type C = MvLog.type
      def apply() = MvLog
    }
  }
  
  case class MvLog(dir: String, performed: Boolean, nature: String, file1AndProperties: List[String], file2: String, time: String = timeStampGiver()) extends LogLine[MvLog] {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + nature + ";" + file1AndProperties.mkString(";") +";"+file2
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = file1AndProperties
    def file1 = nature match {
      case INPUT_FILE => file1AndProperties.head
      case INPUT_FILE_EXTENSION => file1AndProperties.head + file1AndProperties.tail.head
      case INPUT_FILE_PROPERTIES(n) => file1AndProperties.head
    }
  }
  
  implicit object MvLogFile extends LogFileFactory[MvLog] {
    def history_filename = HISTORY_MV_FILE
    def extractor = (s: String, p: Option[Seq[ProgramSet[TraceExpr]]]) => MvLog.unapply(s).map(_.setPrograms(p))
    
    /**Enhanced move command
     * Feature list:
     * - "mv file1 file2" will move file1 to file2, and then 
     * - "mv" will look at the previous history and the last two moves, and perform a generalization to all files
     * - "mv -c" clears previous history
     * - "mv -e" explains
     * - "mv -t" tests the mv command.
     * @param options Various options to indicate how the filtering is performed.
     * 
     */
    def parseCmd(cmd: List[String], options: Options = Options()): Unit = {
      (cmd, options) match {
        case (("-c" | "--clear")::remaining, opt) => // Clears the history
          deleteHistory()
          //deleteFilterHistory()
          if(remaining != Nil) {
            parseCmd(remaining, opt)
            //println(remaining.mkString(" ") + " has been ignored")
          }
        case OptionsDecoder(opt2, cmd2) =>
          parseCmd(cmd2, opt2)
        case (Nil, opt) => // Automated move.
          if(opt.generalize) automatedMv(opt.copy(performAll = opt.perform))
        case (sfile1::sfile2::remaining, opt) =>
          val file1 = new File(workingDirAbsFile, sfile1)
          val file2 = new File(workingDirAbsFile, sfile2)
          val thefile = if(file1.exists()) file1 else file2
          if(!file1.exists() && opt.perform) { println(s"file $sfile1 does not exist"); return(); }
          if(!thefile.exists()) { println(s"Missing file $thefile"); return(); }
          
          var properties: List[String] = Nil
          val relative_sfile1 = relativizePath(workingDirAbsFile, file1.getAbsolutePath())
          val relative_sfile2 = relativizePath(workingDirAbsFile, file2.getAbsolutePath())
          val (nature, filename) = if(thefile.isDirectory()) {
            (INPUT_DIRECTORY, List(relative_sfile1))
          } else {
            if(opt.properties) {
              properties = readProperties(thefile)
              (INPUT_FILE_PROPERTIES(1+properties.length), List(relative_sfile1))
            } else {
              (INPUT_FILE_EXTENSION, splitExtension(relative_sfile1))
            }
          }
          val performed = (opt.perform && file1.exists()) || (!file1.exists() && file2.exists())
  
          storeHistory(MvLog(workingDirAbsFileFile.getAbsolutePath(), performed, nature, filename ++ properties, relative_sfile2))
          if(opt.perform && file1.exists()) move(sfile1, sfile2)
          if(debug) println("Explaining " + opt.explain + " performing " + opt.perform)
          if(remaining != Nil) {
            parseCmd(remaining, opt)
          } else if(opt.generalize) automatedMv(opt.copy(perform=(file1.exists && opt.performAll)))
        case (_, opt) => println("The mv command takes a multiple of two parameters. Options are --clear (-c), --explain (-e), --test (-t), --auto (-a)")
          //automatedMv(perform=opt.performAll, explain=opt.explain)
        case _ =>
      }
    }
    
    /**
     * Performs automated renaming suggestion
     * @param opt Various options for dealing with the history
     * @param examples A set of MvLogs used as example to perform the global renaming
     * Problem: Take the last instances, then the last two, etc.
     */
    def automatedMv(opt: Options, examples: Seq[MvLog] =  getHistory(workingDirAbsFileFile), solver: Option[StringSolver] = None): Option[StringSolver] = {
      val perform = opt.perform
      val explain = opt.explain
      val performAll = opt.performAll
      val c = solver.getOrElse(StringSolver())
      c.setTimeout(5)
      c.setVerbose(opt.debug)
      c.setIterateInput(false)
      val alreadyPerformed = (for(log <- examples if log.performed) yield log.file2).toSet
      if(debug) println(s"where=$workingDirAbsFile examples=$examples, perform=$perform; explain=$explain")
      
      if(examples != Nil && !(opt.minTwoExamples && examples.length < 2)) {
        if(!explain && !perform)
          println("# Looking for a general command...")
      } else {
        println("# No action to reproduce in this folder")
        return None;
      }
      
      val reverse_mapping = (for(log <- examples if log.performed) yield log.file2 -> log.file1).toMap
      
      val nested_level = examples.last match { case MvLog(dir, performed, nature, in, out, time) => in.map(_.count(_ == '/')).sum + in.map(_.count(_ == '\\')).sum}
      val onlyFiles = examples.lastOption.map(_.onlyFiles).getOrElse(true)
      val properties = opt.properties | examples.lastOption.map(_.isProperties).getOrElse(false)
      val withExtension = opt.properties | examples.lastOption.map(_.isExtension).getOrElse(false)
      if(debug) println(s"nested=$nested_level,onlyfiles=$onlyFiles")
      val files_raw: Array[List[String]] =
        listFiles(nested_level,
                  onlyFiles=onlyFiles,
                  filter=opt.filter,
                  extension=withExtension,
                  maybePrevious=(s: String) => reverse_mapping.getOrElse(s, s))
      
      val files = if(properties) {
        files_raw.map{listFiles =>
          val res = listFiles.mkString("")::readProperties(listFiles.mkString)
          if(debug) println(res)
          res
      }
      } else files_raw
      if(debug) println(s"available files:\n" + files.mkString("\n"))
      //if(solver == None)
      examples.reverse.take(2).reverse foreach {
        case m@MvLog(dir, performed, nature, in, out, time) =>
          
          val index = files.indexWhere(s => s.startsWith(in))
          val position = if(index == -1) Math.max(0, files.indexWhere(s => s.head + s.tail.head == out)) else index
          if(debug) println(s"Adding $in, $out at position $position")
          m.getPrograms match {
            case Some(p)=>c.add(p.toIndexedSeq)
            case None =>
              c.setPosition(position)
              val p = c.add(in, List(out))(0)
              m.setPrograms(Some(Seq(p)))
          }
      }
      if(debug) println(s"Files: ${files.mkString}")
      if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
  
      var mapping: Array[(List[String], Seq[String])] = null
      if(files.length != 0) {
        if(debug) println(s"Solving with at most 2")
        
        val attempts = (() => c.solve(),   (e: List[FileName]) => {c.solve(e)})::(if(!opt.minTwoExamples)
                       (() => c.solveLast(), (e: List[FileName]) => {c.solveLast(e)})::Nil else Nil)
        attempts find { case (computation, solver) => 
          c.resetCounter()
          if(debug) println(s"Finding computation")
          computation() match {
            case Some(Concatenate(List(ConstStr(a)))) => println(s"No generalization found. Aborting")
              true
            case Some(prog) =>
              if(!debug && (explain && !opt.test)) displayProg(prog, properties=properties)
              if(opt.produceBash) {
                // TODO : Produce bash code like this one:
                """find . \( -name '*.jpg' -o -name '*.png' \) -print  | (i=0; while read f; do 
                    let i+=1; mv "$f" "${f%/*}/$(printf %04d "$i").${f##*.}"; 
                done)
                """
              }
              if(!explain || opt.test || performAll) {
               // Solves the mapping for all files, even for those who were already done.
               val mappedFiles = files map { f => solver(f) }
               // Removes already performed tasks from the mapping
                mapping = files zip mappedFiles filterNot { case (file, m) => m.exists(_ == "") || alreadyPerformed(file.head + file.tail.head) }
                if(mapping.nonEmpty) {
                  if(performAll) {
                    for((file, to) <- mapping if !alreadyPerformed(file.head + file.tail.head)) {
                      move(file.head + file.tail.head, to.head)
                    } 
                  }
                  suggestMapping(List(prog), mapping map { case (a::b::l, e) => ((a+b)::l, e) case e => e }, "mv", title= !perform && !explain)
                }
              }
              if((opt.test && explain) || debug) displayProg(prog, properties=properties)
              OK
            case None =>
              if(debug) println(s"no program found")
              RETRY
          }
        }
      }
      if(perform) { // We remove the directory from the history.
         removeDirectoryFromHistory(workingDirAbsFileFile)
      }
      Option(c)
    }
  }
  
  /**
   * Auto log
   */
  object AutoLog extends LogFactory[AutoLog]{
    def unapply(s: String): Option[AutoLog] = {
      val a = s.split("\\|\\|").toList
      try {
        val filesAndCommand = a.drop(5)
        val nature = a(4)
        val (input_files, commands) = nature match {
          case MultipleInputs(n) => (filesAndCommand.take(n), filesAndCommand.drop(n))
          case _ => (List(filesAndCommand.head), filesAndCommand.tail)
        }
        Some(AutoLog(a(1), a(2).toBoolean, a(3).toBoolean, a(4), input_files, commands, a(0)))
      } catch {
	    case e: java.lang.IndexOutOfBoundsException => None
	  }
    }
    // Per-companion boilerplate for access via implicit resolution
    implicit def companion = new Companion[AutoLog] {
      type C = AutoLog.type
      def apply() = AutoLog
    }
  }
  
  case class AutoLog(dir: String, performed: Boolean, content: Boolean, nature: String, input_files: List[String], commands: List[String], time: String = timeStampGiver()) extends LogLine[AutoLog] {
    override def mkString = time + "||" + dir + "||" + performed.toString + "||" + content.toString + "||" + nature + "||"  + (input_files++commands).mkString("||")
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = input_files
  }
  
  implicit object AutoLogFile extends LogFileFactory[AutoLog] {
    def history_filename = HISTORY_AUTO_FILE
    def extractor = (s: String, p: Option[Seq[ProgramSet[TraceExpr]]]) => AutoLog.unapply(s).map(_.setPrograms(p))
    
      
    /**
     * Generic automated commands.
     * @param perform Indicates if the requested command is performed
     *   Parameter -t or --test put this variable to false
     *   Default: true
     * @param options Various options to indicate how the command is performed.
     */
    def parseCmd(cmd: List[String], options: Options = Options()): Unit = {
      if(debug) println(s"Action $cmd")
      (cmd, options) match {
        case (("-c" | "--clear")::remaining, opt) =>
          deleteHistory()
          if(remaining != Nil) {
            parseCmd(remaining, opt)
            //println(remaining.mkString(" ") + " has been ignored")
          }
        case OptionsDecoder(opt, cmd) =>
          parseCmd(cmd, opt)
        case (Nil, opt) =>
          if(opt.generalize) automatedAction(opt.copy(performAll=opt.performAll || (opt.perform && !opt.explain)))
        case (command::Nil, opt) => // Automatically detect where the file is and which one it should be applied to.
          val s = workingDirAbsFileFile.list().toList.sortBy(alphaNumericalOrder)
          val sNested = workingDirAbsFileFile.list().toList.flatMap{d => 
            val dir = new File(workingDirAbsFile, d)
            if(dir.isDirectory)
              dir.list().map(name => dir.getName()+"/"+name)
            else
              Nil
          }
          .sortBy(alphaNumericalOrder)
  
          
          if(command.indexOf("...") != -1) {
            if(debug) println("Found '...' Going to decode the action for all present files.")
            storeHistory(AutoLog(dir=workingDirAbsFileFile.getAbsolutePath(), performed=false, content=false, nature=INPUT_FILE_LIST(s.length), input_files=s, commands=command::Nil))
            parseCmd(Nil, opt)
          } else {
            if(debug) println("No input provided. Going to try to extract output from file name")
            val candidates = (s ++ sNested).filter{ f => command.indexOf(f) != -1 }.sortBy(_.size).lastOption
            candidates match {
              case Some(file) =>
                parseCmd(file::command::Nil, opt)
              case None =>
                // If there are three dots in the command, tries to recognize the command
            }
          }
        case (sfile::l, opt) if l != Nil =>
          val file1 = new File(workingDirAbsFile, sfile)
          if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
          
          val dir = workingDirAbsFileFile.getAbsolutePath()
          val nature = if(opt.contentFlag) {
            INPUT_FILE_CONTENT(1)
          } else if(file1.isDirectory()) {
            INPUT_DIRECTORY
          } else {
            INPUT_FILE
          }
          
          storeHistory(AutoLog(dir, false, opt.contentFlag, nature, List(sfile), l))
          if(l.indexWhere(_.indexOf("...") != -1) != -1 && opt.contentFlag) {
            if(debug) println("Found '...' Going to map the action for all lines in command.")
          }
          if(opt.generalize) automatedAction(opt)
         
        case _ =>
      }
    }
    
    /**
     * Performs automated renaming suggestion base on the history
     * @param performLast Indicates if the last action intended has to be performed
     * @param explain Indicates if the algorithm is explained. If both are false, displays the mapping.
     * @param performAll indicates if action is performed on all available examples
     * It always displays the mapping unless explain is set to true.
     */
    def automatedAction(opt: Options, examples: Seq[AutoLog] = getHistory(workingDirAbsFileFile)): Option[Array[(List[String], Seq[String])]] = {    val performLast = opt.perform
      val explain = opt.explain
      val performAll = opt.performAll
      val produceBash = opt.produceBash
  
      val c = StringSolver()
      c.setTimeout(3)
      c.setVerbose(debug)
      
      // Exceptions are files for which the action was already performed
      val alreadyPerformed = (for(log <- examples if log.performed; elem <- log.commands.last::log.input_files) yield elem).toSet
      if(debug) println(s"$workingDirAbsFile $examples, $performLast; $explain, $performAll")
      if(examples != Nil) (if(!explain && !performLast && !performAll) println("# Looking for mappings...")) else {
        println("# No action to reproduce in this folder")
        return None
      }
      
      // Controls the file nesting level
      val nested_level = examples.last match { case AutoLog(folder, performed, contentFlag, nature, files, command, time)  => files.head.count(_ == '/') + files.head.count(_ == '\\') }
      
      // Controls if the input for the (only) command is the list of files.
      var is_input_file_list = false 
      
      // Controls if the last example was reading the content of the file instead of the name itself.
      var read_content_file: Option[FileName] = None
      
      var lastFilesCommand: List[String] = Nil
      // All files should have the same nature.
      // onlyFile represents if the files are files (true) or directories (false)
      val onlyFiles = examples.lastOption.map(_.onlyFiles).getOrElse(true)
      
      // files_raw is a array of singletons list of files, either nested or not.
      val files_raw: Array[List[String]] = listFiles(nested_level, onlyFiles, filter=opt.filter, extension=false)
      
      //Replacing each file by its name and content if requested
      lazy val files_raw2: Array[List[String]] = if(read_content_file != None) {
        files_raw.map{listFiles => listFiles.head::readLines(readFile(listFiles.head))}
      } else files_raw
      
      //Replacing the list of singleton files by one unique input containing all files
      lazy val input = if(is_input_file_list && read_content_file == None) {
        Array(files_raw2.toList.flatten)
      } else files_raw2
      
      // Solving the mapping based on the last two examples.
      examples.reverse.take(2).reverse foreach {
        case a@AutoLog(folder, performed, contentFlag, nature, files, command, time) =>
          // Extra time if looking for dots
          if(command.indexOf("...") != -1) {
            c.setTimeout(4)
            c.setExtraTimeToComputeLoops(2f)
          } 
          // If the last term in the command is a file, it is in exceptions
          if(!performed) lastFilesCommand = files
          nature match {
            case INPUT_FILE_LIST(n) =>
              is_input_file_list = true
              read_content_file = None
            case INPUT_FILE_CONTENT(n) =>
              is_input_file_list = false
              read_content_file = Some(files.head) // Only read this file
            case INPUT_DIR_CONTENT(n) =>
              is_input_file_list = false
              read_content_file = Some(files.head) // Only read this file
            case _ =>
              is_input_file_list = false
              read_content_file = None
          }
          a.getPrograms match {
            case Some(p) => c.add(p.toIndexedSeq)
            case None =>
            // Retrieving the input, either the name of the file or the lines of it if the contentFlag is set.
            // Takes only maximum NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED files or lines.
            val (inputList,output) = if(contentFlag) {
              val content = readFile(files.head)
              if(!content.isEmpty) {
                val lines = readLines(content).take(NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED)
                if(debug) println(s"Adding ${lines} (content=$contentFlag), $command")
                (files.head::lines, command)
              } else (Nil, Nil)
            } else {
              if(debug) println(s"Adding ${files.take(NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED)} (content=$contentFlag), $command")
              (files.take(NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED), command)
            }
            c.setPosition(input.indexWhere(s => s.startsWith(inputList)))
            val p = c.add(inputList, output)
            a.setPrograms(Some(p))
          }
      }
      
      if(debug) println(s"Only files: $onlyFiles, nested level = $nested_level")
  
      if(debug) println(s"Input ${input.mkString("")}")
      var mapping: Array[(List[String], Seq[String])] = null
      if(input.length != 0) {
        if(debug) println(s"Solving with at most 2 and then 1")
        
        val attempts = (() => c.solveAll(),   (e: List[FileName]) => c.solve(e))::
                       (() => c.solveLasts(), (e: List[FileName]) => c.solveLast(e))::Nil
        attempts find { case (computation, solver) => 
          c.resetCounter()
          computation() match {
            case List(Some(Concatenate(List(ConstStr(a))))) => RETRY
            case l if l.forall(_.nonEmpty) =>
              if(explain || debug) {
                for(ls <- l; prog <- ls) {
                 displayProg(prog, lines = (read_content_file != None), folder = !onlyFiles)
                }
              }
              if(!explain || opt.test || performAll) {
                mapping = 
                  for(f <- input;
                      commandFromFile = solver(f)
                      if commandFromFile forall (_.nonEmpty)) yield {
                    f -> commandFromFile
                  }
                suggestMapping(l.map(_.get), mapping, "auto", title= !performAll)
                if(performAll) {
                  if(debug) println("Performing all of them")
                  for((file, command) <- mapping
                      if is_input_file_list || !alreadyPerformed(file.head)
                  ) {
                    auto(command.toList)
                  }
                } else if(performLast) {
                  if(debug) println("Performing the last one")
                  // Performs the last of the provided examples and replace in the history the action.
                  mapping find {
                    case (files, command) if files.startsWith(lastFilesCommand) => true
                    case _ => false
                  } match {
                    case Some((files, command)) =>
                      auto(command.toList)
                      // Put in the history that the action has been made.
                      setHistoryPerformed(workingDirAbsFileFile, files)
                    case None => // Nothing to execute
                    if(debug) println(s"The last one $lastFilesCommand has already been done or was not found in keys of mapping.")
                  }
                }
              }
              OK
            case _ =>
              if(debug) println(s"No program found")
              RETRY
          }
        }
      }
      if(performAll) {
        removeDirectoryFromHistory(workingDirAbsFileFile)
      }
      Option(mapping)
    }
  }
  
  /**
   * Partition log
   */
  object PartitionLog extends LogFactory[PartitionLog] {
    def unapply(s: String): Option[PartitionLog] = {
      val a = s.split(";")
      try {
        Some(PartitionLog(a(1), a(2).toBoolean, a(3), a(4), a(5), a(0)))
      } catch {
	    case e: java.lang.ArrayIndexOutOfBoundsException => None
	  }
    }
    // Per-companion boilerplate for access via implicit resolution
    implicit def companion = new Companion[PartitionLog] {
      type C = PartitionLog.type
      def apply() = PartitionLog
    }
  }
  
  case class PartitionLog(dir: String, performed: Boolean, nature: String, file1: String, folder: String, time: String = timeStampGiver()) extends LogLine[PartitionLog] {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + nature + ";" + file1+";" + folder
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = List(file1)
  }
  
  implicit object PartitionLogFile extends LogFileFactory[PartitionLog] {
    def history_filename = HISTORY_PARTITION_FILE
    def extractor =  (s: String, p: Option[Seq[ProgramSet[TraceExpr]]]) =>  PartitionLog.unapply(s)
    
    
  /**
   * Generic automated partitioning.
   * @param perform Indicates if the requested command is performed
   *   Parameter -t or --test put this variable to false
   *   Default: true
   * @param options Various options to indicate how the partitioning is performed.
   */
  def parseCmd(cmd: List[String], options: Options = Options()): Unit = {
    if(debug) println(s"Action $cmd")
    (cmd, options) match {
      case (("-c" | "--clear")::_, opt) =>
        deleteHistory()
      case OptionsDecoder(opt, cmd) =>
        parseCmd(cmd, opt)
      case (Nil, opt) =>
        if(opt.generalize) automatedPartition(opt.copy(performAll=opt.performAll || (opt.perform && !opt.explain)))
      case (sfile::sfolder::remaining, opt) =>
        val file1 = new File(workingDirAbsFile, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        val dir = workingDirAbsFileFile.getAbsolutePath()
        val nature = if(file1.isDirectory()) {
          INPUT_DIRECTORY
        } else {
          INPUT_FILE
        }
        storeHistory(PartitionLog(dir, opt.perform, nature, sfile, sfolder))
        if(opt.perform) move(sfile,sfolder+"/"+ sfile)
        if(remaining == Nil) {
          if(opt.generalize) automatedPartition(opt)
        } else {
          parseCmd(remaining, opt)
        }
      case _ =>
    }
  }
  
    /**
   * Performs automated partitioning
   * @param opt Various options for dealing with the history
   * @return A mapping given the index
   */
  def automatedPartition(opt: Options, examples: Seq[PartitionLog] = getHistory(workingDirAbsFileFile)): Option[Array[(List[String], Seq[String])]] = {
    val perform = opt.perform
    val explain = opt.explain
    val alreadyPerformed = (for(ex <- examples if ex.performed) yield ex.file1).toSet
    if(debug) println(s"$workingDirAbsFile $examples, $perform; $explain")
    
    if(examples != Nil) (if(!explain && !perform) println("# Looking for a general partition command...")) else {
      println("# No action to reproduce in this folder")
      return None;
    }
    val lastFilesCommand: List[String] = examples.last match { case PartitionLog(dir, performed, nature, in, out, time) => List(in) }
    val nested_level: Int = examples.head match { case PartitionLog(dir, performed, nature, in, out, time) => in.count(_ == '/') + in.count(_ == '\\') }

    val examples_for_partition = examples.map { case PartitionLog(dir, performed, nature, in, out, time) => (in, out) }
    val (c, c2, getCategory) = Service.getPartition(examples_for_partition, StringSolver().setTimeout(5), StringSolver(), opt) getOrElse {
       println("# No partition found.")
       return None
    }
    
    if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
    val onlyFiles = examples.lastOption.map(_.onlyFiles).getOrElse(true)
    
    val files: Array[List[FileName]] = listFiles(nested_level, onlyFiles, filter=opt.filter, extension=false)
    
    if(debug) println(s"Files: $files")
    var mapping: Array[(List[String], Seq[String])] = null
    if(files.length != 0) {
      //println("Solving problem...")
      if(debug) println(s"Solving with at most 2")
      
      val attempts = (() => c.solve(),   (e: List[FileName]) => c.solve(e))::Nil
      attempts find { case (computation, solver) => 
        c.resetCounter()
        computation() match {
          case Some(Concatenate(List(ConstStr(a)))) => true
          case Some(prog) =>
            if(debug || explain) {
              displayProg(prog, prefix = "Groups files by ", suffix = ".")
              c2.flatMap(_.solve()) match {
                case Some(prog2) =>
                  displayProg(prog2, category=true, prefix = "The name of the category is ", suffix = ".")
                case _ =>
                  println("# The name of the category is the result of the grouping program above")
              }
            }
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
            }
            if(!explain || opt.test) {
              val mappedFiles = files map { f => { val tmp = solver(f); if(tmp.length == 1) Seq(getCategory(tmp.head)) else Seq("") } }
              mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") }
              if(mapping.nonEmpty) {
                /*if(perform) {
                  for((file, to) <- mapping if !alreadyPerformed(file.head)) {
                    move(file.head, to.head)
                  }
                }*/
                suggestMapping(List(prog), mapping, Partition(), title= !perform)
                if(opt.performAll) {
                  if(debug) println("Performing all of them")
                  for((file, to) <- mapping
                      if !alreadyPerformed(file.head)
                  ) {
                    move(file.head, to.head + "/" + file.head)
                  }
                } else if(opt.perform) {
                  if(debug) println("Performing the last one")
                  // Performs the last of the provided examples and replace in the history the action.
                  mapping find {
                    case (files, to) if files.startsWith(lastFilesCommand) => true
                    case _ => false
                  } match {
                    case Some((file, to)) =>
                      move(file.head, to.head + "/" + file.head)
                      // Put in the history that the action has been made.
                      setHistoryPerformed(workingDirAbsFileFile, file)
                    case None => // Nothing to execute
                    if(debug) println(s"The last one $lastFilesCommand has already been done or was not found in keys of mapping.")
                  }
                }
                
                
              }
            }
            OK
          case None =>
            if(debug) println(s"no program found")
            RETRY
        }
      }
    }
    if(opt.performAll) { // We remove the directory from the history.
      removeDirectoryFromHistory(workingDirAbsFileFile)
    }
    Option(mapping)
  }
  
  }
  
  /**
   * Filter log for filtering file or folder names or file line.
   */
  object FilterLog extends LogFactory[FilterLog] {
    def unapply(s: String): Option[FilterLog] = {
      val a = s.split(";")
      try {
        Some(FilterLog(a(1), a(2).toBoolean, a(3), a(4), a(5), a(0)))
      } catch {
	      case e: java.lang.ArrayIndexOutOfBoundsException => None
	    }
    }
    // Per-companion boilerplate for access via implicit resolution
    implicit def companion = new Companion[FilterLog] {
      type C = FilterLog.type
      def apply() = FilterLog
    }
  }
  
  case class FilterLog(dir: String, performed: Boolean, nature: String, file1: String, folder: String, time: String = timeStampGiver()) extends LogLine[FilterLog] {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + nature + ";" + file1+";"+folder
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = List(file1)
    
    private var mFilterSolver: Option[(StringSolver, String)] = None
    def getFilterSolver: Option[(StringSolver, String)] = mFilterSolver
    def setFilterSolver(c: StringSolver, expected: String): Unit = {
      mFilterSolver = Some((c, expected))
    }
  }
  
  implicit object FilterLogFile extends LogFileFactory[FilterLog] {
    def history_filename = HISTORY_FILTER_FILE
    def extractor =  (s: String, p: Option[Seq[ProgramSet[TraceExpr]]]) => FilterLog.unapply(s)
    
    /**
     * Generic automated filtering.
     * @param perform Indicates if the requested command is performed
     *   Parameter -t or --test put this variable to false
     *   Default: true
     * @param options Various options to indicate how the filtering is performed.
     */
    def parseCmd(cmd: List[String], options: Options = Options(perform=false,explain=true,test=true)): Unit = {
      if(debug) println(s"Action $cmd, options = $options")
      (cmd, options) match {
        case (("-c" | "--clear")::remaining, opt) =>
          deleteHistory()
          if(remaining != Nil) {
            parseCmd(remaining, opt)
          }
        case OptionsDecoder(opt, cmd) =>
          parseCmd(cmd, opt)
        case (Nil, opt) =>
          if(opt.generalize) automatedFilter(opt.copy(performAll=opt.performAll || (!opt.explain && !opt.test)))
        case (sfileOrLine::sfolder::remaining, opt) =>
          val file1 = new File(workingDirAbsFile, sfileOrLine)
          if(!file1.exists() && !options.contentFlag) { println(s"file $sfileOrLine does not exist"); return(); }
          val dir = workingDirAbsFileFile.getAbsolutePath()
          val nature = if(options.contentFlag) {
            INPUT_LINE
          } else if(file1.isDirectory()) {
            INPUT_DIRECTORY
          } else {
            INPUT_FILE
          }
          storeHistory(FilterLog(dir, opt.perform, nature, sfileOrLine, sfolder))
          if(opt.perform && !options.contentFlag) move(sfileOrLine,sfolder+"/"+ sfileOrLine)
          
          if(remaining == Nil) {
            if(opt.generalize) automatedFilter(opt)
          } else {
            parseCmd(remaining, opt)
          }
        case _ =>
      }
    }
  
    
    /**
     * Performs automated partitioning
     * @param opt Various options for dealing with the history
     * @return A mapping given the index
     */
    def automatedFilter(opt: Options, examples: Seq[FilterLog] = getHistory(workingDirAbsFileFile)): Option[(Array[(List[String], Seq[String])], String => Boolean)] = {
      val perform = opt.perform
      val explain = opt.explain
      
      val alreadyPerformed = (for(ex <- examples if ex.performed) yield ex.file1).toSet
      if(debug) println(s"$workingDirAbsFile, examples=$examples, perform=$perform; explain=$explain")
      
      if(examples != Nil) (if(!explain && !perform) println("# Looking for a general filtering command...")) else {
        println("# No action to reproduce in this folder")
        return None;
      }
      if(debug) println(s"Listing files...")
      var lastFilesCommand: List[String] = examples.last match { case FilterLog(dir, performed, nature, in, out, time) => List(in) }
      val (firstCategory: String, nested_level: Int) = examples.head match { case FilterLog(dir, performed, nature, in, out, time) => (out, in.count(_ == '/') + in.count(_ == '\\')) }
      val otherCategory: String = examples.map(_.folder) find { _ != firstCategory} match {
        case Some(str) => str
        case None => println("# Must provide at least one example in each filter partition"); return None
      }
      val examples_for_service = examples map { case FilterLog(dir, performed, nature, in, out, time) => (in, out == firstCategory) }
      if(debug) println(s"Invoking service command with ${examples_for_service.length}...")
      val (c, determiningSubstring) = Service.getFilter(examples_for_service, StringSolver().setTimeout(5), opt).getOrElse{
        println("# Unable to filter. Please perform filter --clean to reset filtering option")
        return None
      }
      examples.last.setFilterSolver(c, determiningSubstring)
      
      if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
      
      val onlyFiles = examples.lastOption.map(_.onlyFiles).getOrElse(true)
      val files: Array[List[FileName]] = if(opt.contentFlag) {
        listFiles(nested_level, onlyFiles, filter=false, extension=false) // Do not take a previous filter command, it would not make any sense.
      } else {
        Array[List[FileName]]()
      }
      if(debug) println(s"Files: $files")
      var mapping: Array[(List[String], Seq[String])] = null
      if(true || files.length != 0) {
        //println("Solving problem...")
        if(debug) println(s"Solving with at most 2")
        
        val attempts = (() => c.solve(),   (e: List[FileName]) => c.solve(e))::Nil
        attempts find { case (computation, solver) => 
          c.resetCounter()
          computation() match {
            case Some(Concatenate(List(ConstStr(a)))) => RETRY
            case Some(prog) =>
              if(debug || explain) {
                if(opt.contentFlag) {
                  displayProg(prog, lines=true, ifLinesFirstIsFile=false, prefix = s"Keep lines if ", suffix = s" is equal to '$determiningSubstring', and discard them otherwise.")
                } else {
                  displayProg(prog, prefix = s"Moves files to $firstCategory if ", suffix = s" is equal to '$determiningSubstring', and moves them to $otherCategory otherwise.")
                }
              }
              if(opt.produceBash) {
                // TODO : Produce bash code like this one:
                """find . \( -name '*.jpg' \) -print"""
              }
              if((!explain || opt.test) && files.nonEmpty) {
                val mappedFiles = files map { f => { val tmp = solver(f); if(tmp.length == 1 && tmp.head == determiningSubstring) Seq(firstCategory) else Seq(otherCategory) } }
                mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") }
                if(mapping.nonEmpty) {
                  suggestMapping(List(prog), mapping, Filter(), title= !perform)
                  if(opt.performAll) {
                    if(debug) println("Performing all of them")
                    for((file, to) <- mapping
                        if !alreadyPerformed(file.head)
                    ) {
                      move(file.head, to.head + "/" + file.head)
                    }
                  } else if(opt.perform) {
                    if(debug) println("Performing the last one")
                    // Performs the last of the provided examples and replace in the history the action.
                    mapping find {
                      case (files, to) if files.startsWith(lastFilesCommand) => true
                      case _ => false
                    } match {
                      case Some((file, to)) =>
                        move(file.head, to.head + "/" + file.head)
                        // Put in the history that the action has been made.
                        setHistoryPerformed(workingDirAbsFileFile, file)
                      case None => // Nothing to execute
                      if(debug) println(s"The last one $lastFilesCommand has already been done or was not found in keys of mapping.")
                    }
                  }
                  
                  
                }
              }
              OK
            case None =>
              if(debug) println(s"no program found. Continuing")
              RETRY
          }
        } match {
          case Some(_) =>
          case None =>
            println(s"# No filter function found")
        }
      }
      if(opt.performAll) { // We remove the directory from the history.
        removeDirectoryFromHistory(workingDirAbsFileFile)
      }
      Option((mapping, { (s: String) =>
        c.resetCounter();
        val res = c.solve(s) == determiningSubstring;
        //println(c.solve());
        //println(s"Solving for $s. Result is ${c.solve(s)} == $determiningSubstring which is $res")
        res
      }
      ))
    }
  }
  
  /**
   * Filter log for filtering file or folder names or file line.
   */
  object MapLog extends LogFactory[MapLog] {
    def unapply(s: String): Option[MapLog] = {
      val a = s.split("\\|\\|\\|")
      try {
        Some(MapLog(a(1), a(2).toBoolean, a(3), a(4), a(5), a(0)))
      } catch {
	      case e: java.lang.ArrayIndexOutOfBoundsException => None
	    }
    }
    // Per-companion boilerplate for access via implicit resolution
    implicit def companion = new Companion[MapLog] {
      type C = MapLog.type
      def apply() = MapLog
    }
  }
  
  case class MapLog(dir: String, performed: Boolean, nature: String, line_in: String, line_out: String, time: String = timeStampGiver()) extends LogLine[MapLog] {
    override def mkString = time + "|||" + dir + "|||" + performed.toString + "|||" + nature + "|||" + line_in+"|||"+line_out
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = List(line_in)
  }
  
  implicit object MapLogFile extends LogFileFactory[MapLog] {
    def history_filename = HISTORY_MAP_FILE
    def extractor =  (s: String, p: Option[Seq[ProgramSet[TraceExpr]]]) => MapLog.unapply(s)
    
    /**
     * Generic automated filtering.
     * @param perform Indicates if the requMapcommand is performed
     *   Parameter -t or --test put this variable to false
     *   Default: true
     * @param options Various options to indicate how the filtering is performed.
     */
    def parseCmd(cmd: List[String], options: Options = Options()): Unit = {
      if(debug) println(s"Action $cmd, options = $options")
      (cmd, options) match {
        case (("-c" | "--clear")::remaining, opt) =>
          deleteHistory()
          if(remaining != Nil) {
            parseCmd(remaining, opt)
          }
        case OptionsDecoder(opt, cmd) =>
          parseCmd(cmd, opt)
        case (Nil, opt) =>
          if(opt.generalize) automatedMap(opt.copy(performAll=opt.performAll || (!opt.explain && !opt.test)))
        case (line_input::line_output::remaining, opt) =>
          //val file1 = new File(workingDirAbsFile, sfileOrLine)
          //if(!file1.exists() && !options.contentFlag) { println(s"file $sfileOrLine does not exist"); return(); }
          val dir = workingDirAbsFileFile.getAbsolutePath()
          val nature = INPUT_LINE
          storeHistory(MapLog(dir, opt.perform, nature, line_input, line_output))
          
          if(remaining == Nil) {
            if(opt.generalize) automatedMap(opt)
          } else {
            parseCmd(remaining, opt)
          }
        case (sfile::Nil, opt) =>
          val file1 = new File(workingDirAbsFile, sfile)
          if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
          
          val perform = opt.perform
          //if(perform) {
            //mapFile(file1)
            //storeHistory(MvLog(workingDirAbsFileFile.getAbsolutePath(), perform, INPUT_FILE, sfile, relative_sfile2))
          //}
          if(opt.generalize) automatedMap(opt.copy(file = Some(file1)))
        case _ =>
      }
    }
    
    /**
     * Performs automated partitioning
     * @param opt Various options for dealing with the history
     * @return A mapping given the index
     */
    def automatedMap(opt: Options, examples: Seq[MapLog] = getHistory(workingDirAbsFileFile)): Option[Array[(List[String], Seq[String])]] = {
      val perform = opt.perform
      val explain = opt.explain
      val performAll = opt.performAll
      val c = StringSolver()
      c.setTimeout(5)
      c.setVerbose(opt.debug)
      c.setIterateInput(false)
      //c.setExtraTimeToResolve(4f)
      //c.setExtraTimeToMerge(4f)
      if(debug) println(s"where=$workingDirAbsFile examples=$examples, perform=$perform; explain=$explain")
      
      if(examples != Nil && !(opt.minTwoExamples && examples.filter(_.nature == INPUT_LINE).length < 2)) {
        if(!explain && !perform)
          println("# Looking for a general mapping...")
      } else {
        println("# No enough input/output examples given for this file. Aborting")
        return None;
      }
      
      //val reverse_mapping = (for(log <- examples if log.performed) yield log.line_out -> log.line_in).toMap

      val lines_raw: Array[List[String]] = opt.file match {
        case Some(file) => listFileContent(file, opt.filter)
        case None => Array[List[String]]()
      }
      val lines = lines_raw
      if(debug) println(s"available lines:\n" + lines.mkString("\n"))
      //if(solver == None)
      examples foreach {
        case m@MapLog(dir, performed, nature, in, out, time) =>
          val index = lines.indexWhere(s => s.startsWith(List(in)))
          val position = index
          if(debug) println(s"Adding $in, $out at position $position")
          m.getPrograms match {
            case Some(p)=>c.add(p.toIndexedSeq)
            case None =>
              c.setPosition(position)
              val p = c.add(List(in), List(out))(0)
              m.setPrograms(Some(Seq(p)))
          }
      }
  
      var mapping: Array[(List[String], Seq[String])] = null
      val attempts = (() => c.solve(),   (e: List[FileName]) => {c.solve(e)})::Nil
      attempts find { case (computation, solver) => 
        c.resetCounter()
        if(debug) println(s"Finding mapping")
        computation() match {
          case Some(Concatenate(List(ConstStr(a)))) => println(s"No generalization found. Aborting")
            true
          case Some(prog) =>
            if((!debug && explain && !opt.test) || opt.file.isEmpty) displayProg(prog, lines=true, ifLinesFirstIsFile=false, prefix="Maps each line to ", suffix=opt.file.map("in " + _.getName).getOrElse(""))
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
              """find . \( -name '*.jpg' -o -name '*.png' \) -print  | (i=0; while read f; do 
                  let i+=1; mv "$f" "${f%/*}/$(printf %04d "$i").${f##*.}"; 
              done)
              """
            }
            opt.file match {
              case None => // Do nothing.
              case Some(f) =>
              if(!explain || opt.test || performAll) {
               // Solves the mapping for all lines covered by the filter.
               val mappedLines = lines map { case List(line, "false") => List(line) case line::_ => solver(List(line)) }
               // Removes already performed tasks from the mapping
                mapping = lines zip mappedLines
                if(mapping.nonEmpty) {
                  if(perform) {
                    // Overwrite the content.
                    val out = new PrintWriter(new BufferedWriter(new FileWriter(f.getAbsoluteFile(), false)));
                    out.println(mappedLines.map(_.head).mkString("\n"))
                    out.close
                  }
                  /*if(performAll) {
                  }*/
                  suggestMapping(List(prog), mapping map { case (a::l, e) => (a::Nil, e) case e => e }, "mv", title= !perform && !explain)
                }
              }
              if((opt.test && explain) || debug) displayProg(prog, lines=true, ifLinesFirstIsFile=false, prefix="Maps each line to ", suffix=s"in ${f.getName}")
            }
            OK
          case None =>
            if(debug) println(s"no program found")
            RETRY
        }
      }
      if(performAll) { // We remove the directory from the history.
        removeDirectoryFromHistory(workingDirAbsFileFile)
      }
      Option(mapping)
    }
  }
  
  def splitExtension(s: String): List[String] = {
       s.split("\\.(?=[^\\.]+$)") match {
        case Array(a, b) => List(a,"."+b)
        case _ => List(s,"")
      }
  }
  
  /**
   * Lists files or directory at a given nested level (0 or 1)
   * 
   * Sorts the files by alphanumerical order, mapping files 
   * 
   * @param nested_level How many directories can be found in front of the file
   * @param onlyFiles if only files are listed, or only directories
   * @param filter if the last filter -t command is used to filter the files
   * @param extension If set, will split each file into its base name and its extension.
   * 
   * 
   */
  def listFiles(nested_level: Int, onlyFiles: Boolean, filter: Boolean, extension: Boolean, maybePrevious: String => String = (s: String) => s): Array[List[String]] = {
    val f = if(nested_level == 0) {
      fileListerString().sortBy(f => alphaNumericalOrder(maybePrevious(f)))
      .filter(file => new File(workingDirAbsFile, file).isDirectory() ^ onlyFiles)
      .map(file => if(extension) splitExtension(file) else List(file))
    } else if(nested_level == 1){
      fileLister().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().sortBy(f => alphaNumericalOrder(maybePrevious(f)))
        .map(file => theDir.getName() + "/" + file)
        .filter(file => new File(workingDirAbsFile, file).isDirectory() ^ onlyFiles)
        .map(file => if(extension) splitExtension(file) else List(file))
      }
    } else Array[List[String]]()
    val res = if(filter) {
      val loghistory = FilterLogFile.getHistory(workingDirAbsFileFile) // Take the last filter which contains everything.
      loghistory.lastOption match {
        case Some(filterLine) if filterLine.getFilterSolver != None =>
          val Some((c, expected)) = filterLine.getFilterSolver
          f.filter { elem => c.solve(elem.mkString("")) == expected }
        case _ =>
          FilterLogFile.automatedFilter(Options(perform=false, performAll=false, explain=false, test=false), loghistory) match {
          case Some((filters, _)) =>
            val okString = loghistory(0).folder
            val accepted = filters.filter{ case (lin, lout) => lout.head == okString}.map(_._1).toSet
            f filter { elem => accepted(List(elem.mkString(""))) }
          case None => f
        }
      }
    } else {
      f
    }
    res
  }
  
  /**
   * Lists the content of a file or a directory
   * 
   * No sorting is done
   * 
   * @param file The name of the file
   * @param filter If filtering is performed, i.e. if the second element of input is "true" or "false", depending on if the line is considered.
   * @return The array of List(line, true | false) where the second element means if the line is considered or not.
   */
  def listFileContent(file: File, filter: Boolean): Array[List[String]] = {
    val lines = readFile(file).split("\n").map(List(_))
    val res = if(filter) {
      val loghistory = FilterLogFile.getHistory(workingDirAbsFileFile).filter(_.nature == INPUT_LINE) // Take the last filter which contains everything.
      loghistory.lastOption match {
        case Some(filterLine) if filterLine.getFilterSolver != None =>
          val Some((c, expected)) = filterLine.getFilterSolver
          lines.map { elem => elem ++ List( (c.solve(elem.mkString("")) == expected).toString) }
        case _ =>
          FilterLogFile.automatedFilter(Options(perform=false, performAll=false, explain=true, test=false, contentFlag=true), loghistory) match {
          case Some((filters, filterCmd)) =>
            val accepted = filterCmd
            //val okString = loghistory(0).folder
            //val accepted = filters.filter{ case (lin, lout) => lout.head == okString}.map(_._1).toSet
            lines.map { elem => elem ++ List(accepted(elem.mkString("")).toString) }
          case None => lines
        }
      }
    } else {
      lines
    }
    res
  }
  
  /**
   * Moves a file/directory to another relative location.
   * Creates the repository if necessary.
   */
  def move(file: String, to: String) = {
    if(debug) println(s"Moving $file to $to")
    val directoryName = if(to.indexOf("/") != -1 || to.indexOf("\\") != -1) {
      to.getDirectory
    } else workingDirAbsFile
    val theDir = new File(directoryName);

    if (!theDir.exists()) {
      val result = theDir.mkdir();  
    }
    new File(workingDirAbsFile, file).renameTo(new File(workingDirAbsFile, to))
    
    if(file.indexOf("/") != -1 || file.indexOf("\\") != -1) {
      val dir = new File(workingDirAbsFile, file.getDirectory)
      if(dir.list().length == 0) {
        dir.delete()
      }
    }
    //s"move $file $to".!!
    //println(s"move $file $to")
  }
  
  /**
   * Performs a list of command
   */
  def auto(cmd: List[String]): Unit = {
    val cmdString_raw = "\""+cmd.mkString(";").replaceAll("\"","\\\"")+"\""
    
    val p = if(System.getProperty("os.name").contains("indow")) {
      val cygwinbindir = "C:\\cygwin\\bin"
      val cygwinbash = cygwinbindir + "\\bash.exe"
      
      if(!new File(cygwinbash).exists) {
        println(s"Cygwin bin directory not found $cygwinbindir")
        return
      }
      val cmdString = cmdString_raw.replaceAll("""\bconvert """, """convert.exe """)
      
      val env = collection.mutable.Map[String, String]() ++ System.getenv();
      val key = if(env contains "Path") "Path" else if(env contains "PATH") "PATH" else throw new Error("No Path or PATH variable found in environment")
      env(key) += ";"+cygwinbindir+";c:\\Windows\\System32"
      val envString = (env.map{ case (k, v) => k + "=" + v }).toArray
      if(debug) println("Calling runtime.exec with: " + List(Array[String](cygwinbash,"-c",cmdString) mkString " "))
      
      Runtime.getRuntime().exec(Array[String](cygwinbash,"-c",cmdString),
                                 envString,
                                 workingDirAbsFileFile);
    } else {
      Runtime.getRuntime().exec(Array("/bin/bash", "-c", cmdString_raw), Array[String](), workingDirAbsFileFile)
    }
    val input = new BufferedReader(new InputStreamReader(p.getInputStream()));
      var line = input.readLine()
      while (line != null) {
        System.out.println(line);
        line = input.readLine()
      }
      input.close();
      p.waitFor();
  }
  
    
  /**
   * Displays a mapping with an optional title
   */
  def suggestMapping(prog: List[Program], mapping: Array[(List[String], Seq[String])], command: String, title: Boolean = true) = {
    val t = if(title) s"#(Mapping found. Type '$command' to perform it, or '$command -e' to explain)  \n" else ""
    val mappingTrimmed = mapping//.toList.sortBy(_.##).take(5)
    val m = (mappingTrimmed map {
      case (i: List[FileName], j: Seq[String]) =>
        val res = s"# ${i mkString ","} -> ${j.mkString(";")}"
        if(res.length > 50) res.replace(" -> ", " -> \n#>") else res
      } mkString "\n"
      
    )
    println(t+m)
    if(mappingTrimmed.size < mapping.size) println("...")
  }
  
  /**
   * Displays a program with various options
   * @param prog The program to display
   * @param lines If true, considers that the first input is the file name, and all inputs number n+1 are line n
   * @param folder If true, considers that the first input is the folder name, and all inputs number n+1 are file n
   * @param properties If true, considers that the first input is the file name, and the remaining inputs are (owner, year, month, day, hour, minut, second)
   * @param prefix Prefix to display before the name of the program.
   * @param suffix Suffix to add after the name of the program.
   */
  def displayProg(prog: Program, lines: Boolean = false, folder: Boolean = false, properties: Boolean = false, category: Boolean = false, ifLinesFirstIsFile: Boolean = true, prefix: String = "", suffix: String = ""): Unit = {
    val replaceinput = if(lines) "line" else if(folder) "folder name" else if(category) "common string" else "file name"
    val replaceallinputs = if(!lines && !folder && !category) "both file name and its extension" else s"all ${replaceinput}s"
    val tmp = Printer(prog)
    val first = if("(?<!first) input".r.findFirstIn(tmp) != None && (lines || folder) && !properties){ "first " }  else ""
    val second = if(lines && ifLinesFirstIsFile) "first" else "second"
    val third = if(lines && ifLinesFirstIsFile) "second" else "third"
    val fourth = if(lines && ifLinesFirstIsFile) "third" else "fourth"
    val fifth = if(lines && ifLinesFirstIsFile) "fourth" else "fifth"
    val sixth = if(lines && ifLinesFirstIsFile) "fifth" else "sixth"
    val seventh = if(lines && ifLinesFirstIsFile) "sixth" else "seventh"
    val eighth = if(lines && ifLinesFirstIsFile) "seventh" else "eighth"
    val firstinput = if(lines && ifLinesFirstIsFile) "file name" else s"${first}$replaceinput"
    val secondinput = if(properties) "owner" else if(!lines && !folder) "extension" else s"$second $replaceinput"
    val thirdinput = if(properties) "year" else s"$third $replaceinput"
    val fourthinput = if(properties) "month" else s"$fourth $replaceinput"
    val fifthinput = if(properties) "day" else s"$fifth $replaceinput"
    val sixthinput = if(properties) "hour" else s"$sixth $replaceinput"
    val seventhinput = if(properties) "minut" else s"$seventh $replaceinput"
    val eighthinput = if(properties) "second" else s"$eighth $replaceinput"
    val lastinput = if(properties) "second" else s"last $replaceinput"
    val penultimateinput = if(properties) "minut" else s"penultimate $replaceinput"
    val antepenultimateinput = if(properties) "hour" else s"antepenultimate $replaceinput"
    
    println("# " +prefix+ tmp.replaceAll("first input", firstinput)
        .replaceAll("all inputs", replaceallinputs)
        .replaceAll("second input", s"$secondinput")
        .replaceAll("third input", s"$thirdinput")
        .replaceAll("4th input", s"$fourthinput")
        .replaceAll("5th input", s"$fifthinput")
        .replaceAll("6th input", s"$sixthinput")
        .replaceAll("7th input", s"$seventhinput")
        .replaceAll("8th input", s"$eighthinput")
        .replaceAll("last input", s"$lastinput")
        .replaceAll("antepenultimateinput input", s"$antepenultimateinput")
        .replaceAll("penultimate input", s"$penultimateinput")
        .replaceAll(" input ", s" $replaceinput ")
        .replaceAll("line ([a-z][a-z0-9]*)\\+2", "line $1+1")
        .replaceAll("line ([a-z][a-z0-9]*)\\+3", "line $1+2")
        + suffix)
  }
  
  def alphaNumericalOrder(f: String): String = {
    var res = f
    for(i <- 1 to 3; j = 4-i ) {
      res = res.replaceAll(s"""(?<=[^\\d]|^)(\\d{$i})(?=[^\\d]|$$)""","0"*j+"$1")
    }
    res.replaceAll("[ _\\-\\+#\\$]", "").toLowerCase()
  }
  
  def relativizePath(absolutePath: String, otherPath: String): String = {
    new File(absolutePath).toURI().relativize(new File(otherPath).toURI()).getPath()
  }
}