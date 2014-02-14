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

/**
 * Usage
 * create an executable script named mv containing:
 * 
 * java -jar `dirname $0`/flash-fill_2.10-1.0.jar "$@"
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
      case Auto()::q =>    parseAutoCmd(q)
      case Move()::q =>    parseMvCmd(q)
      case Partition()::q => parsePartitionCmd(q)
      case Filter()::q => parseFilterCmd(q)
      case _ => println(s"Unknown command: " + cmd.mkString(" ") + "\n Try auto, mv, filter or partition")
    }
  }
  
  var debug = false // Can be set by the flag -d
  val HISTORY_DIR = "StringSolverRenaming"
  val HISTORY_MV_FILE = "mv.log"
  val HISTORY_AUTO_FILE = "auto.log"
  val HISTORY_PARTITION_FILE = "partition.log"
  val HISTORY_FILTER_FILE = "filter.log"
  final val NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED = 3
  
    /**
     * Different kind of inputs
     */
  final val INPUT_DIRECTORY = "INPUT_DIRECTORY"
  final val INPUT_FILE = "INPUT_FILE"
  final val INPUT_FILE_EXTENSION = "INPUT_FILE_EXTENSION"
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
  implicit class NatureString(s: Logger) {
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
  final object Partition {
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == apply()) {
        Some(())
      } else None
    }
    def apply(): String = "partition"
  }
  
  final object Auto {
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == apply()) {
        Some(())
      } else None
    }
    def apply(): String = "auto"
  }
  final object Filter {
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == apply()) {
        Some(())
      } else None
    }
    def apply(): String = "filter"
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
    content.split("\n").map(_.trim).toList
  }
  
  //val path = Main.getClass().getProtectionDomain().getCodeSource().getLocation().getPath()
  var workingDirAbsFile = System.getProperty("user.dir");//URLDecoder.decode(path, "UTF-8");
  def workingDirAbsFileFile = new File(workingDirAbsFile)
  
  /**
   * Returns an history file.
   */
  private def getHistoryFile(name: String): Option[File] = {
    val tmpDir = System.getProperty("java.io.tmpdir")
    val history_dir = new File(tmpDir, HISTORY_DIR)
    if(!history_dir.exists()) {
      history_dir.mkdir()
    }
    val history_file = new File(history_dir, name)
    if(if(!history_file.exists) history_file.createNewFile() else true) {
      Some(history_file)
    } else None
  }
  def deleteHistory[A <: Logger : LoggerFile]() = {
    getHistoryFile(implicitly[LoggerFile[A]].history_file) map { history_file =>
      history_file.delete()
      if(debug) println("Deleted " + history_file.getAbsolutePath())
    }
  }
  
  var timeStampGiver = () => new Timestamp(new java.util.Date().getTime).toString
  
  /**
   * Logger facilities
   */
  trait Logger {
    def mkString: String
    def dir: String
    def time: String
    def performed: Boolean
    def nature: String
    def input: List[String]
    def setPerformed(b: Boolean): Logger
  }
  trait LoggerFile[A <: Logger] {
    def history_file: String
    def extractor: String => Option[A]
  }
  trait Companion[T] {
    type C
    def apply() : C
  }
  object Companion {
    implicit def companion[T](implicit comp : Companion[T]) = comp()
  }
  trait LoggerCompanion[C <: Logger] {
    implicit def companion: Companion[C]
  }
  
  /**
   * Renaming log
   */
  object MvLog extends LoggerCompanion[MvLog] {
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
  case class MvLog(dir: String, performed: Boolean, nature: String, file1AndProperties: List[String], file2: String, time: String = timeStampGiver()) extends Logger {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + nature + ";" + file1AndProperties.mkString(";") +";"+file2
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = file1AndProperties
    def file1 = nature match {
      case INPUT_FILE => file1AndProperties.head
      case INPUT_FILE_EXTENSION => file1AndProperties.head + file1AndProperties.tail.head
      case INPUT_FILE_PROPERTIES(n) => file1AndProperties.head
    }
  }
  implicit object MvLogFile extends LoggerFile[MvLog] {
    def history_file = HISTORY_MV_FILE
    def extractor = MvLog.unapply(_)
  }
  
  /**
   * Auto log
   */
  object AutoLog extends LoggerCompanion[AutoLog]{
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
  case class AutoLog(dir: String, performed: Boolean, content: Boolean, nature: String, input_files: List[String], commands: List[String], time: String = timeStampGiver()) extends Logger {
    override def mkString = time + "||" + dir + "||" + performed.toString + "||" + content.toString + "||" + nature + "||"  + (input_files++commands).mkString("||")
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = input_files
  }
  implicit object AutoLogFile extends LoggerFile[AutoLog] {
    def history_file = HISTORY_AUTO_FILE
    def extractor = AutoLog.unapply(_)
  }
  
  /**
   * Partition log
   */
  object PartitionLog extends LoggerCompanion[PartitionLog] {
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
  case class PartitionLog(dir: String, performed: Boolean, nature: String, file1: String, folder: String, time: String = timeStampGiver()) extends Logger {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + nature + ";" + file1+";" + folder
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = List(file1)
  }
  implicit object PartitionLogFile extends LoggerFile[PartitionLog] {
    def history_file = HISTORY_PARTITION_FILE
    def extractor = PartitionLog.unapply(_)
  }
  
  /**
   * Filter log
   */
  object FilterLog extends LoggerCompanion[FilterLog] {
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
  case class FilterLog(dir: String, performed: Boolean, nature: String, file1: String, folder: String, time: String = timeStampGiver()) extends Logger {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + nature + ";" + file1+";"+folder
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = List(file1)
  }
  implicit object FilterLogFile extends LoggerFile[FilterLog] {
    def history_file = HISTORY_FILTER_FILE
    def extractor = FilterLog.unapply(_)
  }
  
  /**
   * Recovers all actions in history which happened in this folder
   */
  def getHistory[A <: Logger : LoggerFile](folder: File): Seq[A] = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dir
    getHistoryFile(implicitly[LoggerFile[A]].history_file) map { history_file =>
      if(debug) println(s"history file : $history_file")
      val content = readFile(history_file)
      if(content.isEmpty) Nil
      else {
      val res = 
        for(line <- readLines(content);
            log <- implicitly[LoggerFile[A]].extractor(line);
            if(checkDir(log.dir))) yield log
      if(debug) println(s"content : $res")
      res
      }
    } getOrElse (List[A]())
  }
  
  /**
   * Remove the directory from a given command.
   */
  def removeDirectoryFromHistory[A <: Logger : LoggerFile](folder: File): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s != dir
    try {
      getHistoryFile(implicitly[LoggerFile[A]].history_file) map { history_file =>
        val content = readFile(history_file)
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- readLines(content);
              log <- implicitly[LoggerFile[A]].extractor(line);
              if(checkDir(log.dir))) yield line
        }
         val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), false)));
         out.println(lines.mkString("\n"))
         out.close
      }
    } catch  {
      case e: IOException =>println("ioexception")
    }
  }
  
  /**
   * Stores a transaction for this folder in the history
   */
  def storeHistory[A <: Logger : LoggerFile](log: A): Unit = {
    getHistoryFile(implicitly[LoggerFile[A]].history_file) foreach { history_file =>
      try {
        if(debug) println(s"Writing in history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
        val line=log.mkString
        if(debug) println(s"Writing line \n$line")
        out.println(line);
        out.close();
      } catch  {
        case e: IOException =>println("ioexception")
      }
    }
  }
  
  /**
   * Sets a line in the history to performed state.
   */
  def setHistoryPerformed[A <: Logger : LoggerFile](folder: File, files: List[String]): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dir
    try {
      getHistoryFile(implicitly[LoggerFile[A]].history_file) foreach { history_file =>
        val content = readFile(history_file)
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- readLines(content);
              log <- implicitly[LoggerFile[A]].extractor(line)) yield {
            if(checkDir(log.dir) && log.input.startsWith(files) && !log.performed) {
              log.setPerformed(true).mkString
            } else line
          }
        }
      }
    } catch  {
      case e: IOException =>println("ioexception")
    }
  }
  
  /**
   * Renaming logging routines
   */
  def storeMvHistory(log: MvLog): Unit = storeHistory[MvLog](log)
  def getMvHistory(folder: File): Seq[MvLog] = getHistory[MvLog](folder)
  def removeDirectoryFromMvHistory(folder: File): Unit = removeDirectoryFromHistory[MvLog](folder)
  def deleteMvHistory() = deleteHistory[MvLog]()
  
  /**
   * Auto logging routines
   */
  def storeAutoHistory(log: AutoLog): Unit = storeHistory[AutoLog](log)
  def getAutoHistory(folder: File): Seq[AutoLog] = getHistory[AutoLog](folder)
  def removeDirectoryFromAutoHistory(folder: File): Unit = removeDirectoryFromHistory[AutoLog](folder)
  def setHistoryAutoPerformed(folder: File, input: List[String]): Unit = setHistoryPerformed[AutoLog](folder, input)
  def deleteAutoHistory() = deleteHistory[AutoLog]()
  
  /**
   * Partitioning logging routines
   */
  def storePartitionHistory(log: PartitionLog): Unit = storeHistory[PartitionLog](log)
  def getPartitionHistory(folder: File): Seq[PartitionLog] = getHistory[PartitionLog](folder)
  def removeDirectoryFromPartitionHistory(folder: File): Unit = removeDirectoryFromHistory[PartitionLog](folder)
  def setHistoryPartitionPerformed(folder: File, input: List[String]): Unit = setHistoryPerformed[PartitionLog](folder, input)
  def deletePartitionHistory() = deleteHistory[PartitionLog]()
  
  /**
   * Filtering logging routines
   */
  def storeFilterHistory(log: FilterLog): Unit = storeHistory[FilterLog](log)
  def getFilterHistory(folder: File): Seq[FilterLog] = getHistory[FilterLog](folder)
  def removeDirectoryFromFilterHistory(folder: File): Unit = removeDirectoryFromHistory[FilterLog](folder)
  def setHistoryFilterPerformed(folder: File, input: List[String]): Unit = setHistoryPerformed[FilterLog](folder, input)
  def deleteFilterHistory() = deleteHistory[FilterLog]()
  
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
      workingDirAbsFileFile.list().sortBy(f => alphaNumericalOrder(maybePrevious(f)))
      .filter(file => new File(workingDirAbsFile, file).isDirectory() ^ onlyFiles)
      .map(file => if(extension) splitExtension(file) else List(file))
    } else if(nested_level == 1){
      workingDirAbsFileFile.listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().sortBy(f => alphaNumericalOrder(maybePrevious(f)))
        .map(file => theDir.getName() + "/" + file)
        .filter(file => new File(workingDirAbsFile, file).isDirectory() ^ onlyFiles)
        .map(file => if(extension) splitExtension(file) else List(file))
      }
    } else Array[List[String]]()
    val res = if(filter) {
      val loghistory = getFilterHistory(workingDirAbsFileFile)
      automatedFilter(Options(perform=false, performAll=false, explain=false), loghistory) match {
        case Some(filters) =>
          val okString = loghistory(0).folder
          val accepted = filters.filter{ case (lin, lout) => lout.head == okString}.map(_._1).toSet
          f filter { elem => accepted(List(elem.mkString(""))) }
        case None => f
      }
    } else {
      f
    }
    res
  }
  
  /**
   * Performs automated renaming suggestion
   * @param opt Various options for dealing with the history
   * @param examples A set of MvLogs used as example to perform the global renaming
   * Problem: Take the last instances, then the last two, etc.
   */
  def automatedRenaming(opt: Options, examples: Seq[MvLog] = getMvHistory(workingDirAbsFileFile), solver: Option[StringSolver] = None): Option[StringSolver] = {
    val perform = opt.perform
    val explain = opt.explain
    val c = solver.getOrElse(StringSolver())
    c.setTimeout(5)
    c.setVerbose(opt.debug)
    val alreadyPerformed = (for(log <- examples if log.performed) yield log.file2).toSet
    if(debug) println(s"$workingDirAbsFile $examples, $perform; $explain")
    
    if(examples != Nil && !(opt.minTwoExamples && examples.length < 2)) {
      if(!explain && !perform)
        println("# Looking for a general command...")
    } else {
      println("# No action to reproduce in this folder")
      return None;
    }
    
    val reverse_mapping = (for(log <- examples if log.performed) yield log.file2 -> log.file1).toMap
    
    val nested_level = examples.last match { case MvLog(dir, performed, nature, in, out, time) => in.count(_ == '/') + in.count(_ == '\\') }
    val onlyFiles = examples.lastOption.map(_.onlyFiles).getOrElse(true)
    val properties = opt.properties | examples.lastOption.map(_.isProperties).getOrElse(false)
    val withExtension = opt.properties | examples.lastOption.map(_.isExtension).getOrElse(false)
    val files_raw: Array[List[String]] =
      listFiles(nested_level,
                onlyFiles=onlyFiles,
                filter=opt.filter,
                extension=withExtension,
                maybePrevious=(s: String) => reverse_mapping.getOrElse(s, s))
    
    val files = if(opt.properties) {
      files_raw.map{listFiles => listFiles.mkString("")::readProperties(listFiles.head)}
    } else files_raw

    if(solver == None)
    examples.reverse.take(2).reverse foreach {
      case MvLog(dir, performed, nature, in, out, time) =>
        
        val index = files.indexWhere(s => s.startsWith(in))
        val position = if(index == -1) Math.max(0, files.indexWhere(s => s.head + s.tail.head == out)) else index
        if(debug) println(s"Adding $in, $out at position $position")
        c.setPosition(position)
        c.add(in, List(out))(0)
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
            if(!explain || opt.test) {
             // Solves the mapping for all files, even for those who were already done.
             val mappedFiles = files map { f => solver(f) }
             // Removes already performed tasks from the mapping
              mapping = files zip mappedFiles filterNot { case (file, m) => m.exists(_ == "") || alreadyPerformed(file.head + file.tail.head) }
              if(mapping.nonEmpty) {
                if(perform) {
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
      removeDirectoryFromMvHistory(workingDirAbsFileFile)
    }
    Option(c)
  }
  
  /**
   * Decodes options given on the command line
   */
  object OptionsDecoder {
    def unapply(l: (List[String], Options)): Option[(Options, List[String])] = {
      l match {
        case (("-e" | "--explain")::remaining, options) => // Explain the algorithm
          Some((options.copy(perform=false, explain=true), remaining))
        case (("-t" | "--test")::remaining, options) => // Show what the algorithm would do
          Some((options.copy(perform=false, test=true), remaining))
        case (("-a" | "--auto" | "--all")::remaining, options) => // Generalizes the command
          Some((options.copy(performAll=true), remaining))
        case (("-f" | "--filter")::remaining, options) => // Generalizes the command
          Some((options.copy(filter=true), remaining))
        case (("-l" | "--lines")::remaining, options) => // Takes input from the content of the file/folder
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
    generalize: Boolean = true
  ) {
    
  }
  
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
  def parseMvCmd(cmd: List[String], options: Options = Options()): Unit = {
    (cmd, options) match {
      case (("-c" | "--clear")::remaining, opt) => // Clears the history
        deleteMvHistory()
        //deleteFilterHistory()
        if(remaining != Nil) {
          parseMvCmd(remaining, opt)
          //println(remaining.mkString(" ") + " has been ignored")
        }
      case OptionsDecoder(opt2, cmd2) =>
        parseMvCmd(cmd2, opt2)
      case (Nil, opt) => // Automated move.
        if(opt.generalize) automatedRenaming(opt.copy(performAll = opt.perform))
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

        storeMvHistory(MvLog(workingDirAbsFileFile.getAbsolutePath(), performed, nature, filename ++ properties, relative_sfile2))
        if(opt.perform && file1.exists()) move(sfile1, sfile2)
        if(debug) println("Explaining " + opt.explain + " performing " + opt.perform)
        if(remaining != Nil) {
          parseMvCmd(remaining, opt)
        } else if(opt.generalize) automatedRenaming(opt.copy(perform=(file1.exists && opt.performAll)))
      case (_, opt) => println("The mv command takes a multiple of two parameters. Options are --clear (-c), --explain (-e), --test (-t), --auto (-a)")
        //automatedRenaming(perform=opt.performAll, explain=opt.explain)
      case _ =>
    }
  }
  
  /**
   * Generic automated commands.
   * @param perform Indicates if the requested command is performed
   *   Parameter -t or --test put this variable to false
   *   Default: true
   * @param options Various options to indicate how the command is performed.
   */
  def parseAutoCmd(cmd: List[String], options: Options = Options()): Unit = {
    if(debug) println(s"Action $cmd")
    (cmd, options) match {
      case (("-c" | "--clear")::remaining, opt) =>
        deleteAutoHistory()
        if(remaining != Nil) {
          parseAutoCmd(remaining, opt)
          //println(remaining.mkString(" ") + " has been ignored")
        }
      case OptionsDecoder(opt, cmd) =>
        parseAutoCmd(cmd, opt)
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
          storeAutoHistory(AutoLog(dir=workingDirAbsFileFile.getAbsolutePath(), performed=false, content=false, nature=INPUT_FILE_LIST(s.length), input_files=s, commands=command::Nil))
          parseAutoCmd(Nil, opt)
        } else {
          if(debug) println("No input provided. Going to try to extract output from file name")
          val candidates = (s ++ sNested).filter{ f => command.indexOf(f) != -1 }.sortBy(_.size).lastOption
          candidates match {
            case Some(file) =>
              parseAutoCmd(file::command::Nil, opt)
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
        
        storeAutoHistory(AutoLog(dir, false, opt.contentFlag, nature, List(sfile), l))
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
  def automatedAction(opt: Options, examples: Seq[AutoLog] = getAutoHistory(workingDirAbsFileFile)): Option[Array[(List[String], Seq[String])]] = {
    val performLast = opt.perform
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
    
    var previous_input: List[String] = Nil
    var previous_output: List[String] = Nil
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
      case AutoLog(folder, performed, contentFlag, nature, files, command, time) =>
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
        if(inputList != previous_input) {
          c.setPosition(input.indexWhere(s => s.startsWith(inputList)))
          c.add(inputList, output)
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
            if(!explain || opt.test) {
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
                    setHistoryAutoPerformed(workingDirAbsFileFile, files)
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
      removeDirectoryFromAutoHistory(workingDirAbsFileFile)
    }
    Option(mapping)
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
    val m = (mappingTrimmed map { case (i: List[FileName], j: Seq[String]) => s"# ${i mkString ","} -> ${j.mkString(";")}"} mkString "\n")
    println(t+m)
    if(mappingTrimmed.size < mapping.size) println("...")
  }
  
  /**
   * Generic automated partitioning.
   * @param perform Indicates if the requested command is performed
   *   Parameter -t or --test put this variable to false
   *   Default: true
   * @param options Various options to indicate how the partitioning is performed.
   */
  def parsePartitionCmd(cmd: List[String], options: Options = Options()): Unit = {
    if(debug) println(s"Action $cmd")
    (cmd, options) match {
      case (("-c" | "--clear")::_, opt) =>
        deletePartitionHistory()
      case OptionsDecoder(opt, cmd) =>
        parsePartitionCmd(cmd, opt)
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
        storePartitionHistory(PartitionLog(dir, opt.perform, nature, sfile, sfolder))
        if(opt.perform) move(sfile,sfolder+"/"+ sfile)
        if(remaining == Nil) {
          if(opt.generalize) automatedPartition(opt)
        } else {
          parsePartitionCmd(remaining, opt)
        }
      case _ =>
    }
  }
  
  
  /**
   * Generic automated filtering.
   * @param perform Indicates if the requested command is performed
   *   Parameter -t or --test put this variable to false
   *   Default: true
   * @param options Various options to indicate how the filtering is performed.
   */
  def parseFilterCmd(cmd: List[String], options: Options = Options(perform=false)): Unit = {
    if(debug) println(s"Action $cmd, options = $options")
    (cmd, options) match {
      case (("-c" | "--clear")::remaining, opt) =>
        deleteFilterHistory()
        if(remaining != Nil) {
          parseFilterCmd(remaining, opt)
        }
      case OptionsDecoder(opt, cmd) =>
        parseFilterCmd(cmd, opt)
      case (Nil, opt) =>
        if(opt.generalize) automatedFilter(opt.copy(performAll=opt.performAll || (!opt.explain && !opt.test)))
      case (sfile::sfolder::remaining, opt) =>
        val file1 = new File(workingDirAbsFile, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        val dir = workingDirAbsFileFile.getAbsolutePath()
        val nature = if(file1.isDirectory()) {
          INPUT_DIRECTORY
        } else {
          INPUT_FILE
        }
        storeFilterHistory(FilterLog(dir, opt.perform, nature, sfile, sfolder))
        if(opt.perform) move(sfile,sfolder+"/"+ sfile)
        
        if(remaining == Nil) {
          if(opt.generalize) automatedFilter(opt)
        } else {
          parseFilterCmd(remaining, opt)
        }
      case _ =>
    }
  }
  

  /**
   * Performs automated partitioning
   * @param opt Various options for dealing with the history
   * @return A mapping given the index
   */
  def automatedPartition(opt: Options, examples: Seq[PartitionLog] = getPartitionHistory(workingDirAbsFileFile)): Option[Array[(List[String], Seq[String])]] = {
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
              displayProg(prog, prefix = "Groups files by ")
              c2.flatMap(_.solve()) match {
                case Some(prog2) =>
                  displayProg(prog2, prefix = "The name of the category is ")
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
                      setHistoryPartitionPerformed(workingDirAbsFileFile, file)
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
      removeDirectoryFromPartitionHistory(workingDirAbsFileFile)
    }
    Option(mapping)
  }
  
  
  /**
   * Performs automated partitioning
   * @param opt Various options for dealing with the history
   * @return A mapping given the index
   */
  def automatedFilter(opt: Options, examples: Seq[FilterLog] = getFilterHistory(workingDirAbsFileFile)): Option[Array[(List[String], Seq[String])]] = {
    val perform = opt.perform
    val explain = opt.explain
    
    val alreadyPerformed = (for(ex <- examples if ex.performed) yield ex.file1).toSet
    if(debug) println(s"$workingDirAbsFile, examples=$examples, perform=$perform; explain=$explain")
    
    if(examples != Nil) (if(!explain && !perform) println("# Looking for a general filtering command...")) else {
      println("# No action to reproduce in this folder")
      return None;
    }
    var lastFilesCommand: List[String] = examples.last match { case FilterLog(dir, performed, nature, in, out, time) => List(in) }
    val (firstCategory: String, nested_level: Int) = examples.head match { case FilterLog(dir, performed, nature, in, out, time) => (out, in.count(_ == '/') + in.count(_ == '\\')) }
    val otherCategory: String = examples.map(_.folder) find { _ != firstCategory} match {
      case Some(str) => str
      case None => println("# Must provide at least one example in each filter partition"); return None
    }
    val examples_for_service = examples map { case FilterLog(dir, performed, nature, in, out, time) => (in, out == firstCategory) }

    val (c, determiningSubstring) = Service.getFilter(examples_for_service, StringSolver().setTimeout(5), opt).getOrElse{
      println("# Unable to filter. Please perform filter --clean to reset filtering option")
      return None
    }
    
    if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
    
    val onlyFiles = examples.lastOption.map(_.onlyFiles).getOrElse(true)
    val files: Array[List[FileName]] = listFiles(nested_level, onlyFiles, filter=false, extension=false) // Do not take a previous filter command, it would not make any sense.
    if(debug) println(s"Files: $files")
    var mapping: Array[(List[String], Seq[String])] = null
    if(files.length != 0) {
      //println("Solving problem...")
      if(debug) println(s"Solving with at most 2")
      
      val attempts = (() => c.solve(),   (e: List[FileName]) => c.solve(e))::Nil
      attempts find { case (computation, solver) => 
        c.resetCounter()
        computation() match {
          case Some(Concatenate(List(ConstStr(a)))) => RETRY
          case Some(prog) =>
            if(debug || explain) {
              displayProg(prog, prefix = s"Moves files to $firstCategory if ", suffix = s" is equal to '$determiningSubstring', and moves them to $otherCategory otherwise.")
            }
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
              """find . \( -name '*.jpg' \) -print"""
            }
            if(!explain || opt.test) {
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
                      setHistoryFilterPerformed(workingDirAbsFileFile, file)
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
      removeDirectoryFromFilterHistory(workingDirAbsFileFile)
    }
    Option(mapping)
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
   * Displays a program with various options
   * @param prog The program to display
   * @param lines If true, considers that the first input is the file name, and all inputs number n+1 are line n
   * @param folder If true, considers that the first input is the folder name, and all inputs number n+1 are file n
   * @param properties If true, considers that the first input is the file name, and the remaining inputs are (owner, year, month, day, hour, minut, second)
   * @param prefix Prefix to display before the name of the program.
   * @param suffix Suffix to add after the name of the program.
   */
  def displayProg(prog: Program, lines: Boolean = false, folder: Boolean = false, properties: Boolean = false, prefix: String = "", suffix: String = ""): Unit = {
    val replaceinput = if(lines) "line" else if(folder) "folder name" else "file name"
    val tmp = Printer(prog)
    val first = if("(?<!first) input".r.findFirstIn(tmp) != None && (lines || folder) && !properties){ "first " }  else ""
    val second = if(lines) "first" else "second"
    val third = if(lines) "second" else "third"
    val fourth = if(lines) "third" else "fourth"
    val fifth = if(lines) "fourth" else "fifth"
    val sixth = if(lines) "fifth" else "sixth"
    val seventh = if(lines) "sixth" else "seventh"
    val eighth = if(lines) "seventh" else "eighth"
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
    
    println("# " +prefix+ tmp.replaceAll("first input", if(lines) "file name" else s"${first}$replaceinput")
        .replaceAll("all inputs", s"all ${replaceinput}s")
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
      res = res.replaceAll(s"""(?:[^\\d]|^)(\\d{$i})(?=[^\\d]|$$)""","0"*j+"$1")
    }
    res.replaceAll("[ _\\-\\+#\\$]", "").toLowerCase()
  }
  
  def relativizePath(absolutePath: String, otherPath: String): String = {
    new File(absolutePath).toURI().relativize(new File(otherPath).toURI()).getPath()
  }
}