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
import Programs.Concatenate
import Programs.ConstStr
import Programs.Program

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
  trait ParameterInput { def prefix: String
    def apply(n: Int) = prefix+n.toString
    def unapply(s: String): Option[Int] = if(s.startsWith(prefix)) Some(s.substring(prefix.length).toInt) else None
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
  type Nature = String
  type FileName = String
  type Performed = Boolean
  type ContentFlag = Boolean
  
  final val RETRY = false
  final val OK = true
  
  
  final object Move {
    def unapply(s: String): Option[Unit] = {
      if(s == "mv" || s.toLowerCase() == "move") {
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
  
  def readFile(path: String): String = {
    readFile(new File(decodedPath, path))
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
  var decodedPath = System.getProperty("user.dir");//URLDecoder.decode(path, "UTF-8");
  def decodedPathFile = new File(decodedPath)
  
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
      println("Deleted " + history_file.getAbsolutePath())
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
    def input: List[String]
    def setPerformed(b: Boolean): Logger
  }
  trait LoggerFile[A <: Logger] {
    def history_file: String
    def extractor: String => Option[A]
  }
  
  /**
   * Renaming log
   */
  object MvLog {
    def unapply(s: String): Option[MvLog] = {
      val a = s.split(";")
      if(a.length == 5) {
        Some(MvLog(a(1), a(2).toBoolean, a(3), a(4), a(0)))
      } else None
    }
  }
  case class MvLog(dir: String, performed: Boolean, file1: String, file2: String, time: String = timeStampGiver()) extends Logger {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + file1+";"+file2
    def setPerformed(b: Boolean) = this.copy(performed = true)
    def input = List(file1)
  }
  implicit object MvLogFile extends LoggerFile[MvLog] {
    def history_file = HISTORY_MV_FILE
    def extractor = MvLog.unapply(_)
  }
  
  /**
   * Auto log
   */
  object AutoLog {
    def unapply(s: String): Option[AutoLog] = {
      val a = s.split("\\|\\|").toList
      if(a.length >= 7) {
        val filesAndCommand = a.drop(5)
        val nature = a(4)
        val (input_files, commands) = nature match {
            case INPUT_FILE | INPUT_DIRECTORY => (List(filesAndCommand.head), filesAndCommand.tail)
            case INPUT_FILE_LIST(n) => (filesAndCommand.take(n), filesAndCommand.drop(n))
            case INPUT_FILE_CONTENT(n) => (filesAndCommand.take(n), filesAndCommand.drop(n))
            case INPUT_DIR_CONTENT(n) => (filesAndCommand.take(n), filesAndCommand.drop(n))
            case _ => throw new Error(s"Unknown nature: $nature")
        }
        Some(AutoLog(a(1), a(2).toBoolean, a(3).toBoolean, a(4), input_files, commands, a(0)))
      } else None
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
  object PartitionLog {
    def unapply(s: String): Option[PartitionLog] = {
      val a = s.split(";")
      if(a.length == 5) {
        Some(PartitionLog(a(1), a(2).toBoolean, a(3), a(4), a(0)))
      } else None
    }
  }
  case class PartitionLog(dir: String, performed: Boolean, file1: String, folder: String, time: String = timeStampGiver()) extends Logger {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + file1+";"+folder
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
  object FilterLog {
    def unapply(s: String): Option[FilterLog] = {
      val a = s.split(";")
      if(a.length == 5) {
        Some(FilterLog(a(1), a(2).toBoolean, a(3), a(4), a(0)))
      } else None
    }
  }
  case class FilterLog(dir: String, performed: Boolean, file1: String, folder: String, time: String = timeStampGiver()) extends Logger {
    override def mkString = time + ";" + dir + ";" + performed.toString + ";" + file1+";"+folder
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
  
  /**
   * Performs automated renaming suggestion
   * @param opt Various options for dealing with the history
   * @param examples A set of MvLogs used as example to perform the global renaming
   * Problem: Take the last instances, then the last two, etc.
   */
  def automatedRenaming(opt: Options, examples: Seq[MvLog] = getMvHistory(decodedPathFile)): Option[Array[(List[String], Seq[String])]] = {
    val perform = opt.perform
    val explain = opt.explain
    val c = StringSolver()
    c.setTimeout(5)
    var alreadyPerformed = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    
    if(examples != Nil) (if(!explain && !perform) println("# Looking for a general command...")) else {
      println("# No action to reproduce in this folder")
      return None;
    }
    
    var nested_level = 0
    
    
    lazy val files: Array[List[FileName]] = if(nested_level == 0) {
      decodedPathFile.list().sortBy(alphaNumericalOrder)
      .filter(file => !new File(decodedPath, file).isDirectory())
      .map(List(_))
    } else if(nested_level == 1){
      decodedPathFile.listFiles().filter(_.isDirectory())
        .flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + "/" + file)
        .filter(file => !new File(decodedPath, file).isDirectory())
        .sortBy(alphaNumericalOrder)
        .map(List(_))
      }
    } else Array[List[FileName]]()
    if(debug) println(s"Files: $files")
    
    examples.reverse.take(2).reverse foreach {
      case MvLog(dir, performed, in, out, time) =>
        if(debug) println(s"Adding $in, $out")
        nested_level = in.count(_ == '/') + in.count(_ == '\\')
        
        c.setPosition(files.indexWhere(s => s.startsWith(List(in))))
        c.add(List(in), List(out))(0)
        
        if(performed) alreadyPerformed += out
    }
    if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
    
    var mapping: Array[(List[String], Seq[String])] = null
    if(files.length != 0) {
      //println("Solving problem...")
      if(debug) println(s"Solving with at most 2")
      
      val attempts = (() => c.solve(),   (e: List[FileName]) => c.solve(e))::
                     (() => c.solveLast(), (e: List[FileName]) => c.solveLast(e))::Nil
      attempts find { case (computation, solver) => 
        c.resetCounter()
        computation() match {
          case Some(Concatenate(List(ConstStr(a)))) => true
          case Some(prog) =>
            if(debug || explain) displayProg(prog)
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
              """find . \( -name '*.jpg' -o -name '*.png' \) -print  | (i=0; while read f; do 
                  let i+=1; mv "$f" "${f%/*}/$(printf %04d "$i").${f##*.}"; 
              done)
              """
            }
            if(!explain) {
              // Removes already performed tasks from the mapping
             val mappedFiles = files map { f => solver(f) }
              mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") || alreadyPerformed(f.head) }
              if(mapping.nonEmpty) {
                if(perform) {
                  for((file, to) <- mapping if !alreadyPerformed(file.head)) {
                    move(file.head, to.head)
                  } 
                }
                suggestMapping(List(prog), mapping, "mv", title= !perform)
              }
            }
            OK
          case None =>
            if(debug) println(s"no program found")
            RETRY
        }
      }
    }
    if(perform) { // We remove the directory from the history.
      removeDirectoryFromMvHistory(decodedPathFile)
    }
    Option(mapping)
  }
  
  /**
   * Decodes options given on the command line
   */
  object OptionsDecoder {
    def unapply(l: (List[String], Options)): Option[(Options, List[String])] = {
      l match {
        case (("-e" | "--explain")::remaining, options) => // Explain the algorithm
          Some((options.copy(explain=true, perform=false), remaining))
        case (("-t" | "--test")::remaining, options) => // Show what the algorithm would do
          Some((options.copy(explain=false, perform=false), remaining))
        case (("-a" | "--auto")::remaining, options) => // Generalizes the command
          Some((options.copy(performAll=true), remaining))
        case (("-l" | "--lines")::remaining, options) => // Takes input from the content of the file/folder
          Some((options.copy(contentFlag=true), remaining))
        case (("-b" | "--bash")::remaining, options) => // Produces a bash script for the mapping.
          Some((options.copy(produceBash=true), remaining))
        case (("-s" | "--simple")::remaining, options) => // do not perform suggestions
          Some((options.copy(generalize=false), remaining))
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
    performAll: Boolean = false, 
    contentFlag: Boolean = false,
    produceBash: Boolean = false,
    debug: Boolean = false,
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
      case (("-c" | "--clear")::remaining, opts) => // Clears the history
        deleteMvHistory()
        if(remaining != Nil) {
          println(remaining.mkString(" ") + " has been ignored")
        }
      case OptionsDecoder(opt2, cmd2) =>
        parseMvCmd(cmd2, opt2)
      case (Nil, opt) => // Automated move.
        if(opt.generalize) automatedRenaming(opt)
      case (sfile1::sfile2::Nil, opt) =>
        val file1 = if(sfile1.indexOf("\\") != -1 || sfile1.indexOf("/") != -1) new File(sfile1) else new File(decodedPath, sfile1)
        val file2 = if(sfile2.indexOf("\\") != -1 || sfile2.indexOf("/") != -1) new File(sfile2) else new File(decodedPath, sfile2)
        if(!file1.exists()) { println(s"file $sfile1 does not exist"); return(); }
        if(opt.perform) move(sfile1, sfile2)
        storeMvHistory(MvLog(decodedPathFile.getAbsolutePath(), opt.perform, sfile1, sfile2))
        if(debug) println("Explaining " + opt.explain + " performing " + opt.perform)
        if(opt.generalize) automatedRenaming(opt.copy(perform=opt.performAll))
      case (_, opt) => println("The mv command takes exactly two parameters. Options are --clear (-c), --explain (-e), --test (-t), --auto (-a)")
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
      case (("-c" | "--clear")::_, opt) =>
        deleteAutoHistory()
      case OptionsDecoder(opt, cmd) =>
        parseAutoCmd(cmd, opt)
      case (Nil, opt) =>
        if(opt.generalize) automatedAction(opt.copy(performAll=opt.performAll || (opt.perform && !opt.explain)))
      case (command::Nil, opt) => // Automatically detect where the file is and which one it should be applied to.
        val s = decodedPathFile.list().toList.sortBy(alphaNumericalOrder)
        val sNested = decodedPathFile.list().toList.flatMap{d => 
          val dir = new File(decodedPath, d)
          if(dir.isDirectory)
            dir.list().map(name => dir.getName()+"/"+name)
          else
            Nil
        }
        .sortBy(alphaNumericalOrder)
        
        /*val nature = if(contentFlag) {
          INPUT_FILE_CONTENT(input_files.length)
        } else input_files match {
          case List(file) => if(new File(dir, file).isDirectory()) INPUT_DIRECTORY else {
            INPUT_FILE
          }
          case Nil => throw new Error("No input file provided")
          case l => INPUT_FILE_LIST(input_files.length)
        }*/
        
        if(command.indexOf("...") != -1) {
          if(debug) println("Found '...' Going to decode the action for all present files.")
          storeAutoHistory(AutoLog(dir=decodedPathFile.getAbsolutePath(), performed=false, content=false, nature=INPUT_FILE_LIST(s.length), input_files=s, commands=command::Nil))
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
        val file1 = new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        
        val dir = decodedPathFile.getAbsolutePath()
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
  def automatedAction(opt: Options, examples: Seq[AutoLog] = getAutoHistory(decodedPathFile)): Option[Array[(List[String], Seq[String])]] = {
    val performLast = opt.perform
    val explain = opt.explain
    val performAll = opt.performAll
    val produceBash = opt.produceBash

    val c = StringSolver()
    c.setTimeout(3)
    c.setVerbose(debug)
    
    // Exceptions are files for which the action was already performed
    var alreadyPerformed = Set[String]()
    if(debug) println(s"$decodedPath $examples, $performLast; $explain, $performAll")
    if(examples != Nil) (if(!explain && !performLast && !performAll) println("# Looking for mappings...")) else {
      println("# No action to reproduce in this folder")
      return None
    }
    
    // Controls the file nesting level
    var nested_level = 0
    
    // Controls if the input for the (only) command is the list of files.
    var is_input_file_list = false 
    
    // Controls if the last example was reading the content of the file instead of the name itself.
    var read_content_file: Option[FileName] = None
    
    var previous_input: List[String] = Nil
    var previous_output: List[String] = Nil
    var lastFilesCommand: List[String] = Nil
    // All files should have the same nature.
    // onlyFile represents if the files are files (true) or directories (false)
    val onlyFiles = examples.lastOption match {
      case Some(AutoLog(dir, performed, contentFlag, (INPUT_FILE | INPUT_FILE_LIST(_) | INPUT_FILE_CONTENT(_)), files, command, time)) => true
      case Some(AutoLog(dir, performed, contentFlag, (INPUT_DIR_CONTENT(_) | INPUT_DIRECTORY), files, command, time)) => false
      case None => true
    }
    
    // files_raw is a array of singletons list of files, either nested or not.
    lazy val files_raw: Array[List[String]] = if(nested_level == 0) {
      decodedPathFile.list().sortBy(alphaNumericalOrder)
      .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
      .map(List(_))
    } else if(nested_level == 1){
      decodedPathFile.listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + "/" + file)
        .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
        .map(List(_))
      }
    } else Array[List[String]]()
    
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
        // Recovers the nesting level.
        nested_level = files.head.count(_ == '/') + files.head.count(_ == '\\')
        // If the last term in the command is a file, it is in exceptions
        if(performed) {
          alreadyPerformed += command.last // Assuming that the last element of the command might be a file (?)
          alreadyPerformed ++= files
        } else lastFilesCommand = files
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
        c.resetCounter() // TODO : Count the exceptions which already occurred.
        computation() match {
          case List(Some(Concatenate(List(ConstStr(a))))) => RETRY
          case l if l.forall(_.nonEmpty) =>
            if(explain || debug) {
              for(ls <- l; prog <- ls) {
               displayProg(prog, lines = (read_content_file != None), folder = !onlyFiles)
              }
            }
            if(!explain) {
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
                    setHistoryAutoPerformed(decodedPathFile, files)
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
      removeDirectoryFromAutoHistory(decodedPathFile)
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
                                 decodedPathFile);
    } else {
      Runtime.getRuntime().exec(Array("/bin/bash", "-c", cmdString_raw), Array[String](), decodedPathFile)
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
        val file1 = new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        val dir = decodedPathFile.getAbsolutePath()
        if(opt.perform) move(sfile,new File(new File(decodedPath, sfolder).getAbsolutePath(), sfile).getAbsolutePath())
        storePartitionHistory(PartitionLog(dir, opt.perform, sfile, sfolder))
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
  def parseFilterCmd(cmd: List[String], options: Options = Options()): Unit = {
    if(debug) println(s"Action $cmd")
    (cmd, options) match {
      case (("-c" | "--clear")::_, opt) =>
        deleteFilterHistory()
      case OptionsDecoder(opt, cmd) =>
        parseFilterCmd(cmd, opt)
      case (Nil, opt) =>
        if(opt.generalize) automatedFilter(opt.copy(performAll=opt.performAll || (opt.perform && !opt.explain)))
      case (sfile::sfolder::remaining, opt) =>
        val file1 = new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        val dir = decodedPathFile.getAbsolutePath()
        if(opt.perform) move(sfile,new File(new File(decodedPath, sfolder).getAbsolutePath(), sfile).getAbsolutePath())
        storeFilterHistory(FilterLog(dir, opt.perform, sfile, sfolder))
        if(remaining == Nil) {
          if(opt.generalize) automatedFilter(opt)
        } else {
          parseFilterCmd(remaining, opt)
        }
      case _ =>
    }
  }
  
  
  
  /**
   * Returns a set of all common substrings
   */
  def intersect(s1: String, s2: String): Set[String] = {
    (for(i <- 0 until s1.length;
        j <- (i+1) to s1.length;
        c = s1.substring(i, j)
        if s2.indexOf(c) != -1) yield c).toSet
  }
  
  /**
   * Returns the set of all substrings
   */
  def substrings(s1: String): Set[String] = {
    (for(i <- 0 until s1.length;
          j <- (i+1) to s1.length;
          c = s1.substring(i, j)) yield c).toSet
  }   
  
  /**
   * Performs automated partitioning
   * @param opt Various options for dealing with the history
   * @return A mapping given the index
   */
  def automatedPartition(opt: Options, examples: Seq[PartitionLog] = getPartitionHistory(decodedPathFile)): Option[Array[(List[String], Seq[String])]] = {
    val perform = opt.perform
    val explain = opt.explain
    val c = StringSolver()
    c.setTimeout(5)
    var alreadyPerformed = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    
    if(examples != Nil) (if(!explain && !perform) println("# Looking for a general partition command...")) else {
      println("# No action to reproduce in this folder")
      return None;
    }
    var lastFilesCommand: List[String] = examples.last match {
      case PartitionLog(dir, performed, in, out, time) => List(in)
    }
    var nested_level = 0
    val partitions = examples groupBy { case PartitionLog(dir, performed, in, out, time) => out }
    val partitions_w_substrings = partitions map { case (category, partition) => 
      val files = partition map { case PartitionLog(dir, performed, in, out, time) => in}
      val common_substrings = files match {
        case List(a) => substrings(a)
        case List(a,b) => intersect(a, b)
        case a::b::q => ((intersect(a, b) /: q){ case (s, a) => s intersect substrings(a) })
      }
      if(common_substrings.isEmpty) {
        println("# Impossible to determine the common substrings of files " + files.mkString(","))
        deletePartitionHistory()
        return None
      }
      (category, (common_substrings, partition))
    }
    val substring_to_category = ListBuffer[(String, String)]()
    val partitions_w_determining_substrings = partitions_w_substrings map {
      case (category, (substrings, partition)) =>
        val other_substrings = (partitions_w_substrings.toList.filter{ case (c, (l2, p)) => c != category }.flatMap{ case (c, (l2, p)) => l2 } )
        val smallest_set = substrings -- other_substrings
        if(smallest_set.isEmpty) {
          println(s"# Files in partition ${category} are too similar to other categories to be put in a different partition")
          deletePartitionHistory()
          return None
        }
        val representative = smallest_set.toList.sortBy(_.length).last
        substring_to_category += representative -> category
        (category, (representative, partition))
    }
    
    // Adding mappings
    partitions_w_determining_substrings.toList foreach {
      case (category, (determining_substring, partition)) =>
        import ProgramsSet._
        import Programs._
        partition foreach {
           case PartitionLog(dir, performed, in, out, time) =>
             c.add(List(in),determining_substring)
             nested_level = in.count(_ == '/') + in.count(_ == '\\')
             if(performed) {
               alreadyPerformed += out
               alreadyPerformed += in
             }
        }
    }
    
    /**
     * Solver to find a mapping between the representative and the category
     * Finds counters, but also substrings from representative
     */
    val c2 = StringSolver()
    substring_to_category.sortBy({ case (a, b) => alphaNumericalOrder(a) }).distinct foreach {
      case (representative, category) =>
        if(debug) {
          println(s"Add c2 $representative -> $category")
        }
        c2.add(List(representative), category)
    }
    val substring_to_category_map = MMap() ++ substring_to_category // TODO : Only one per category.
    def getCategory(representative: String, orElse: String) = substring_to_category_map.getOrElseUpdate(representative, {
      val tmp = c2.solve(representative)
      val res = if(tmp == "") orElse else tmp
      if(debug) println(s"Category of $representative is $res")
      res
    })
    
    if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
    
    val files: Array[List[FileName]] = if(nested_level == 0) {
      decodedPathFile.list().sortBy(alphaNumericalOrder)
      .filter(file => !new File(decodedPath, file).isDirectory())
      .map(List(_))
    } else if(nested_level == 1){
      decodedPathFile.listFiles().filter(_.isDirectory())
        .flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + "/" + file)
        .filter(file => !new File(decodedPath, file).isDirectory())
        .sortBy(alphaNumericalOrder)
        .map(List(_))
      }
    } else Array[List[FileName]]()
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
              c2.solve() match {
                case Some(prog2) =>
                  displayProg(prog2, prefix = "The name of the category is ")
                case _ =>
                  println("# The name of the category is the result of the grouping program above")
              }
            }
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
            }
            if(!explain) {
              val mappedFiles = files map { f => { val tmp = solver(f); if(tmp.length == 1) Seq(getCategory(tmp.head, tmp.head)) else Seq("") } }
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
                      setHistoryPartitionPerformed(decodedPathFile, file)
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
      removeDirectoryFromPartitionHistory(decodedPathFile)
    }
    Option(mapping)
  }
  
  
  /**
   * Performs automated partitioning
   * @param opt Various options for dealing with the history
   * @return A mapping given the index
   */
  def automatedFilter(opt: Options, examples: Seq[FilterLog] = getFilterHistory(decodedPathFile)): Option[Array[(List[String], Seq[String])]] = {
    val perform = opt.perform
    val explain = opt.explain
    val c = StringSolver()
    c.setTimeout(5)
    var alreadyPerformed = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    
    if(examples != Nil) (if(!explain && !perform) println("# Looking for a general filtering command...")) else {
      println("# No action to reproduce in this folder")
      return None;
    }
    var lastFilesCommand: List[String] = examples.last match {
      case FilterLog(dir, performed, in, out, time) => List(in)
    }
    var nested_level = 0
    var firstCategory: String = null 
    val filterings = examples.groupBy({ case FilterLog(dir, performed, in, out, time) =>
      if(firstCategory == null) firstCategory = out; out }).toList.sortBy({ case (key, value) => if(key == firstCategory) 1 else 2 })
    if(filterings.length < 2 || filterings.length > 2) {
      println("# Filtering requires exactly two output folders, where the first one is accepting. Got "+filterings.length)
      if(debug) println(filterings)
      return None
    }
    val filterings_w_substrings = filterings map { case (category, filtering) => 
      val files = filtering map { case FilterLog(dir, performed, in, out, time) => in}
      val common_substrings = files match {
        case List(a) => Set.empty[String]
        case List(a,b) => intersect(a, b)
        case a::b::q => ((intersect(a, b) /: q){ case (s, a) => s intersect substrings(a) })
      }
      (category, (common_substrings, filtering))
    }
    //val substring_to_category = ListBuffer[(String, String)]()
    val filterings_w_determining_substrings = filterings_w_substrings map {
      case (category, (substrings, filtering)) =>
        if(category == firstCategory) {
          val other_substrings = (filterings_w_substrings.toList.filter{ case (c, (l2, p)) => c != category }.flatMap{ case (c, (l2, p)) => l2 } )
          val smallest_set = substrings -- other_substrings
          if(smallest_set.isEmpty) {
            println(s"# Files in category ${category} do not share a common string not found in others.")
            deleteFilterHistory()
            return None
          }
          (category, (smallest_set.toList.sortBy(e => e.length).last, filtering))
        } else {
          (category, ("", filtering))
        }
        
        //val representative = smallest_set.toList.sortBy(_.length).last
        //substring_to_category += representative -> category
    }
    
    /** To find out which of the substring in the smallest set is determining the filter,
     *  we need to intersect all corresponding programs to generate each one of them. */
    
    // Adding mappings
    var determiningSubstring = ""
    var otherCategory = ""
    filterings_w_determining_substrings.toList foreach {
      case (category, (determining_substring, filtering)) =>
        import ProgramsSet._
        import Programs._
        if(category == firstCategory) {
          determiningSubstring = determining_substring
          filtering foreach {
             case FilterLog(dir, performed, in, out, time) =>
               c.add(List(in),determining_substring)
               nested_level = in.count(_ == '/') + in.count(_ == '\\')
               if(performed) {
                 alreadyPerformed += out
                 alreadyPerformed += in
               }
          }
        } else {
          otherCategory = category
          filtering foreach {
             case FilterLog(dir, performed, in, out, time) =>
               if(performed) {
                 alreadyPerformed += out
                 alreadyPerformed += in
               }
          }
        }
    }
    
    if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
    
    val files: Array[List[FileName]] = if(nested_level == 0) {
      decodedPathFile.list().sortBy(alphaNumericalOrder)
      .filter(file => !new File(decodedPath, file).isDirectory())
      .map(List(_))
    } else if(nested_level == 1){
      decodedPathFile.listFiles().filter(_.isDirectory())
        .flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + "/" + file)
        .filter(file => !new File(decodedPath, file).isDirectory())
        .sortBy(alphaNumericalOrder)
        .map(List(_))
      }
    } else Array[List[FileName]]()
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
              displayProg(prog, prefix = s"Moves files to $firstCategory if ", suffix = s" is equal to $determiningSubstring, and move them to $otherCategory otherwise.")
            }
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
            }
            if(!explain) {
              val mappedFiles = files map { f => { val tmp = solver(f); if(tmp.length == 1 && tmp.head == determiningSubstring) Seq(firstCategory) else Seq(otherCategory) } }
              mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") }
              if(mapping.nonEmpty) {
                /*if(perform) {
                  for((file, to) <- mapping if !alreadyPerformed(file.head)) {
                    move(file.head, to.head)
                  }
                }*/
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
                      setHistoryFilterPerformed(decodedPathFile, file)
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
      removeDirectoryFromFilterHistory(decodedPathFile)
    }
    Option(mapping)
  }
  
  /**
   * Moves a file to another location.
   * Creates the repository if necessary.
   */
  def move(file: String, to: String) = {
    val directoryName = if(to.indexOf("/") != -1 || to.indexOf("\\") != -1) {
      to.getDirectory
    } else decodedPath
    val theDir = new File(directoryName);

    if (!theDir.exists()) {
      val result = theDir.mkdir();  
    }
    new File(decodedPath, file).renameTo(new File(decodedPath, to))
    
    if(file.indexOf("/") != -1 || file.indexOf("\\") != -1) {
      val dir = new File(decodedPath, file.getDirectory)
      if(dir.list().length == 0) {
        dir.delete()
      }
    }
    //s"move $file $to".!!
    //println(s"move $file $to")
  }
  
  def displayProg(prog: Program, lines: Boolean = false, folder: Boolean = false, prefix: String = "", suffix: String = ""): Unit = {
    val replaceinput = if(lines) "line" else if(folder) "folder name" else "file name"
    val tmp = Printer(prog)
    val first = if("(?<!first) input".r.findFirstIn(tmp) != None) "first " else ""
    val second = if(lines) "first" else "second"
    val third = if(lines) "second" else "third"
    println("# " +prefix+ tmp.replaceAll("first input", if(lines) "file name" else s"${first}$replaceinput")
        .replaceAll("all inputs", s"all ${replaceinput}s")
        .replaceAll("second input", s"$second $replaceinput")
        .replaceAll("third input", s"$third $replaceinput")
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
    res
  }
}