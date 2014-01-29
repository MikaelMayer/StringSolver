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
  
  var debug = false
  val HISTORY_DIR = "StringSolverRenaming"
  val HISTORY_MV_FILE = "mv.log"
  val HISTORY_AUTO_FILE = "auto.log"
  val HISTORY_PARTITION_FILE = "partition.log"
  final val NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED = 4
  
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
      if(s.toLowerCase() == "partition") {
        Some(())
      } else None
    }
  }
  
  final object Auto {
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == "auto") {
        Some(())
      } else None
    }
    def apply(): String = "auto"
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
  def deleteHistory(filename: String) = {
    getHistoryFile(filename) map { history_file =>
      history_file.delete()
      println("Deleted " + history_file.getAbsolutePath())
    } getOrElse {
      println("Could not delete or inexisting temp file" + filename)
    }
  }
  def deleteMvHistory() = deleteHistory(HISTORY_MV_FILE)
  def deleteAutoHistory() = deleteHistory(HISTORY_AUTO_FILE)
  def deletePartitionHistory() = deleteHistory(HISTORY_PARTITION_FILE)
  
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
  
  /**
   * Auto logging routines
   */
  def storeAutoHistory(log: AutoLog): Unit = storeHistory[AutoLog](log)
  def getAutoHistory(folder: File): Seq[AutoLog] = getHistory[AutoLog](folder)
  def removeDirectoryFromAutoHistory(folder: File): Unit = removeDirectoryFromHistory[AutoLog](folder)
  def setAutoHistoryPerformed(folder: File, input: List[String]): Unit = setHistoryPerformed[AutoLog](folder, input)
  /**
   * Partitioning logging routines
   */
  def storePartitionHistory(log: PartitionLog): Unit = storeHistory[PartitionLog](log)
  def getPartitionHistory(folder: File): Seq[PartitionLog] = getHistory[PartitionLog](folder)
  def removeDirectoryFromPartitionHistory(folder: File): Unit = removeDirectoryFromHistory[PartitionLog](folder)
  
  /**
   * Main function
   */
  def main(args: Array[String]): Unit = {
    val cmd = args.toList
    cmd match {
      case Auto()::q =>    parseAutoCmd(q, Options())
      case Move()::q =>    parseMvCmd(q, Options())
      case Partition()::q => //parsePartitionCmd(q, Options())
      //case Convert()::q => automatedConvert(cmd)
      case _ =>
    }
  }
  
  /**
   * Performs automated renaming suggestion
   * @param perform Indicates if renaming is applied.
   * 
   * Problem: Take the last instances, then the last two, etc.
   */
  def automatedRenaming(opt: Options): Unit = {
    val perform = opt.perform
    val explain = opt.explain
    val examples = getMvHistory(new File(decodedPath))
    val c = StringSolver()
    c.setTimeout(5)
    var alreadyPerformed = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    
    if(examples != Nil) (if(!explain && !perform) println("# Looking for a general command...")) else {
      println("# No action to reproduce in this folder")
      return ();
    }
    
    var nested_level = 0
    examples.reverse.take(2).reverse foreach {
      case MvLog(dir, performed, in, out, time) =>
        if(debug) println(s"Adding $in, $out")
        c.add(List(in), List(out))(0)
        nested_level = in.count(_ == '/') + in.count(_ == '\\')
        if(performed) alreadyPerformed += out
    }
    if(debug) println(s"Exceptions: $alreadyPerformed, nested level = $nested_level")
    
    val files: Array[List[FileName]] = if(nested_level == 0) {
      new File(decodedPath).list().sortBy(alphaNumericalOrder)
      .filter(file => !new File(decodedPath, file).isDirectory())
      //.filterNot(alreadyPerformed)
      .map(List(_))
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory())
        .flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + "/" + file)
        .filter(file => !new File(decodedPath, file).isDirectory())
        .sortBy(alphaNumericalOrder)
        //.filterNot(alreadyPerformed)
        .map(List(_))
      }
    } else Array[List[FileName]]()
    if(debug) println(s"Files: $files")
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
            if(debug) displayProg(prog)
            if(opt.produceBash) {
              // TODO : Produce bash code like this one:
              """find . \( -name '*.jpg' -o -name '*.png' \) -print  | (i=0; while read f; do 
                  let i+=1; mv "$f" "${f%/*}/$(printf %04d "$i").${f##*.}"; 
              done)
              """
            }
            if(explain) {
              if(!debug) displayProg(prog)
            } else {
              val mappedFiles = files map { f => solver(f) }
              val mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") }
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
      removeDirectoryFromMvHistory(new File(decodedPath))
    }
  }
  
  /**
   * Decodes options given on the command line
   */
  object OptionsDecoder {
    def unapply(l: (List[String], Options)): Option[(Options, List[String])] = {
      l match {
        case (command::("-e" | "--explain")::remaining, options) => // Explain the algorithm
          Some((options.copy(explain=true, perform=false), command::remaining))
        case (command::("-t" | "--test")::remaining, options) => // Show what the algorithm would do
          Some((options.copy(explain=false, perform=false), command::remaining))
        case (command::("-a" | "--auto")::remaining, options) => // Generalizes the command
          Some((options.copy(performAll=true), command::remaining))
        case (command::("-l" | "--lines")::remaining, options) => // Takes input from the content of the file/folder
          Some((options.copy(contentFlag=true), command::remaining))
        case (command::("-b" | "--bash")::remaining, options) => // Produces a bash script for the mapping.
          Some((options.copy(produceBash=true), command::remaining))
        case (command::("-d" | "--debug")::remaining, options) => // Produces a bash script for the mapping.
          debug = true
          Some((options.copy(debug=true), command::remaining))
        case _ => None
      }
    }
  }
  
  case class Options(
    perform: Boolean = true,
    explain: Boolean = false, 
    performAll: Boolean = false, 
    contentFlag: Boolean = false,
    produceBash: Boolean = false,
    debug: Boolean = false
  ) {
    
  }
  
  /**Enhanced move command
   * Feature list:
   * - "mv file1 file2" will move file1 to file2, and then 
   * - "mv" will look at the previous history and the last two moves, and perform a generalization to all files
   * - "mv -c" clears previous history
   * - "mv -e" explains
   * - "mv -t" tests the mv command.
   */
  def parseMvCmd(cmd: List[String], options: Options): Unit = {
    (cmd, options) match {
      case (("-c" | "--clear")::remaining, opts) => // Clears the history
        deleteMvHistory()
        if(remaining != Nil) {
          println(remaining.mkString(" ") + " has been ignored")
        }
      case OptionsDecoder(opt2, cmd2) =>
        parseMvCmd(cmd2, opt2)
      case (Nil, opt) => // Automated move.
        automatedRenaming(opt)
      case (sfile1::sfile2::Nil, opt) =>
        val file1 = if(sfile1.indexOf("\\") != -1 || sfile1.indexOf("/") != -1) new File(sfile1) else new File(decodedPath, sfile1)
        val file2 = if(sfile2.indexOf("\\") != -1 || sfile2.indexOf("/") != -1) new File(sfile2) else new File(decodedPath, sfile2)
        if(!file1.exists()) { println(s"file $sfile1 does not exist"); return(); }
        if(opt.perform) move(sfile1, sfile2)
        storeMvHistory(MvLog(new File(decodedPath).getAbsolutePath(), opt.perform, sfile1, sfile2))
        if(debug) println("Explaining " + opt.explain + " performing " + opt.perform)
        automatedRenaming(opt.copy(perform=opt.performAll))
      case (_, opt) => println("The mv command takes exactly two parameters. Options are --clear (-c), --explain (-e), --test (-t), --auto (-a)")
        //automatedRenaming(perform=opt.performAll, explain=opt.explain)
      case _ =>
    }
  }
  
  
  /*def automatedConvert(cmd: List[String]): Unit = {
    cmd match {
      case Convert()::("-c" | "--clear")::_ =>
      case _ =>
    }
  }*/

  /**
   * Sets a line in the history to performed state.
   */
  def setHistoryAutoPerformed(folder: File, files: List[String]): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dir
    try {
      getHistoryFile(HISTORY_AUTO_FILE) foreach { history_file =>
        val content = readFile(history_file)
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- content.filterNot(_ == '\r').split("\n").toList;
            splitted = line.split("\\|\\|").toList if splitted.length >= 7;
            time::dir::performed::contentFlag::nature::filesAndCommand = splitted) yield {
            if(checkDir(dir) && filesAndCommand.startsWith(files) && !performed.toBoolean) {
              (time::dir::true::contentFlag::nature::filesAndCommand).mkString("||")
            } else line
          }
        }
      }
    } catch  {
      case e: IOException =>println("ioexception")
    }
  }
  
  /**
   * Generic automated commands.
   * @param perform Indicates if the requested command is performed
   *   Parameter -t or --test put this variable to false
   *   Default: true
   * @param explain Indicates if the requested command should be explained if applied to all files
   *   Parameter -e or --explain.
   *   Default: false
   * @param peformAll Indicates if the requested command should be performed for all files.
   *   Parameter -a or --auto
   *   Default: false
   * @param contentFlag Indicates if the content of the file rather than the file name should be processed.
   *   Parameter -l or --lines
   *   Default: false
   */
  def parseAutoCmd(cmd: List[String], options: Options): Unit = {
    if(debug) println(s"Action $cmd")
    (cmd, options) match {
      case (("-c" | "--clear")::_, opt) =>
        deleteAutoHistory()
      case OptionsDecoder(opt, cmd) =>
        parseAutoCmd(cmd, opt)
      case (Nil, opt) =>
        automatedAction(opt.copy(performAll=opt.performAll || (opt.perform && !opt.explain)))
      case (command::Nil, opt) => // Automatically detect where the file is and which one it should be applied to.
        val s = new File(decodedPath).list().toList.sortBy(alphaNumericalOrder)
        val sNested = new File(decodedPath).list().toList.flatMap{d => 
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
          storeAutoHistory(AutoLog(dir=new File(decodedPath).getAbsolutePath(), performed=false, content=false, nature=INPUT_FILE_LIST(s.length), input_files=s, commands=command::Nil))
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
        
        val dir = new File(decodedPath).getAbsolutePath()
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
        automatedAction(opt)
       
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
  def automatedAction(opt: Options): Unit = {
    val performLast = opt.perform
    val explain = opt.explain
    val performAll = opt.performAll
    val produceBash = opt.produceBash
    
    // Retrieves examples (performed or not) from the previous launches.
    val examples = getAutoHistory(new File(decodedPath))
    
    val c = StringSolver()
    c.setTimeout(3)
    c.setVerbose(debug)
    
    // Exceptions are files for which the action was already performed
    var alreadyPerformed = Set[String]()
    if(debug) println(s"$decodedPath $examples, $performLast; $explain, $performAll")
    if(examples != Nil) (if(!explain && !performLast && !performAll) println("# Looking for mappings...")) else {
      println("# No action to reproduce in this folder")
      return ();
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
    // Solving the mapping based on the last two examples.
    examples.reverse.take(2).reverse foreach {
      case AutoLog(folder, performed, contentFlag, nature, files, command, time) =>
        // Extra time if looking for dots
        if(command.indexOf("...") != -1) {
          c.setTimeout(4)
          c.setExtraTimeToComputeLoops(2f)
        } 
        // Retrieving the input, either the name of the file or the lines of it if the contentFlag is set.
        // Takes only maximum NUM_INPUT_EXAMPLES_WHEN_UNBOUNDED files or lines.
        val (input,output) = if(contentFlag) {
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
        if(input != previous_input) {
          c.add(input, output)
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
    }
    
    // All files should have the same nature.
    // onlyFile represents if the files are files (true) or directories (false)
    val onlyFiles = examples.lastOption match {
      case Some(AutoLog(dir, performed, contentFlag, (INPUT_FILE | INPUT_FILE_LIST(_) | INPUT_FILE_CONTENT(_)), files, command, time)) => true
      case Some(AutoLog(dir, performed, contentFlag, (INPUT_DIR_CONTENT(_) | INPUT_DIRECTORY), files, command, time)) => false
      case None => true
    }
    if(debug) println(s"Only files: $onlyFiles, nested level = $nested_level")
    
    // files_raw is a array of singletons list of files, either nested or not.
    val files_raw: Array[List[String]] = if(nested_level == 0) {
      new File(decodedPath).list().sortBy(alphaNumericalOrder)
      .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
      //.filterNot(if(is_input_file_list) Set.empty else alreadyPerformed)
      .map(List(_))
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + "/" + file)
        .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
        //.filterNot(if(is_input_file_list) Set.empty else alreadyPerformed)
        .map(List(_))
      }
    } else Array[List[String]]()
    
    //Replacing each file by its name and content if requested
    val files_raw2: Array[List[String]] = if(read_content_file != None) {
      files_raw.map{listFiles => listFiles.head::readLines(readFile(listFiles.head))}
    } else files_raw
    
    //Replacing the list of singleton files by one unique input containing all files
    val input = if(is_input_file_list && read_content_file == None) {
      Array(files_raw2.toList.flatten)
    } else files_raw2

    if(debug) println(s"Input ${input.mkString("")}")
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
              val mapping = 
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
                    setHistoryAutoPerformed(new File(decodedPath), files)
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
      removeDirectoryFromAutoHistory(new File(decodedPath))
    }
  }
  
  
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
                                 new File(decodedPath));
    } else {
      Runtime.getRuntime().exec(Array("/bin/bash", "-c", cmdString_raw), Array[String](), new File(decodedPath))
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
  
  
  def suggestMapping(prog: List[Program], mapping: Array[(List[FileName], Seq[String])], command: String, title: Boolean = true) = {
    val t = if(title) s"#(Mapping found. Type '$command' to perform it, or '$command -e' to explain)  \n" else ""
    val mappingTrimmed = mapping//.toList.sortBy(_.##).take(5)
    val m = (mappingTrimmed map { case (i: List[FileName], j: Seq[String]) => s"# ${i mkString ","} -> ${j.mkString(";")}"} mkString "\n")
    println(t+m)
    if(mappingTrimmed.size < mapping.size) println("...")
    /*val question = "Apply the following transformation to all files?\n" + m
    ask(question, 
    Seq(('y', "Continue renaming",     FINISH, () =>
          for((file, to) <- mapping) {
            move(file, to)
          }),
        ('\n',"",         FINISH,   () => ()), 
        ('n', "Abort",    FINISH,   () => ()), 
        ('e', "explain",  CONTINUE, () =>displayProg(prog)), 
        ('t', "test",     CONTINUE, () => ()))
    )
    */
  }
  
  /**
   * Generic automated partitioning.
   * @param perform Indicates if the requested command is performed
   *   Parameter -t or --test put this variable to false
   *   Default: true
   * @param explain Indicates if the requested command should be explained if applied to all files
   *   Parameter -e or --explain.
   *   Default: false
   * @param peformAll Indicates if the requested command should be performed for all files.
   *   Parameter -a or --auto
   *   Default: false
   * @param contentFlag Indicates if the content of the file rather than the file name should be processed.
   *   Parameter -l or --lines
   *   Default: false
   */
  /*def parsePartitionCmd(cmd: List[String], options: Options): Unit = {
    if(debug) println(s"Action $cmd")
    (cmd, options) match {
      case (("-c" | "--clear")::_, opt) =>
        deletePartitionHistory()
      case OptionsDecoder(opt, cmd) =>
        parsePartitionCmd(cmd, opt)
      case (Nil, opt) =>
        automatedPartition(opt.copy(performAll=opt.performAll || (opt.perform && !opt.explain)))
      case (command::Nil, opt) => // Automatically detect where the file is and which one it should be applied to.
        val s = new File(decodedPath).list().toList.sortBy(alphaNumericalOrder)
        val sNested = new File(decodedPath).list().toList.flatMap{d => 
          val dir = new File(decodedPath, d)
          if(dir.isDirectory)
            dir.list().map(name => dir.getName()+"/"+name)
          else
            Nil
        }
        .sortBy(alphaNumericalOrder)
        
        if(command.indexOf("...") != -1) {
          if(debug) println("Found '...' Going to decode the action for all present files.")
          storePartitionHistory(new File(decodedPath), performed=false, content=false, s, command::Nil)
          parsePartitionCmd(Nil, opt)
        } else {
          if(debug) println("No input provided. Going to try to extract output from file name")
          val candidates = (s ++ sNested).filter{ f => command.indexOf(f) != -1 }.sortBy(_.size).lastOption
          candidates match {
            case Some(file) =>
              parsePartitionCmd(file::command::Nil, opt)
            case None =>
              // If there are three dots in the command, tries to recognize the command
          }
        }
      case (sfile::l, opt) if l != Nil =>
        val file1 = new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        
        storePartitionHistory(new File(decodedPath), false, opt.contentFlag, List(sfile), l)
        if(l.indexWhere(_.indexOf("...") != -1) != -1 && opt.contentFlag) {
          if(debug) println("Found '...' Going to map the action for all lines in command.")
        }
        automatedPartition(opt)
       
      case _ =>
    }
  }
  
  */
  
  /**
   * Asks the user yes and no and give an explanation if necessary.
   * @param s The string to ask
   * @param other_options A list of accepted chars, keywords and functions to trigger if the input matches the char.
   */
  /*def ask(s: String, commands: Seq[(Char, String, REPFL_ANSWER, ()=>Unit)]): Unit = {
    println(s)
    val answer = Array[Byte](2)
    val command_string = commands filterNot (_._1 == '\n') map { case (c, s, a, f) => s"$s($c)"} mkString ", "
    while(true) {
      println(s"$command_string?")
      do {stdin.read(answer, 0, 1)
      commands find { case (c,s,a,f) =>
        if(c == answer(0)) {
          f();
          if(a == FINISH) return;
          true
        } else false
      }
      } while(answer(0) == '\n' || answer(0) == '\r')
    }
  }*/
  
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
  
  def displayProg(prog: Program, lines: Boolean = false, folder: Boolean = false): Unit = {
    val replaceinput = if(lines) "line" else if(folder) "folder name" else "file name"
    val tmp = Printer(prog)
    val first = if("(?<!first) input".r.findFirstIn(tmp) != None) "first " else ""
    val second = if(lines) "first" else "second"
    val third = if(lines) "second" else "third"
    println("# " + tmp.replaceAll("first input", if(lines) "file name" else s"${first}$replaceinput")
        .replaceAll("all inputs", s"all ${replaceinput}s")
        .replaceAll("second input", s"$second $replaceinput")
        .replaceAll("third input", s"$third $replaceinput")
        .replaceAll(" input ", s" $replaceinput ")
        .replaceAll("line ([a-z][a-z0-9]*)\\+2", "line $1+1")
        .replaceAll("line ([a-z][a-z0-9]*)\\+3", "line $1+2"))
  }
  
  def alphaNumericalOrder(f: String): String = {
    var res = f
    for(i <- 1 to 3; j = 4-i ) {
      res = res.replaceAll(s"""(?:[^\\d]|^)(\\d{$i})(?=[^\\d]|$$)""","0"*j+"$1")
    }
    res
  }
}