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
  
  final val debug = false
  val HISTORY_DIR = "StringSolverRenaming"
  val HISTORY_MV_FILE = "mv.log"
  val HISTORY_AUTO_FILE = "auto.log"
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
  final object Convert {
    def unapply(s: String): Option[Unit] = {
      if(s.toLowerCase() == "convert") {
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
    val encoded = Files.readAllBytes(Paths.get(file.getAbsolutePath()));
    val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
    content
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
  def deleteMvHistory() = {
    getHistoryFile(HISTORY_MV_FILE) map { history_file =>
      history_file.delete()
      println("Deleted " + history_file.getAbsolutePath())
    } getOrElse {
      println("Could not delete temp file")
    }
  }
  def deleteAutoHistory() = {
    getHistoryFile(HISTORY_AUTO_FILE) map { history_file =>
      history_file.delete()
      println("Deleted " + history_file.getAbsolutePath())
    } getOrElse {
      println("Could not delete temp file")
    }
  }
  
  /**
   * Stores a renaming in the rename history
   */
  def storeMvHistory(dir: File, performed: Performed, file: String, to: String): Unit = {
    getHistoryFile(HISTORY_MV_FILE) map { history_file =>
      try {
        if(debug) println(s"Writing in mv history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
        val date= new java.util.Date();
        val time = new Timestamp(date.getTime()).toString();
   
        //println("Timestamp")
        val line=time + ";" + dir.getAbsolutePath() + ";" + performed.toString + ";" + file+";"+to
        if(debug) println(s"Writing line \n$line")
        out.println(line);
        out.close();
      } catch  {
        case e: IOException =>println("ioexception")
      }
    }
  }
  
  /**
   * Recovesr all renaming in history which happened in this folder
   */
  def getMvHistory(folder: File): Seq[(Performed, FileName, FileName)] = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dir
    getHistoryFile(HISTORY_MV_FILE) map { history_file =>
      if(debug) println(s"history file : $history_file")
      val content = readFile(history_file)
      if(content.isEmpty) Nil
      else {
      val res = 
        for(line <- readLines(content);
            splitted = line.split(";") if splitted.length == 5;
            Array(time, dir, performed, file1, file2) = splitted;
            if(checkDir(dir))) yield (performed.toBoolean, file1, file2)
      res
      }
    } getOrElse (List[(Performed, FileName, FileName)]())
  }
  
  /**
   * Remove the directory from a given move command.
   */
  def removeDirectoryFromMvHistory(folder: File): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s != dir
    try {
      getHistoryFile(HISTORY_MV_FILE) map { history_file =>
        val content = readFile(history_file)
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- content.filterNot(_ == '\r').split("\n").toList;
              splitted = line.split(";") if splitted.length == 5;
              Array(time, dir, performed, file1, file2) = splitted;
              if(checkDir(dir))) yield line
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
   * Main function
   */
  def main(args: Array[String]): Unit = {
    val cmd = args.toList
    cmd match {
      case Auto()::q =>    parseAutoCmd(cmd, perform=true, explain=false, performAll=false, contentFlag=false)
      case Move()::q =>    parseMvCmd(cmd)
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
  def automatedRenaming(perform: Boolean = true, explain: Boolean = false): Unit = {
    val examples = getMvHistory(new File(decodedPath))
    val c = StringSolver()
    c.setTimeout(5)
    var exceptions = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    
    if(examples != Nil) (if(!explain && !perform) println("Looking for mappings...")) else {
      println("No action to reproduce in this folder")
      return ();
    }
    
    var nested_level = 0
    examples.reverse.take(2).reverse foreach {
      case (performed, in, out) =>
        if(debug) println(s"Adding $in, $out")
        c.add(List(in), List(out))(0)
        nested_level = in.count(_ == '/') + in.count(_ == '\\')
        if(performed) exceptions += out
    }
    if(debug) println(s"Exceptions: $exceptions, nested level = $nested_level")
    
    val files: Array[List[FileName]] = if(nested_level == 0) {
      new File(decodedPath).list().sortBy(alphaNumericalOrder)
      .filter(file => !new File(decodedPath, file).isDirectory())
      .filterNot(exceptions)
      .map(List(_))
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory())
        .flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + File.separator + file)
        .filter(file => !new File(decodedPath, file).isDirectory())
        .sortBy(alphaNumericalOrder)
        .filterNot(exceptions)
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
        computation() match {
          case Some(Concatenate(List(ConstStr(a)))) => true
          case Some(prog) =>
            if(debug) displayProg(prog)
            if(explain) {
             if(!debug) displayProg(prog)
            } else {
              val mappedFiles = files map { f => solver(f) }
              val mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") }
              if(mapping.nonEmpty) {
                if(perform) {
                  for((file, to) <- mapping) {
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
  
  
  /**Enhanced move command
   * Feature list:
   * - "mv file1 file2" will move file1 to file2, and then 
   * - "mv" will look at the previous history and the last two moves, and perform a generalization to all files
   * - "mv -c" clears previous history
   * - "mv -e" explains
   * - "mv -t" tests the mv command.
   */
  def parseMvCmd(cmd: List[String], perform: Boolean=true, explain: Boolean=false, doall: Boolean = false): Unit = {
    cmd match {
      case Move()::("-c" | "--clear")::_ => // Clears the history
        deleteMvHistory()
      case Move()::("-e" | "--explain")::remaining => // Explain the algorithm
        parseMvCmd(Move()::remaining, perform=false, explain=true, doall=doall)
      case Move()::("-t" | "--test")::remaining => // Show what the algorithm would do
        parseMvCmd(Move()::remaining, perform=false, explain=explain, doall=doall)
      case Move()::("-a" | "--auto")::remaining => // Generalizes the command
        parseMvCmd(Move()::remaining, perform=perform, explain=explain, doall=true)
      case Move()::Nil => // Automated move.
        automatedRenaming(perform, explain)
      case Move()::sfile1::sfile2::Nil =>
        val file1 = if(sfile1.indexOf("\\") != -1 || sfile1.indexOf("/") != -1) new File(sfile1) else new File(decodedPath, sfile1)
        val file2 = if(sfile2.indexOf("\\") != -1 || sfile2.indexOf("/") != -1) new File(sfile2) else new File(decodedPath, sfile2)
        if(!file1.exists()) { println(s"file $sfile1 does not exist"); return(); }
        if(perform) move(sfile1, sfile2)
        storeMvHistory(new File(decodedPath), perform, sfile1, sfile2)
        if(debug) println("Explaining " + explain + " performing " + perform)
        automatedRenaming(perform=doall, explain=explain)
      case Move()::_ => println("The mv command takes exactly two parameters. Options are --clear (-c), --explain (-e), --test (-t), --auto (-a)")
      automatedRenaming(perform=doall, explain=explain)
        
      case other => println("Unkown function : " + other.mkString(" "))
    }
  }
  
  
  /*def automatedConvert(cmd: List[String]): Unit = {
    cmd match {
      case Convert()::("-c" | "--clear")::_ =>
      case _ =>
    }
  }*/

  
  
  /**
   * Stores a renaming in the rename history
   */
  def storeAutoHistory(dir: File, performed: Performed, content: Boolean, input_files: List[FileName], cmd: List[String]): Unit = {
    getHistoryFile(HISTORY_AUTO_FILE) foreach { history_file =>
      try {
        if(debug) println(s"Writing in history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
        val date= new java.util.Date();
        val time = new Timestamp(date.getTime()).toString();
        val nature = if(content) {
          INPUT_FILE_CONTENT(input_files.length)
        } else input_files match {
          case List(file) => if(new File(dir, file).isDirectory()) INPUT_DIRECTORY else {
            INPUT_FILE
          }
          case Nil => throw new Error("No input file provided")
          case l => INPUT_FILE_LIST(input_files.length)
        }
        val line = time + "||" + dir.getAbsolutePath() + "||" + performed + "||" + content + "||" + nature + "||" + (input_files++cmd).mkString("||")
        if(debug) println(s"Writing line \n$line")
        out.println(line);
        out.close();
      } catch  {
        case e: IOException =>println("ioexception")
      }
    }
  }
  
  /**
   * Recovers all renaming in history which happened in this folder
   * Name of file, nature (DIRECTORY or FILE), and list of associated commands.
   */
  def getAutoHistory(folder: File): Seq[(Nature, Performed, ContentFlag, List[FileName], List[String])] = {
    val dirFolder = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dirFolder
    getHistoryFile(HISTORY_AUTO_FILE) map { history_file =>
      if(debug) println(s"history file : $history_file")
      val content = readFile(history_file)
      if(content.isEmpty) Nil
      else {
      val res = 
        for(line <- content.filterNot(_ == '\r').split("\n").toList;
            splitted = line.split("\\|\\|").toList if splitted.length >= 7;
            time::dir::performed::contentFlag::nature::filesAndCommand = splitted;
            if(checkDir(dir))) yield {
          nature match {
            case INPUT_FILE | INPUT_DIRECTORY => (nature, performed.toBoolean, contentFlag.toBoolean, List(filesAndCommand.head), filesAndCommand.tail)
            case INPUT_FILE_LIST(n) => (nature, performed.toBoolean, contentFlag.toBoolean, filesAndCommand.take(n), filesAndCommand.drop(n))
            case INPUT_FILE_CONTENT(n) => (nature, performed.toBoolean, contentFlag.toBoolean, List(filesAndCommand.head), filesAndCommand.tail)
            case _ => throw new Error(s"Unknown nature: $nature")
          }
        }
      res
      }
    } getOrElse (List[(Nature, Performed, ContentFlag, List[FileName], List[String])]())
  }
  
  /**
   * Remove the directory from a given move command.
   */
  def removeDirectoryFromAutoHistory(folder: File): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s != dir
    try {
      getHistoryFile(HISTORY_AUTO_FILE) map { history_file =>
        val content = readFile(history_file)
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- content.filterNot(_ == '\r').split("\n").toList;
            splitted = line.split("\\|\\|").toList if splitted.length >= 7;
            time::dir::performed::contentFlag::nature::filesAndCommand = splitted;
            if(checkDir(dir))) yield line
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
  def parseAutoCmd(cmd: List[String], perform: Boolean, explain: Boolean, performAll: Boolean, contentFlag: Boolean): Unit = {
    if(debug) println(s"Action $cmd")
    cmd match {
      case Auto()::("-c" | "--clear")::_ =>
        deleteAutoHistory()
      case Auto()::Nil =>
        automatedAction(performLast=perform, explain=explain, performAll=performAll)
      case Auto()::("-e" | "--explain")::remaining =>
        parseAutoCmd(Auto()::remaining, perform=false, explain = true, performAll=performAll, contentFlag = contentFlag)
      case Auto()::("-t" | "--test")::remaining =>
        parseAutoCmd(Auto()::remaining, perform=false, explain=explain, performAll=performAll, contentFlag = contentFlag)
      case Auto()::("-a" | "--auto")::remaining => 
        parseAutoCmd(Auto()::remaining, perform=perform, explain=explain, performAll=true, contentFlag = contentFlag)
      case Auto()::("-l" | "--lines")::remaining => 
        parseAutoCmd(Auto()::remaining, perform=perform, explain=explain, performAll=performAll, contentFlag = true)
        //parseAutoCmd(Auto()::remaining, perform=perform, explain=explain, doall=true)
      case Auto()::command:: Nil => // Automatically detect where the file is and which one it should be applied to.
        val s = new File(decodedPath).list().toList.sortBy(alphaNumericalOrder)
        if(command.indexOf("...") != -1) {
          if(debug) println("Found '...' Going to decode the action for all present files.")
          storeAutoHistory(new File(decodedPath), performed=false, content=false, s, command::Nil)
          parseAutoCmd(Auto()::Nil, perform, explain, performAll=performAll, contentFlag=contentFlag)
        } else {
          if(debug) println("No input provided. Going to try to extract output from file name")
          val candidates = s.filter{ f => command.indexOf(f) != -1 }.sortBy(_.size).lastOption
          candidates match {
            case Some(file) =>
              parseAutoCmd(Auto()::file::command::Nil, perform, explain, performAll, contentFlag)
            case None =>
              // If there are three dots in the command, tries to recognize the command
          }
        }
      case Auto()::sfile::l if l != Nil =>
        val file1 = new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        
        storeAutoHistory(new File(decodedPath), false, contentFlag, List(sfile), l)
        if(l.indexWhere(_.indexOf("...") != -1) != -1 && contentFlag) {
          if(debug) println("Found '...' Going to map the action for all lines in command.")
          automatedAction(perform, explain, performAll)
        } else {
          automatedAction(perform, explain, performAll)
        }
       
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
  def automatedAction(performLast: Boolean = true, explain: Boolean = false, performAll: Boolean): Unit = {
    
    // Retrieves examples (performed or not) from the previous launches.
    val examples = getAutoHistory(new File(decodedPath))
    
    val c = StringSolver()
    c.setTimeout(2)
    c.setVerbose(debug)
    
    // Exceptions are files for which the action was already performed
    var exceptions = Set[String]()
    if(debug) println(s"$decodedPath $examples, $performLast; $explain, $performAll")
    if(examples != Nil) (if(!explain && !performLast && !performAll) println("Looking for mappings...")) else {
      println("No action to reproduce in this folder")
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
    // Solving the mapping based on the last two examples.
    examples.reverse.take(2).reverse foreach {
      case (nature, performed, contentFlag, files, command) =>
        // Extra time if looking for dots
        if(command.indexOf("...") != -1) {
          c.setTimeout(3)
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
          exceptions += command.last
          exceptions ++= files
        }
        nature match {
          case INPUT_FILE_LIST(n) =>
            is_input_file_list = true
            read_content_file = None
          case INPUT_FILE_CONTENT(n) =>
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
      case Some((INPUT_FILE | INPUT_FILE_LIST(_) | INPUT_FILE_CONTENT(_), performed, contentFlag, files, command)) => true
      case Some((_, performed, contentFlag, files, command)) => false
      case None => true
    }
    if(debug) println(s"Only files: $onlyFiles, nested level = $nested_level")
    
    // files_raw is a array of singletons list of files, either nested or not.
    val files_raw: Array[List[String]] = if(nested_level == 0) {
      new File(decodedPath).list().sortBy(alphaNumericalOrder)
      .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
      .filterNot(if(is_input_file_list) Set.empty else exceptions)
      .map(List(_))
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + File.separator + file)
        .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
        .filterNot(if(is_input_file_list) Set.empty else exceptions)
        .map(List(_))
      }
    } else Array[List[String]]()
    
    //Replacing each file by its name and content if requested
    val files_raw2: Array[List[String]] = if(read_content_file != None && onlyFiles) {
      files_raw.map{listFiles => listFiles.head::readLines(readFile(listFiles.head))}
    } else files_raw
    
    //Replacing each file by its content if the content is read.
    val input = if(is_input_file_list && read_content_file == None && onlyFiles) {
      Array(files_raw2.toList.flatten)
    } else files_raw2

    if(debug) println(s"Input ${input.mkString("")}")
    if(input.length != 0) {
      if(debug) println(s"Solving with at most 2 and then 1")
      
      val attempts = (() => c.solveAll(),   (e: List[FileName]) => c.solve(e))::
                     (() => c.solveLasts(), (e: List[FileName]) => c.solveLast(e))::Nil
      attempts find { case (computation, solver) => 
        computation() match {
          case List(Some(Concatenate(List(ConstStr(a))))) => RETRY
          case l if l.forall(_.nonEmpty) =>
            if(explain || debug) {
              for(ls <- l; prog <- ls) {
               displayProg(prog, lines = (read_content_file != None))
              }
            }
            if(!explain) {
              val mapping = 
                for(f <- input;
                    commandFromFile = solver(f)
                    if commandFromFile forall (_.nonEmpty)) yield {
                  f -> commandFromFile
                }
              suggestMapping(l.map(_.get), mapping, "auto", title= !performLast)
              if(performAll) {
                for((file, command) <- mapping) {
                  auto(command.toList)
                }
              } else if(performLast) {
                mapping.toList.lastOption foreach { case (files, commands) =>
                    auto(commands.toList)
                }
              }
            }
            true
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
    val t = if(title) s"  (Mapping found. Type '$command' to perform it, or '$command -e' to explain)  \n" else ""
    val mappingTrimmed = mapping//.toList.sortBy(_.##).take(5)
    val m = (mappingTrimmed map { case (i: List[FileName], j: Seq[String]) => s"${i mkString ","} -> ${j.mkString(";")}"} mkString "\n")
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
  
  def displayProg(prog: Program, lines: Boolean = false): Unit = {
    val replaceinput = if(lines) "line" else "file name"
    val tmp = Printer(prog)
    val first = if("(?<!first) input".r.findFirstIn(tmp) != None) "first " else ""
    val second = if(lines) "first" else "second"
    val third = if(lines) "second" else "third"
    println(tmp.replaceAll("first input", if(lines) "file name" else s"${first}$replaceinput")
        .replaceAll("all inputs", s"all ${replaceinput}s")
        .replaceAll("second input", s"$second $replaceinput")
        .replaceAll("third input", s"$third $replaceinput")
        .replaceAll(" input ", s" $replaceinput ")
        .replaceAll("line ([a-z][a-z0-9]*)\\+2", "line $1+1"))
  }
  
  def alphaNumericalOrder(f: String): String = {
    var res = f
    for(i <- 1 to 3; j = 4-i ) {
      res = res.replaceAll(s"""(?:[^\\d]|^)(\\d{$i})(?=[^\\d]|$$)""","0"*j+"$1")
    }
    res
  }
}