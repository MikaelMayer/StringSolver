package ch.epfl.lara.synthesis.stringsolver

import scala.sys.process._
import java.net.URLDecoder
import java.io.File
import Programs._
import Evaluator._
import scala.language.postfixOps
import java.io.IOException
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import java.sql.Timestamp
import java.nio.file._
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer
import scala.language.postfixOps
import scala.sys.process._
import Evaluator._
import Programs._
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import java.io.BufferedReader
import java.io.InputStreamReader

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
  
  final val INPUT_DIRECTORY = "INPUT_DIRECTORY"
  final val INPUT_FILE = "INPUT_FILE"
  object INPUT_FILE_LIST {
    val prefix = "INPUT_FILE_LIST"
    def apply(n: Int) = prefix+n.toString
    def unapply(s: String): Option[Int] = if(s.startsWith(prefix)) Some(s.substring(prefix.length).toInt) else None
  }
    
  type Nature = String
  type FileName = String
  type Executed = Boolean
  
  
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
  
  
  trait REPFL_ANSWER
  object FINISH extends REPFL_ANSWER
  object CONTINUE extends REPFL_ANSWER
  
  
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
    getHistoryFile(HISTORY_MV_FILE) map (_.delete())
  }
  def deleteAutoHistory() = {
    getHistoryFile(HISTORY_AUTO_FILE) map (_.delete())
  }
  
  /**
   * Stores a renaming in the rename history
   */
  def storeMvHistory(dir: File, executed: Boolean, file: String, to: String): Unit = {
    getHistoryFile(HISTORY_MV_FILE) map { history_file =>
      try {
        if(debug) println(s"Writing in mv history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
        val date= new java.util.Date();
        val time = new Timestamp(date.getTime()).toString();
   
        //println("Timestamp")
        val line=time + ";" + dir.getAbsolutePath() + ";" + executed.toString + ";" + file+";"+to
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
  def getMvHistory(folder: File): Seq[(Executed, FileName, FileName)] = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dir
    getHistoryFile(HISTORY_MV_FILE) map { history_file =>
      if(debug) println(s"history file : $history_file")
      val encoded = Files.readAllBytes(Paths.get(history_file.getAbsolutePath()));
      val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
      if(content.isEmpty) Nil
      else {
      val res = 
        for(line <- content.filterNot(_ == '\r').split("\n").toList;
            splitted = line.split(";") if splitted.length == 5;
            Array(time, dir, executed, file1, file2) = splitted;
            if(checkDir(dir))) yield (executed.toBoolean, file1, file2)
      res
      }
    } getOrElse (List[(Executed, FileName, FileName)]())
  }
  
  /**
   * Remove the directory from a given move command.
   */
  def removeDirectoryFromMvHistory(folder: File): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s != dir
    try {
      getHistoryFile(HISTORY_MV_FILE) map { history_file =>
        val encoded = Files.readAllBytes(Paths.get(history_file.getAbsolutePath()));
        val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- content.filterNot(_ == '\r').split("\n").toList;
              splitted = line.split(";") if splitted.length == 5;
              Array(time, dir, executed, file1, file2) = splitted;
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
      case Auto()::q =>    automateCmd(cmd)
      case Move()::q =>    automatedMv(cmd)
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
      case (executed, in, out) =>
        if(debug) println(s"Adding $in, $out")
        c.add(List(in), List(out))(0)
        nested_level = in.count(_ == '/') + in.count(_ == '\\')
        if(executed) exceptions += out
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
            true
          case None =>
            if(debug) println(s"no program found")
            false
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
  def automatedMv(cmd: List[String], perform: Boolean=true, explain: Boolean=false, doall: Boolean = false): Unit = {
    cmd match {
      case Move()::("-c" | "--clear")::_ => // Clears the history
        deleteMvHistory()
      case Move()::("-e" | "--explain")::remaining => // Explain the algorithm
        automatedMv(Move()::remaining, perform=false, explain=true, doall=doall)
      case Move()::("-t" | "--test")::remaining => // Show what the algorithm would do
        automatedMv(Move()::remaining, perform=false, explain=explain, doall=doall)
      case Move()::("-a" | "--auto")::remaining => // Generalizes the command
        automatedMv(Move()::remaining, perform=perform, explain=explain, doall=true)
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
  def storeAutoHistory(dir: File, executed: Boolean, input_files: List[FileName], cmd: List[String]): Unit = {
    getHistoryFile(HISTORY_AUTO_FILE) foreach { history_file =>
      try {
        if(debug) println(s"Writing in history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
        val date= new java.util.Date();
        val time = new Timestamp(date.getTime()).toString();
        val nature = input_files match {
          case List(file) => if(new File(dir, file).isDirectory()) INPUT_DIRECTORY else INPUT_FILE
          case Nil => throw new Error("No input file provided")
          case l => INPUT_FILE_LIST(input_files.length)
        }
        val line = time + "||" + dir.getAbsolutePath() + "||" + executed + "||" + nature + "||" + (input_files++cmd).mkString("||")
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
  def getAutoHistory(folder: File): Seq[(Nature, Executed, List[FileName], List[String])] = {
    val dirFolder = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s == dirFolder
    getHistoryFile(HISTORY_AUTO_FILE) map { history_file =>
      if(debug) println(s"history file : $history_file")
      val encoded = Files.readAllBytes(Paths.get(history_file.getAbsolutePath()));
      val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
      if(content.isEmpty) Nil
      else {
      val res = 
        for(line <- content.filterNot(_ == '\r').split("\n").toList;
            splitted = line.split("\\|\\|").toList if splitted.length >= 6;
            time::dir::executed::nature::filesAndCommand = splitted;
            if(checkDir(dir))) yield {
          nature match {
            case INPUT_FILE | INPUT_DIRECTORY => (nature, executed.toBoolean, List(filesAndCommand.head), filesAndCommand.tail)
            case INPUT_FILE_LIST(n) => (nature, executed.toBoolean, filesAndCommand.take(n), filesAndCommand.drop(n))
          }
        }
      res
      }
    } getOrElse (List[(Nature, Executed, List[FileName], List[String])]())
  }
  
  /**
   * Remove the directory from a given move command.
   */
  def removeDirectoryFromAutoHistory(folder: File): Unit = {
    val dir = if(folder.isDirectory()) folder.getAbsolutePath() else new File(folder.getParent()).getAbsolutePath()
    val checkDir = (s: String) => s != dir
    try {
      getHistoryFile(HISTORY_AUTO_FILE) map { history_file =>
        val encoded = Files.readAllBytes(Paths.get(history_file.getAbsolutePath()));
        val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
        val lines = if(content.isEmpty) Nil
        else {
          for(line <- content.filterNot(_ == '\r').split("\n").toList;
            splitted = line.split("\\|\\|").toList if splitted.length >= 6;
            time::dir::executed::nature::filesAndCommand = splitted;
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
   */
  def automateCmd(cmd: List[String], perform: Boolean = true, explain: Boolean = false, doall: Boolean = false): Unit = {
    if(debug) println(s"Action $cmd")
    cmd match {
      case Auto()::("-c" | "--clear")::_ =>
        deleteAutoHistory()
      case Auto()::Nil =>
        automatedAction(perform=perform, explain=explain)
      case Auto()::("-e" | "--explain")::remaining =>
        automateCmd(Auto()::remaining, perform=false, explain = true, doall=doall)
      case Auto()::("-t" | "--test")::remaining =>
        automateCmd(Auto()::remaining, perform=false, explain=explain, doall=doall)
      case Auto()::("-a" | "--auto")::remaining => // Generalizes the command
        automateCmd(Auto()::remaining, perform=perform, explain=explain, doall=true)
      case Auto()::command:: Nil => // Automatically detect where the file is and which one it should be applied to.
        val s = new File(decodedPath).list().toList
        if(command.indexOf("...") != -1) {
          if(debug) println("Found '...' Going to decode the action.")
          storeAutoHistory(new File(decodedPath), false, s, command::Nil)
          automateCmd(Auto()::Nil, perform, explain)
        } else {
          if(debug) println("No input provided. Going to try to extract output from file name")
          val candidates = s.filter{ f => command.indexOf(f) != -1 }.sortBy(_.size).lastOption
          candidates match {
            case Some(file) =>
              automateCmd(Auto()::file::command::Nil, perform, explain, doall)
            case None =>
              // If there are three dots in the command, tries to recognize the command
          }
        }
      case Auto()::sfile::l if l != Nil =>
        val file1 = if(sfile.indexOf("\\") != -1 || sfile.indexOf("/") != -1) new File(decodedPath, sfile) else new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        storeAutoHistory(new File(decodedPath), perform, List(sfile), l)
        if(perform) auto(l) // performs the action
        automatedAction(doall, explain)
      case _ =>
    }
  }
  
  /**
   * Performs automated renaming suggestion
   * @param perform Indicates if renaming is applied.
   * @param explain Indicates if the algorithm is explained. If both are false, displays the mapping.
   */
  def automatedAction(perform: Boolean = true, explain: Boolean = false): Unit = {
    val examples = getAutoHistory(new File(decodedPath))
    val c = StringSolver()
    c.setTimeout(2)
    c.setVerbose(false)
    var exceptions = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    if(examples != Nil) (if(!explain && !perform) println("Looking for mappings...")) else {
      println("No action to reproduce in this folder")
      return ();
    }
    
    var nested_level = 0
    var recreate_command = false // Recreates an incomplete command containing '...'
    examples.reverse.take(2).reverse foreach {
      case (nature, executed, files, command) =>
        if(debug) println(s"Adding $files, $command")
        c.add(files.take(3), command)(0)
        nested_level = files.head.count(_ == '/') + files.head.count(_ == '\\')
        exceptions += command.last
        if(executed) exceptions ++= files
        nature match {
          case INPUT_FILE_LIST(n) => recreate_command = true
          case _ => recreate_command = false
        }
    }
    
    // All files should have the same nature.
    val onlyFiles = examples.lastOption match {
      case Some((INPUT_FILE | INPUT_FILE_LIST(), executed, files, command)) => true
      case Some((_, executed, files, command)) => false
      case None => true
    }
    if(debug) println(s"Only files: $onlyFiles, nested level = $nested_level")
    
    val files_raw: Array[List[FileName]] = if(nested_level == 0) {
      new File(decodedPath).list().sortBy(alphaNumericalOrder)
      .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
      .filterNot(if(recreate_command) Set.empty else exceptions)
      .map(List(_))
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().sortBy(alphaNumericalOrder)
        .map(file => theDir.getName() + File.separator + file)
        .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
        .filterNot(if(recreate_command) Set.empty else exceptions)
        .map(List(_))
      }
    } else Array[List[FileName]]()
    val files = if(recreate_command) {
      Array(files_raw.toList.flatten)
    } else files_raw
    if(debug) println(s"File mapping: ${files.mkString("")}")
    if(files.length != 0) {       
      if(debug) println(s"Solving with at most 2 and then 1")
      
      val attempts = (() => c.solveAll(),   (e: List[FileName]) => c.solve(e))::
                     (() => c.solveLasts(), (e: List[FileName]) => c.solveLast(e))::Nil
      attempts find { case (computation, solver) => 
        computation() match {
          case List(Some(Concatenate(List(ConstStr(a))))) => false
          case l if l.forall(_.nonEmpty) =>
            if(explain || debug) {
              for(ls <- l; prog <- ls) {
               displayProg(prog)
              }
            }
            if(!explain) {
              val mapping = for(f <- files;
                  mappedFile = solver(f)
                  if mappedFile forall (_.nonEmpty);
                  dummy = if(perform) auto(mappedFile.toList) else ()) yield {
                f -> mappedFile
              }
              suggestMapping(l.map(_.get), mapping, "auto", title= !perform)
            }
            true
          case _ =>
            if(debug) println(s"No program found")
            false
        }
      }
    }
    if(perform) {
      removeDirectoryFromAutoHistory(new File(decodedPath))
    }
  }
  
  
  def auto(cmd: List[String]): Unit = {
    val p = if(System.getProperty("os.name").contains("indow")) {
      val cygwinbindir = "C:\\cygwin\\bin"
      val cygwinbash = cygwinbindir + "\\bash.exe"
      
      if(!new File(cygwinbash).exists) {
        println(s"Cygwin bin directory not found $cygwinbindir")
        return
      }
      val cmdString = "\""+cmd.mkString(";").replaceAll("\"","\\\"").replaceAll("""\bconvert """, """convert.exe """) +"\""
      val env = collection.mutable.Map[String, String]() ++ System.getenv();
      val key = if(env contains "Path") "Path" else if(env contains "PATH") "PATH" else throw new Error("No Path or PATH variable found in environment")
      env(key) += ";"+cygwinbindir+";c:\\Windows\\System32"
      val envString = (env.map{ case (k, v) => k + "=" + v }).toArray
      Runtime.getRuntime().exec(Array[String](cygwinbash,"-c",cmdString),
                                 envString,
                                 new File(decodedPath));
    } else {
      Runtime.getRuntime().exec(Array("/bin/bash", "-c") ++ cmd.toArray, Array[String](), new File(decodedPath))
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
  
  def displayProg(prog: Program): Unit = {
    val tmp = Printer(prog)
    val first = if(tmp.indexOf("second input") != -1) "first " else ""
    println(tmp.replaceAll("first input", s"${first}file name")
        .replaceAll("all inputs", "all file names")
        .replaceAll("second input", "second file name"))
  }
  
  def alphaNumericalOrder(f: String): String = {
    var res = f
    for(i <- 1 to 3; j = 4-i ) {
      res = res.replaceAll(s"""(?:[^\\d]|^)(\\d{$i})(?=[^\\d]|$$)""","0"*j+"$1")
    }
    res
  }
}