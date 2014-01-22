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

/**
 * Usage
 * create an executable script named mv containing:
 * 
 * java -jar `dirname $0`/flash-fill_2.10-1.0.jar "$@"
 */
object Main {
  import Implicits._
  
  final val debug = true
  
  final object Move {
    def unapply(s: String): Option[Unit] = {
      if(s == "mv" || s.toLowerCase() == "move") {
        Some(())
      } else None
    }
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
  }
  
  
  trait REPFL_ANSWER
  object FINISH extends REPFL_ANSWER
  object CONTINUE extends REPFL_ANSWER
  
  
  //val path = Main.getClass().getProtectionDomain().getCodeSource().getLocation().getPath()
  var decodedPath = System.getProperty("user.dir");//URLDecoder.decode(path, "UTF-8");
  
  val HISTORY_DIR = "StringSolverRenaming"
  val HISTORY_MV_FILE = "mv.log"
  val HISTORY_AUTO_FILE = "auto.log"
  
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
  def storeMvHistory(dir: File, file: String, to: String): Unit = {
    getHistoryFile(HISTORY_MV_FILE) map { history_file =>
      try {
        if(debug) println(s"Writing in history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
        val date= new java.util.Date();
        val time = new Timestamp(date.getTime()).toString();
   
        //println("Timestamp")
        out.println(time + ";" + dir.getAbsolutePath() + ";" + file+";"+to);
        out.close();
      } catch  {
        case e: IOException =>println("ioexception")
      }
    }
  }
  
  /**
   * Recovesr all renaming in history which happened in this folder
   */
  def getMvHistory(folder: File): Seq[(String, String)] = {
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
            splitted = line.split(";") if splitted.length == 4;
            Array(time, dir, file1, file2) = splitted;
            if(checkDir(dir))) yield (file1, file2)
      res
      }
    } getOrElse (List[(String, String)]())
  }
  
  /**
   * Main function
   */
  def main(args: Array[String]): Unit = {
    val cmd = args.toList
    cmd match {
      case Auto()::q =>    automateCmd(cmd)
      case Move()::q =>    automatedMv(cmd)
      case Convert()::q => automatedConvert(cmd)
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
    c.setTimeout(2)
    var exceptions = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    if(examples != Nil) (if(!explain && !perform) println("Looking for mappings...")) else if(explain) return ();
    
    var nested_level = 0
    examples.reverse.take(2).reverse foreach {
      case (in, out) =>
        if(debug) println(s"Adding $in, $out")
        c.add(List(in), List(out))(0)
        nested_level = in.count(_ == '/') + in.count(_ == '\\')
        exceptions += out
    }
    
    
    val files: Array[String] = if(nested_level == 0) {
      new File(decodedPath).list()
      .filter(file => !new File(decodedPath, file).isDirectory()).filterNot(exceptions)
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().map(file => theDir.getName() + File.separator + file)
        .filter(file => !new File(decodedPath, file).isDirectory())
        .filterNot(exceptions)
      }
    } else Array[String]()
    if(files.length != 0) {       
      //println("Solving problem...")
      if(debug) println(s"Solving with at most 2")
      
      val attempts = (() => c.solve(),   (e: String) => c.solve(List(e)))::
                     (() => c.solveLast(), (e: String) => c.solveLast(List(e)))::Nil
      attempts find { case (computation, solver) => 
        computation() match {
          case Some(Concatenate(List(ConstStr(a)))) => true
          case Some(prog) =>
            if(debug) displayProg(prog)
            if(explain) {
             displayProg(prog)
            } else {
              val mappedFiles = files map { f => solver(f) }
              val mapping = files zip mappedFiles filterNot { case (f, m) => m.exists(_ == "") }
              if(mapping.nonEmpty) {
                if(perform) {
                  for((file, to) <- mapping) {
                    move(file, to.head)
                  }
                  suggestMapping(List(prog), mapping, title=false)
                } else {
                  suggestMapping(List(prog), mapping)
                }
              }
            }
            true
          case None =>
            if(debug) println(s"no program found")
            false
        }
      }
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
  def automatedMv(cmd: List[String]): Unit = {
    cmd match {
      case Move()::("-c" | "--clear")::_ => // Automated move.
        deleteMvHistory()
      case Move()::("-e" | "--explain")::_ => // Automated move.
        automatedRenaming(perform=false, explain=true)
      case Move()::("-t" | "--test")::_ => // Automated move.
        automatedRenaming(perform=false, explain=false)
      case Move()::Nil => // Automated move.
        automatedRenaming()
        
      case Move()::sfile1::sfile2::Nil =>
        val file1 = if(sfile1.indexOf("\\") != -1 || sfile1.indexOf("/") != -1) new File(sfile1) else new File(decodedPath, sfile1)
        val file2 = if(sfile2.indexOf("\\") != -1 || sfile2.indexOf("/") != -1) new File(sfile2) else new File(decodedPath, sfile2)
        if(!file1.exists()) { println(s"file $sfile1 does not exist"); return(); }
        move(sfile1, sfile2)
        storeMvHistory(new File(decodedPath), sfile1, sfile2)
        automatedRenaming(false)
      case Move()::_ => println("The mv command takes exactly two parameters")
        
      case other => println("Unkown function : " + other.mkString(" "))
    }
  }
  
  
  def automatedConvert(cmd: List[String]): Unit = {
    cmd match {
      case Convert()::("-c" | "--clear")::_ =>
      case _ =>
    }
  }
  
  
  /**
   * Stores a renaming in the rename history
   */
  def storeAutoHistory(dir: File, file: String, cmd: List[String]): Unit = {
    getHistoryFile(HISTORY_AUTO_FILE) foreach { history_file =>
      try {
        if(debug) println(s"Writing in history")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
        val date= new java.util.Date();
        val time = new Timestamp(date.getTime()).toString();
        val nature = if(new File(dir, file).isDirectory()) "DIRECTORY" else "FILE"
        //println("Timestamp")
        out.println(time + "||" + dir.getAbsolutePath() + "||" + file + "||" + nature + "||" + cmd.mkString("||"));
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
  def getAutoHistory(folder: File): Seq[(String, String, List[String])] = {
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
            splitted = line.split("\\|\\|").toList if splitted.length >= 5;
            time::dir::file::nature::command = splitted;
            if(checkDir(dir))) yield (file, nature, command)
      res
      }
    } getOrElse (List[(String, String, List[String])]())
  }
  
  /**
   * Generic automated commands.
   */
  def automateCmd(cmd: List[String]): Unit = {
    cmd match {
      case Auto()::("-c" | "--clear")::_ =>
        deleteAutoHistory()
      case Auto()::Nil =>
        automatedAction(perform=true, explain=false)
      case Auto()::("-e" | "--explain")::_ =>
        automatedAction(perform=false,explain=true)
      case Auto()::("-t" | "--test")::_ =>
        automatedAction(perform=false, explain=false)
      case Auto()::sfile::l =>
        val file1 = if(sfile.indexOf("\\") != -1 || sfile.indexOf("/") != -1) new File(decodedPath, sfile) else new File(decodedPath, sfile)
        if(!file1.exists()) { println(s"file $sfile does not exist"); return(); }
        storeAutoHistory(new File(decodedPath), sfile, l)
        auto(l)
        automatedAction(false, false)
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
    var exceptions = Set[String]()
    if(debug) println(s"$decodedPath $examples, $perform; $explain")
    if(examples != Nil) (if(!explain && !perform) println("Looking for mappings...")) else {
      println("No action to reproduce in this folder")
      return ();
    }
    
    var nested_level = 0
    examples.reverse.take(2).reverse foreach {
      case (file, nature, command) =>
        if(debug) println(s"Adding $file, $command")
        c.add(List(file), command)(0)
        nested_level = file.count(_ == '/') + file.count(_ == '\\')
        exceptions += command.last
        exceptions += file
    }
    // All files should have the same nature.
    val onlyFiles = examples.lastOption match {
      case Some((file, nature, command)) => nature == "FILE"
      case None => true
    }
    
    val files: Array[String] = if(nested_level == 0) {
      new File(decodedPath).list()
      .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles).filterNot(exceptions)
    } else if(nested_level == 1){
      new File(decodedPath).listFiles().filter(_.isDirectory()).flatMap{ theDir =>
        theDir.list().map(file => theDir.getName() + File.separator + file)
        .filter(file => new File(decodedPath, file).isDirectory() ^ onlyFiles)
        .filterNot(exceptions)
      }
    } else Array[String]()
    if(files.length != 0) {       
      //println("Solving problem...")
      if(debug) println(s"Solving with at most 2 and then 1")
      
      val attempts = (() => c.solveAll(),   (e: List[String]) => c.solve(e))::
                     (() => c.solveLasts(), (e: List[String]) => c.solveLast(e))::Nil
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
                  mappedFile = solver(List(f))
                  if mappedFile forall (_.nonEmpty);
                  dummy = if(perform) auto(mappedFile.toList) else ()) yield {
                f -> mappedFile
              }
              suggestMapping(l.map(_.get), mapping, title= !perform)
            }
            true
          case _ =>
            if(debug) println(s"No program found")
            false
        }
      }
    }
  }
  
  
  def auto(cmd: List[String]): Unit = {
    if(System.getProperty("os.name").contains("indow")) {
      val cmdString = "\""+cmd.mkString(";").replaceAll("\"","\\\"").replaceAll("""\bconvert """, """convert.exe """) +"\""
      val env = collection.mutable.Map[String, String]() ++ System.getenv();
      val key = if(env contains "Path") "Path" else if(env contains "PATH") "PATH" else throw new Error("No Path or PATH variable found in environment")
      env(key) += ";c:\\cygwin\\bin;c:\\Windows\\System32"
      val envString = (env.map{ case (k, v) => k + "=" + v }).toArray
      val p = Runtime.getRuntime().exec(Array[String]("C:\\cygwin\\bin\\bash.exe","-c",cmdString),
                                 envString,
                                 new File(decodedPath));
      p.waitFor()
    } else {
      Runtime.getRuntime().exec(cmd.toArray, Array[String](), new File(decodedPath))
    }
  }
  
  
  def suggestMapping(prog: List[Program], mapping: Array[(String, Seq[String])], title: Boolean = true) = {
    val t = if(title) "  (Mapping found. Type 'mv' to perform it, 'mv -e' to explain)  \n" else ""
    val m = (mapping map { case (i: String, j: Seq[String]) => s"$i -> ${j.mkString(";")}"} mkString "\n")
    println(t+m)
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
  def ask(s: String, commands: Seq[(Char, String, REPFL_ANSWER, ()=>Unit)]): Unit = {
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
  
  def displayProg(prog: Program): Unit = {
    println(Printer(prog).replaceAll("first input", "file name"))
  }
}