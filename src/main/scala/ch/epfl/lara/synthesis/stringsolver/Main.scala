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

/**
 * Usage
 * create an executable script named mv containing:
 * 
 * java -jar `dirname $0`/flash-fill_2.10-1.0.jar "$@"
 */
object Main {
  import Implicits._
  
  final object Move {
    def unapply(s: String): Option[Unit] = {
      if(s == "mv" || s.toLowerCase() == "move") {
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
  val HISTORY_FILE = "mv.log"
  
  /**
   * Returns an history file.
   */
  private def getHistoryFile(): Option[File] = {
    val tmpDir = System.getProperty("java.io.tmpdir")
    val history_dir = new File(tmpDir, HISTORY_DIR)
    if(!history_dir.exists()) {
      history_dir.mkdir()
    }
    val history_file = new File(history_dir, HISTORY_FILE)
    if(if(!history_file.exists) history_file.createNewFile() else true) {
      Some(history_file)
    } else None
  }
  
  def deleteHistory() = {
    getHistoryFile() map (_.delete())
  }
  
  /**
   * Store a renaming in the rename history
   */
  def storeRenameHistory(file: File, to: File): Unit = {
    //println(s"recover history file for $file to $to")
    getHistoryFile() map { history_file =>
      try {
         //println(s"Creating out in $history_file")
          val out = new PrintWriter(new BufferedWriter(new FileWriter(history_file.getAbsoluteFile(), true)));
          
          val date= new java.util.Date();
          val time = new Timestamp(date.getTime()).toString();
   
          // println("Timestamp")
          out.println(time + ";" + file.getAbsolutePath()+";"+to.getAbsolutePath());
          out.close();
      } catch  {
        case e: IOException =>// println("ioexception")
      }
    }
  }
  /**
   * Recover all renaming in history
   */
  def recoverHistoryForFolder(path: File): Seq[(String, String)] = {
    val dir = if(path.isDirectory()) path.getAbsoluteFile() else new File(path.getParent()).getAbsolutePath()
    
    val checkFile = (s: String) => new File(new File(s).getParent()).getAbsoluteFile() == dir
      
    getHistoryFile() map { history_file =>
      val encoded = Files.readAllBytes(Paths.get(history_file.getAbsolutePath()));
      val content = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(encoded)).toString();
      if(content.isEmpty) Nil
      else {
      val res = 
        for(line <- content.filterNot(_ == '\r').split("\n").toList;
            Array(time, file1, file2) = line.split(";");
            if(checkFile(file1))) yield (new File(file1).getName(), new File(file2).getName())
      res
      }
    } getOrElse (List[(String, String)]())
  }
  
  /**
   * Main function
   */
  def main(args: Array[String]): Unit = {
    val cmd = args.toList
    automatedMv(cmd)
  }
  
  /**
   * Performs automated renaming suggestion
   * @param perform Indicates if renaming is applied.
   */
  def automatedRenaming(perform: Boolean = true, explain: Boolean = false): Unit = {
    val examples = recoverHistoryForFolder(new File(decodedPath))
    val c = StringSolver()
    c.setTimeout(2)
    var exceptions = Set[String]()
    if(examples != Nil) (if(!explain && !perform) println("Looking for mappings...")) else return ();
    examples.reverse.take(2).reverse foreach {
      case (in, out) =>
        c.add(List(in), List(out))(0)
        exceptions += out
    }
    c.solve() match {
      case Some(Concatenate(List(ConstStr(a)))) =>
      case Some(prog) =>
        if(explain) {
          println(Printer(prog))
        } else {
          val files = new File(decodedPath).list()
             .filter(file => !new File(decodedPath, file).isDirectory()).filterNot(exceptions)
          if(files.length != 0) {
            val mappedFiles = files map { f => c.solve(f) }
            val mapping = files zip mappedFiles
            if(perform) {
              for((file, to) <- mapping) {
                move(file, to)
              }
              suggestMapping(prog, mapping, title=false)
            } else {
              suggestMapping(prog, mapping)
            }
          }
        }
      case None =>
        if(!explain && !perform) println("No mappings found")
    }
  }
  
  /**Enhanced move command
   * Feature list:
   * - "mv file1 file2" will move file1 to file2, and then 
   * - "mv" will look at the previous history and the last two moves, and perform a generalization to all files
   */
  def automatedMv(cmd: List[String]): Unit = {
    cmd match {
      case Move()::"-e"::_ => // Automated move.
        automatedRenaming(perform=false, explain=true)
      case Move()::Nil => // Automated move.
        automatedRenaming()
        
      case Move()::sfile1::sfile2::Nil =>
        val file1 = if(sfile1.indexOf("\\") != -1 || sfile1.indexOf("/") != -1) new File(sfile1) else new File(decodedPath, sfile1)
        val file2 = if(sfile2.indexOf("\\") != -1 || sfile2.indexOf("/") != -1) new File(sfile2) else new File(decodedPath, sfile2)
        if(!file1.exists()) { println(s"file $sfile1 does not exist"); return(); }
        val dir = file1.getAbsolutePath().getDirectory
        val files = new File(dir).list().filter(file => !new File(file).isDirectory())
        move(sfile1, sfile2)
        storeRenameHistory(file1, file2)
        automatedRenaming(false)
        /*if(files.length != 0 && files(0) == sfile1.getFile) {
          // see if there is a pattern in renaming.
          val c = StringSolver()
          c.setTimeout(4)
          c.add(List(sfile1), List(sfile2))
          c.solve() match {
            case Some(Concatenate(List(ConstStr(a)))) =>
            case Some(prog) =>
              val mappedFiles = files map { f => c.solve(f) }
              val mapping = files zip mappedFiles
              askIfMapping(prog, mapping)
            case None => 
          }
        }*/
      case Move()::_ => println("The mv command takes exactly two parameters")
        
      case other => println("Unkown function : " + other.mkString(" "))
    }
  }
  
  def suggestMapping(prog: Program, mapping: Array[(String, String)], title: Boolean = true) = {
    val t = if(title) "  (Mapping found. Type 'mv' to perform it, 'mv -e' to explain)  \n" else ""
    val m = (mapping map { case (i: String, j: String) => s"$i -> $j"} mkString "\n")
    println(t+m)
    /*val question = "Apply the following transformation to all files?\n" + m
    ask(question, 
    Seq(('y', "Continue renaming",     FINISH, () =>
          for((file, to) <- mapping) {
            move(file, to)
          }),
        ('\n',"",         FINISH,   () => ()), 
        ('n', "Abort",    FINISH,   () => ()), 
        ('e', "explain",  CONTINUE, () => println(Printer(prog))), 
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

    // if the directory does not exist, create it
    if (!theDir.exists()) {
      //System.out.println("creating directory: " + directoryName);
      val result = theDir.mkdir();  
  
      //if(result) {    
      //  System.out.println("DIR created");  
      //}
    }
    new File(decodedPath, file).renameTo(new File(theDir, to))
    
    //s"move $file $to".!!
    //println(s"move $file $to")
  }
}