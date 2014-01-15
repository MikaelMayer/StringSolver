package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec 
import java.io.File

class MainTest extends WordSpec with ShouldMatchers {
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import StringSolver._
  import Implicits._
  import Evaluator._
  
  def renderer(c: StringSolver): Unit = {
    c.solve() match {
      case Some(prog) => println(Printer(prog))
      case none => println("No program found")
    }
  }
  
  "Main should keep history" in {
    Main.deleteHistory()
    Main.storeRenameHistory(new File(Main.decodedPath, "test.txt"), new File(Main.decodedPath, "tist.txt"))
    Main.storeRenameHistory(new File(Main.decodedPath, "tost.txt"), new File(Main.decodedPath, "tust.txt"))
    println(Main.decodedPath)
    Main.recoverHistoryForFolder(new File(Main.decodedPath)) should equal (List(("test.txt", "tist.txt"), ("tost.txt", "tust.txt")))
    Main.deleteHistory()
    Main.recoverHistoryForFolder(new File(Main.decodedPath)) should equal (Nil)
  }
  
  "Main should suggest renaming" in {
    Main.automatedMv(List("mv", "test.txt", "test.pdf"))
    Main.automatedMv(List("mv", "orange.txt", "orange.pdf"))
    
  }
}