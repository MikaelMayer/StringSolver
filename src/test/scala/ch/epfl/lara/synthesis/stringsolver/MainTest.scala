package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import java.io.File
import java.io.InputStream
import java.io.ByteArrayInputStream

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
    val tmpIn = System.in
    val tmpUserDir = System.getProperty("user.dir")

    val tmpDir = new File(System.getProperty("java.io.tmpdir"), "tmpMainTest")
    if(!tmpDir.exists()) {
      tmpDir.mkdir()
    }
    
    var c1: File = null
    var c2: File = null
    var c3: File = null
    var cc1: File = null
    var cc2: File = null
    var cc3: File = null
    
    try {
      import Main._
      Main.decodedPath = tmpDir.getAbsolutePath()
      
      c1 = new File(decodedPath, "infoAutocad.log")
      c2 = new File(decodedPath, "mathAnalyse.log")
      c3 = new File(decodedPath, "physiquePlasma.log")
      
      cc1 = new File(decodedPath, "autocad_info_1.txt")
      cc2 = new File(decodedPath, "analyse_math_2.txt")
      cc3 = new File(decodedPath, "plasma_physique_3.txt")
      
      c1.createNewFile()
      c2.createNewFile()
      c3.createNewFile()
      
      System.setIn(new ByteArrayInputStream("n\ny\n".getBytes()))
      Main.automatedMv(List("mv", "infoAutocad.log", "autocad_info_1.txt"))
      Main.automatedMv(List("mv", "mathAnalyse.log", "analyse_math_2.txt"))
      Main.automatedMv(List("mv"))
      c1 should not be 'exists
      c2 should not be 'exists
      c3 should not be 'exists
      cc1 should be('exists)
      cc2 should be('exists)
      cc3 should be('exists)
    } finally {
      System.setIn(tmpIn)
      if(c1!=null&&c1.exists()) c1.delete()
      if(c2!=null&&c2.exists()) c2.delete()
      if(c3!=null&&c3.exists()) c3.delete()
      if(cc1!=null&&cc1.exists()) cc1.delete()
      if(cc2!=null&&cc2.exists()) cc2.delete()
      if(cc3!=null&&cc3.exists()) cc3.delete()
    }
  }
}