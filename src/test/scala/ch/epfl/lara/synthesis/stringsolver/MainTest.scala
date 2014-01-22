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

  
  "Main should keep history" in {
    Main.deleteMvHistory()
    Main.storeMvHistory(new File(Main.decodedPath), "test.txt", "tist.txt")
    Main.storeMvHistory(new File(Main.decodedPath), "tost.txt", "tust.txt")
    println(Main.decodedPath)
    Main.getMvHistory(new File(Main.decodedPath)) should equal (List(("test.txt", "tist.txt"), ("tost.txt", "tust.txt")))
    Main.deleteMvHistory()
    Main.getMvHistory(new File(Main.decodedPath)) should equal (Nil)
  }
  
  "Main should suggest renaming" in {
    val tmpIn = System.in
    val tmpUserDir = System.getProperty("user.dir")

    val tmpDir = new File(System.getProperty("java.io.tmpdir"), "tmpMainTest")
    if(!tmpDir.exists()) {
      tmpDir.mkdir()
    }
    
    tmpDir.list() foreach { e => new File(tmpDir, e).delete()}
    
    var c1: File = null
    var c2: File = null
    var c3: File = null
    var cc1: File = null
    var cc2: File = null
    var cc3: File = null
    
    try {
      import Main._
      Main.decodedPath = tmpDir.getAbsolutePath()
      Main.deleteMvHistory()
      
      
      c1 = new File(decodedPath, "infoAutocad.log")
      c2 = new File(decodedPath, "mathAnalyse.log")
      c3 = new File(decodedPath, "physiquePlasma.log")
      
      cc1 = new File(decodedPath, "autocad_info_1.txt")
      cc2 = new File(decodedPath, "analyse_math_2.txt")
      cc3 = new File(decodedPath, "plasma_physique_3.txt")
      
      c1.createNewFile()
      c2.createNewFile()
      c3.createNewFile()
      
      cc1 should not be('exists)
      cc2 should not be('exists)
      cc3 should not be('exists)
      
      //System.setIn(new ByteArrayInputStream("n\ny\n".getBytes()))
      Main.automatedMv(List("mv", "infoAutocad.log", "autocad_info_1.txt"))
      c1 should not be 'exists
      cc1 should be('exists)
      Main.automatedMv(List("mv", "mathAnalyse.log", "analyse_math_2.txt"))
      c2 should not be 'exists
      cc2 should be('exists)
      cc3 should not be('exists)
      Main.automatedMv(List("mv"))
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
  
  "Main should have a working command line " in {
    val tmpUserDir = System.getProperty("user.dir")
    val tmpDir = new File(System.getProperty("java.io.tmpdir"), "tmpMainTest")
    if(!tmpDir.exists()) {
      tmpDir.mkdir()
    }
    tmpDir.list() foreach { e => new File(tmpDir, e).delete()}
    
    Main.decodedPath = tmpDir.getAbsolutePath()
    Main.auto(List("touch file.txt"))
    val f = new File(tmpDir, "file.txt")
    val p = new File(tmpDir, "file.pdf")
    val f1 = new File(tmpDir, "folder\\file.txt")
    val ff = new File(tmpDir, "folder")
    f should be('exists)
    p should not be ('exists)
    Main.auto(List("mkDir folder;mv file.txt folder/file.txt"))
    f should not be 'exists
    f1 should be ('exists)
    Main.auto(List("convert folder/file.txt file.pdf;rm -r folder"))
    ff should not be 'exists
    p should be ('exists)
    Main.auto(List("rm file.pdf"))
    p should not be ('exists)
  }
  
  "Main should create multiple commands" in {
    //val tmpIn = System.in
    val tmpUserDir = System.getProperty("user.dir")

    val tmpDir = new File(System.getProperty("java.io.tmpdir"), "tmpMainTest")
    if(!tmpDir.exists()) {
      tmpDir.mkdir()
    }
    tmpDir.list() foreach { e => new File(tmpDir, e).delete()}
    
    var c1: File = null
    var c2: File = null
    var c3: File = null
    var a1: IndexedSeq[File] = null
    var a2: IndexedSeq[File] = null
    var a3: IndexedSeq[File] = null
    var b1: IndexedSeq[File] = null
    var b2: IndexedSeq[File] = null
    var b3: IndexedSeq[File] = null
    def as = List(a1, a2, a3)
    def bs = List(b1, b2, b3)
    var cc1: File = null
    var cc2: File = null
    var cc3: File = null
    
    try {
      import Main._
      decodedPath = tmpDir.getAbsolutePath()
      Main.deleteAutoHistory()
      
      c1 = new File(decodedPath, "Algorithms")
      c2 = new File(decodedPath, "Maths")
      c3 = new File(decodedPath, "Algebras")
      
      a1 = for(i <- 1 to 3) yield new File(decodedPath, s"Algorithm$i.txt")
      a2 = for(i <- 1 to 5) yield new File(decodedPath, s"Math$i.txt")
      a3 = for(i <- 1 to 11) yield new File(decodedPath, s"Algebra$i.txt")
      b1 = for(i <- 1 to 3) yield new File(decodedPath, s"Algorithms/Algorithm$i.pdf")
      b2 = for(i <- 1 to 5) yield new File(decodedPath, s"Maths/Math$i.pdf")
      b3 = for(i <- 1 to 11) yield new File(decodedPath, s"Algebras/Algebra$i.pdf")
      
      cc1 = new File(decodedPath, "AlgorithmsBook.pdf")
      cc2 = new File(decodedPath, "MathsBook.pdf")
      cc3 = new File(decodedPath, "AlgebrasBook.pdf")
      
      for(a <- as; f <- a) f.createNewFile()

      //System.setIn(new ByteArrayInputStream("n\ny\n".getBytes()))
      Main.automateCmd(List("auto", "Algorithm1.txt", "mkDir Algorithms;convert Algorithm1.txt Algorithms/Algorithm1.pdf;rm Algorithm1.txt"))
      Main.automateCmd(List("auto"))
      for(a <- as; f <- a) f should not be ('exists)
      
      c1 should be('exists)
      c2 should be('exists)
      c3 should be('exists)
      for(a <- as; f <- a) f should not be 'exists
      for(a <- bs; f <- a) f should be ('exists)
      Main.automateCmd(List("auto", "Algorithms", "convert Algorithms/*.pdf AlgorithmsBook.pdf;rm -rf Algorithms"))
      cc1 should be('exists)
      Main.automateCmd(List("auto"))
      cc2 should be('exists)
      cc3 should be('exists)
      for(a <- bs; f <- a) f should not be ('exists)
      c1 should not be('exists)
      c2 should not be('exists)
      c3 should not be('exists)
      
    } finally {
      //System.setIn(tmpIn)
      if(c1!=null&&c1.exists()) c1.delete()
      if(c2!=null&&c2.exists()) c2.delete()
      if(c3!=null&&c3.exists()) c3.delete()
      for(a <- as; f <- a) if(f != null && f.exists()) f.delete()
      for(a <- bs; f <- a) if(f != null && f.exists()) f.delete()
      if(cc1!=null&&cc1.exists()) cc1.delete()
      if(cc2!=null&&cc2.exists()) cc2.delete()
      if(cc3!=null&&cc3.exists()) cc3.delete()
    }
  }
}