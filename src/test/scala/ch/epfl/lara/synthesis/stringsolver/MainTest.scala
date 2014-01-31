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
  import Main._

  "Service should filter: By extension" in {
    val Some((c, s)) = Service.getFilter(List(("abc.txt",true), ("defg.txt",true), ("abc2.jpg",false)))
    s should equal (".txt")
    c.solve("file.txt") should equal (s)
    c.solve("myotherfile.txt") should equal (s)
    c.solve("file.pdf") should not equal (s)
    c.solve("file.jpg") should not equal (s)
  }
  "Service should filter: By file name prefix" in {
    val Some((c, s)) = Service.getFilter(List(("abc1.txt",true), ("defg1.txt",false), ("abc2.txt",true)))
    s should equal ("abc")
    c.solve("file.txt") should not equal (s)
    c.solve("myotherfile.txt") should not equal (s)
    c.solve("abc3.pdf") should equal (s)
    c.solve("abc10.jpg") should equal (s)
  }
  
  "Service should partition : Name of categories unrelated to the name of files." in {
     //    Example 1: Name of categories unrelated to the name of files.
     val Some((c, c2, s)) = Service.getPartition(List(("abc.txt","blue"), ("defg.txt","blue"), ("hijk.jpg","red"), ("lmnop.jpg","red")))
     c.solve("file.txt") should equal (".txt")
     c.solve("file.jpg") should equal (".jpg")
     c.solve("file.pdf") should equal (".pdf")
     c2.solve(".txt") should equal ("")
     s(".txt") should equal ("blue")
     s(".jpg") should equal ("red")
     s(".pdf") should equal (".pdf")
  }
  
  "Service should partition : Categories are numbered." in {
     // Example 2: Categories are numbered
     val Some((c, c2, s)) = Service.getPartition(List(("abc.txt","1"), ("defg.txt","1"), ("hijk.jpg","2"), ("lmnop.jpg","2")))
     c.solve("file.txt") should equal (".txt")
     c.solve("file.jpg") should equal (".jpg")
     c.solve("file.pdf") should equal (".pdf")
     //c2.solve(".txt") should equal ("3") // Do not call because else the counter will increment and the following tests would be wrong
     s(".txt") should equal ("1")
     s(".jpg") should equal ("2")
     s(".pdf") should equal ("3")
     s(".png") should equal ("4")
  }
  
  "Service should partition : Name of categories related to the name of files." in {
     //Example 3: Name of categories related to the name of files.
     val Some((c, c2, s)) = Service.getPartition(List(("abc.txt","category-txt"), ("defg.txt","category-txt"), ("hijk.jpg","category-jpg"), ("lmnop.jpg","category-jpg")))
     c.solve("file.txt") should equal (".txt")
     c.solve("file.jpg") should equal (".jpg")
     c.solve("file.pdf") should equal (".pdf")
     c2.solve(".pdf") should equal ("category-pdf")
     s(".txt") should equal ("category-txt")
     s(".jpg") should equal ("category-jpg")
     s(".pdf") should equal ("category-pdf")
     s(".png") should equal ("category-png")
  }
  
  "Main should keep history" in {
    Main.deleteMvHistory()
    val path = new File(Main.decodedPath).getAbsolutePath()
    Main.timeStampGiver = () => "T1"
    Main.storeMvHistory(MvLog(path, true, "test.txt", "tist.txt"))
    Main.timeStampGiver = () => "T2"
    Main.storeMvHistory(MvLog(path, false, "tost.txt", "tust.txt"))
    println(Main.decodedPath)
    Main.getMvHistory(new File(Main.decodedPath)) should equal (List(MvLog(path, true, "test.txt", "tist.txt", "T1"), MvLog(path, false, "tost.txt", "tust.txt", "T2")))
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
      
      cc1 = new File(decodedPath, "01_autocad_info.txt")
      cc2 = new File(decodedPath, "02_analyse_math.txt")
      cc3 = new File(decodedPath, "03_plasma_physique.txt")
      
      c1.createNewFile()
      c2.createNewFile()
      c3.createNewFile()
      
      cc1 should not be('exists)
      cc2 should not be('exists)
      cc3 should not be('exists)
      
      //System.setIn(new ByteArrayInputStream("n\ny\n".getBytes()))
      Main.parseMvCmd(List(c1.getName, cc1.getName), Options())
      c1 should not be 'exists
      cc1 should be('exists)
      Main.parseMvCmd(List(c2.getName, cc2.getName), Options())
      c2 should not be 'exists
      cc2 should be('exists)
      cc3 should not be('exists)
      Main.parseMvCmd(List(), Options())
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
      Main.parseAutoCmd(List("Algorithm1.txt", "mkDir Algorithms;convert Algorithm1.txt Algorithms/Algorithm1.pdf;rm Algorithm1.txt"), Options(performAll=true))
      for(a <- as; f <- a) f should not be ('exists)
      
      c1 should be('exists)
      c2 should be('exists)
      c3 should be('exists)
      for(a <- as; f <- a) f should not be 'exists
      for(a <- bs; f <- a) f should be ('exists)
      Main.parseAutoCmd(List("Algorithms", "convert Algorithms/*.pdf AlgorithmsBook.pdf;rm -rf Algorithms"), Options(performAll=true))
      cc1 should be('exists)
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