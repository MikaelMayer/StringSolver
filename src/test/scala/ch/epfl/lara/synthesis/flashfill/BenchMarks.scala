package ch.epfl.lara.synthesis.flashfill

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec 

class BenchMarks extends WordSpec  with ShouldMatchers {
  import ch.epfl.lara.synthesis.flashfill._
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import FlashFill._
  import Implicits._
  import Evaluator._
  
  def renderer(c: FlashFillSolver): Unit = {
    println(Printer(c.solve().get))
  }
  
  "1. Renaming files with counters http://stackoverflow.com/questions/4631123/complex-file-rename" in {
    val c = FlashFill()
    c.add(List("AB1234.gif"), "AB-0111-1.gif")
    c.add(List("B3245.gif"), "B-0111-2.gif")
    c.add(List("AB2541.jpg"), "AB-0111-3.jpg")
    c.add(List("AB11422.gif"), "AB-0111-4.gif")
    renderer(c)
    c.solve(List("AB5973.jpg"))(0) should equal ("AB-0111-5.jpg")
  }

  "2. Renaming movie files http://www.unix.com/shell-programming-scripting/154111-mass-renaming-files-complex-filenames.html" in {
    val c = FlashFill()
    val p1 = c.add(List("[Various-PC]_Some_Name_178_HD_[e2813be1].mp4"), "Ep 178.mp4")
    val p2 = c.add(List("[TTB]_Some_Name_-_496_Vost_SD_(720x400_XviD_MP3).avi"), "Ep 496.avi")
    renderer(c)
    c.solve(List("Goffytofansub_Some name 483_HD.avi"))(0) should equal ("Ep 483.avi")
  }
  
  "3. Renaming  multiple Unix files (http://stackoverflow.com/questions/1086502/rename-multiple-files-in-unix)" in {
    val c = FlashFill()
    c.add(List("fghfilea"), "jklfilea")
    c.add(List("fghfileb"), "jklfileb")
    c.add(List("fghfile"), "jklfile")
    renderer(c)
    c.solve(List("fghfileeg"))(0) should equal ("jklfileeg")
  }

  "4. Renaming files in subdirectories http://stackoverflow.com/questions/245840/rename-files-in-sub-directories  " in {
    val c = FlashFill()
    c.add(List("*.html"), "*.htm")
    c.add(List("myfile.html"), "myfile.htm")
    renderer(c)
    c.solve(List("otherf.html"))(0) should equal ("otherf.htm")
  }

  "5. Renaming files in GIT http://stackoverflow.com/questions/6269431/rename-a-number-of-files-with-git  " in {
    val c = FlashFill()
    c.add(List("__a.txt"), "_a.txt")
    c.add(List("__b.txt"), "_b.txt")
    renderer(c)
    c.solve(List("__c.txt"))(0) should equal ("_c.txt")
  }
  
  "6. Rename multiple files shell http://stackoverflow.com/questions/6911301/rename-multiple-files-shell  " in {
    val c = FlashFill()
    c.add("linux_file1.mp4 ->  file1.mp4")
    renderer(c)
    c.solve(List("linux_file2.mp4"))(0) should equal ("file2.mp4")
  }

  "7. Changing mulitple files dropping chars http://www.unix.com/unix-dummies-questions-answers/186591-rename-multiple-files-changing-prefix-extension-dropping-characters.html    " in {
    val c = FlashFill()
    c.add("000012ABCDEFGHIJ.XXX.YYY.ZZZ -> newa012.abc ")
    c.add("000078KLMNO.XXX.YYY.ZZZ          -> newa078.abc ")
    renderer(c)
    c.solve(List("000099PQ.XXX.YYY.ZZZ"))(0) should equal ("newa099.abc")
  }
  
  "8. Rename file directory http://www.unix.com/unix-dummies-questions-answers/177641-bash-script-rename-files-directory.html" in {
    val c = FlashFill()
  c.add("""apple.doc | John  | 23Feb2012  | johnFeb23apple.doc""", 3)
  c.add("""orange.doc  | Harry | 11Apr2011 | harryApr11orange.doc""", 3)   
    renderer(c)
    c.solve("""dfe.doc  | Lisa | 14Dec1990""") should equal ("lisaDec14dfe.doc")
    c.solve("""dfeasd.doc  | Paul | 24May1995""") should equal ("paulMay24dfeasd.doc")
  }
  
  "9. Rename all html files http://www.unix.com/unix-dummies-questions-answers/149885-bash-script-rename-all-files-within-folder.html" in {
    val c = FlashFill()
  c.add("""abc.mp3 | file1.avi 
xyz.avi | file2.avi 
pqr.mp4 | file3.avi""", 1)
    renderer(c)
    c.solve("aqw.mkv") should equal ("file4.avi")
    c.solve("awq.mkv") should equal ("file5.avi")
  }
  
  "10. Rename all JPG to jpg http://www.unix.com/shell-programming-scripting/135178-rename-all-jpg-files-jpg-under-all-subfolders.html" in {
    val c = FlashFill()
  c.add("""abc.JPG | abc.jpg""", 1)
    renderer(c)
    c.solve("xsers.JPG") should equal ("xsers.jpg")
    c.solve("awes.JPG") should equal ("awes.jpg")
  }
  
  "11. Rename all txt to html http://www.unix.com/shell-programming-scripting/80154-rename-file-extension.html" in {
    val c = FlashFill()
    c.add("""file1.txt -> file1.html """)
    c.add("""file2.txt->  file2.html """)
    renderer(c)
    c.solve("file10.txt") should equal ("file10.html")
  }
  
  "12. Rename all files using different names http://www.unix.com/linux/139049-rename-files-using-loop-different-name.html" in {
    val c = FlashFill()
    c.add("""file1.dat ->  file1_201001.dat """)
    c.add("""file2.dat -> file2_201002.dat""")
    renderer(c)
    c.solve("file5.dat") should equal ("file5_201005.dat")
  }
  
  "13. Rename with counter http://www.unix.com/shell-programming-scripting/166181-rename-files-sub-directories-sequential-numbers.html   " in {
    val c = FlashFill()
    c.add("""asd.dat | 1.dat.txt  
sdfrds.dat  | 2.dat.txt  
asdx.dat  | 3.dat.txt""", 1)
    renderer(c)
    c.solve("qwesd.dat") should equal ("4.dat.txt")
  }
  
  "14. Append timestamp to name http://www.unix.com/shell-programming-scripting/172750-how-rename-file-using-shellscript.html" in {
    val c = FlashFill()
    c.add("""abc.log  | 2011120706:54:59  | abc.2011120706:54:59.log""", 2)
    renderer(c)
    c.solve("""ppp.log  | 2011120703:54:59""") should equal ("ppp.2011120703:54:59.log")
    c.solve("""shd.log  | 2011100706:51:59""") should equal ("shd.2011100706:51:59.log")
    
  }
  "15. Rename multiple jpg files http://stackoverflow.com/questions/11901555/rename-multiple-jpg-files" in {
    val c = FlashFill()
    c.add("""0001webcamimage.jpg ->  0001.jpg""")
    c.add("""0002webcamimage.jpg ->  0002.jpg""")
    renderer(c)
    c.solve("""0003webcamimage.jpg""") should equal ("0003.jpg")
  }
  "16. Rename directory structure http://www.unix.com/shell-programming-scripting/172964-complex-renaming-then-moving-files.html " in {
    val c = FlashFill()
    c.add("""out.pdb | Experiment1  | out_Experiment1.pdb""", 2)
    renderer(c)
    c.solve("""out.pdb | Experiment2""") should equal ("out_Experiment2.pdb")
  }
  "17. Rename music files http://kangry.com/topics/viewcomment.php?index=9075" in {
    val c = FlashFill()
    c.add("""music01helpless.mp3 | music01.mp3""", 1)
    renderer(c)
    c.solve("""music02hopeless.mp3""") should equal ("music02.mp3")
    c.solve("""music03merciless.mp3""") should equal ("music03.mp3")
  }
  "18. Rename images by less 400  http://unix.stackexchange.com/questions/34014/bulk-rename-files-with-numbering" in {
    val c = FlashFill()
    c.add("""Image401.jpg -> Image001.jpg""")
    renderer(c)
    c.solve("""Image402.jpg""") should equal ("Image002.jpg")
    c.solve("""Image403.jpg""") should equal ("Image003.jpg")
  }
}