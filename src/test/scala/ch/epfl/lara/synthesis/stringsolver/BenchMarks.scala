package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec 

class BenchMarks extends WordSpec  with ShouldMatchers {
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
  
  "1. Renaming files with counters http://stackoverflow.com/questions/4631123/complex-file-rename" in {
    val c = StringSolver()
    c.add(List("AB1234.gif"), "AB-0111-1.gif")
    c.add(List("B3245.gif"), "B-0111-2.gif")
    c.add(List("AB2541.jpg"), "AB-0111-3.jpg")
    c.add(List("AB11422.gif"), "AB-0111-4.gif")
    renderer(c)
    c.solve(List("AB5973.jpg"))(0) should equal ("AB-0111-5.jpg")
  }

  "2. Renaming movie files http://www.unix.com/shell-programming-scripting/154111-mass-renaming-files-complex-filenames.html" in {
    val c = StringSolver()
    val p1 = c.add(List("[Various-PC]_Some_Name_178_HD_[e2813be1].mp4"), "Ep 178.mp4")
    val p2 = c.add(List("[TTB]_Some_Name_-_496_Vost_SD_(720x400_XviD_MP3).avi"), "Ep 496.avi")
    println("Size of first:" + p1.sizePrograms)
    println("Size of second:" + p2.sizePrograms)
    println("Size of intersction:" + intersect(p1, p2).sizePrograms)
    //val res = intersect(p1, p2)
    renderer(c)
    c.solve(List("Goffytofansub_Some name 483_HD.avi"))(0) should equal ("Ep 483.avi")
  }
  
  "3. Renaming  multiple Unix files (http://stackoverflow.com/questions/1086502/rename-multiple-files-in-unix)" in {
    val c = StringSolver()
    c.add(List("fghfilea"), "jklfilea")
    c.add(List("fghfileb"), "jklfileb")
    c.add(List("fghfile"), "jklfile")
    renderer(c)
    c.solve(List("fghfileeg"))(0) should equal ("jklfileeg")
  }

  "4. Renaming files in subdirectories http://stackoverflow.com/questions/245840/rename-files-in-sub-directories  " in {
    val c = StringSolver()
    c.add(List("*.html"), "*.htm")
    c.add(List("myfile.html"), "myfile.htm")
    renderer(c)
    c.solve(List("otherf.html"))(0) should equal ("otherf.htm")
  }

  "5. Renaming files in GIT http://stackoverflow.com/questions/6269431/rename-a-number-of-files-with-git  " in {
    val c = StringSolver()
    c.add(List("__a.txt"), "_a.txt")
    c.add(List("__b.txt"), "_b.txt")
    renderer(c)
    c.solve(List("__c.txt"))(0) should equal ("_c.txt")
  }
  
  "6. Rename multiple files shell http://stackoverflow.com/questions/6911301/rename-multiple-files-shell  " in {
    val c = StringSolver()
    c.add("linux_file1.mp4 ->  file1.mp4")
    renderer(c)
    c.solve(List("linux_file2.mp4"))(0) should equal ("file2.mp4")
  }

  "7. Changing mulitple files dropping chars http://www.unix.com/unix-dummies-questions-answers/186591-rename-multiple-files-changing-prefix-extension-dropping-characters.html    " in {
    val c = StringSolver()
    c.add("000012ABCDEFGHIJ.XXX.YYY.ZZZ -> newa012.abc ")
    c.add("000078KLMNO.XXX.YYY.ZZZ          -> newa078.abc ")
    renderer(c)
    c.solve(List("000099PQ.XXX.YYY.ZZZ"))(0) should equal ("newa099.abc")
  }
  
  "8. Rename file directory http://www.unix.com/unix-dummies-questions-answers/177641-bash-script-rename-files-directory.html" in {
    val c = StringSolver()
  c.add("""apple.doc | John  | 23Feb2012  | johnFeb23apple.doc""", 3)
  c.add("""orange.doc  | Harry | 11Apr2011 | harryApr11orange.doc""", 3)   
    renderer(c)
    c.solve("""dfe.doc  | Lisa | 14Dec1990""") should equal ("lisaDec14dfe.doc")
    c.solve("""dfeasd.doc  | Paul | 24May1995""") should equal ("paulMay24dfeasd.doc")
  }
  
  "9. Rename all html files http://www.unix.com/unix-dummies-questions-answers/149885-bash-script-rename-all-files-within-folder.html" in {
    val c = StringSolver()
  c.add("""abc.mp3 | file1.avi 
xyz.avi | file2.avi 
pqr.mp4 | file3.avi""", 1)
    renderer(c)
    c.solve("aqw.mkv") should equal ("file4.avi")
    c.solve("awq.mkv") should equal ("file5.avi")
  }
  
  "10. Rename all JPG to jpg http://www.unix.com/shell-programming-scripting/135178-rename-all-jpg-files-jpg-under-all-subfolders.html" in {
    val c = StringSolver()
  c.add("""abc.JPG | abc.jpg""", 1)
    renderer(c)
    c.solve("xsers.JPG") should equal ("xsers.jpg")
    c.solve("awes.JPG") should equal ("awes.jpg")
  }
  
  "11. Rename all txt to html http://www.unix.com/shell-programming-scripting/80154-rename-file-extension.html" in {
    val c = StringSolver()
    c.add("""file1.txt -> file1.html """)
    c.add("""file2.txt->  file2.html """)
    renderer(c)
    c.solve("file10.txt") should equal ("file10.html")
  }
  
  "12. Rename all files using different names http://www.unix.com/linux/139049-rename-files-using-loop-different-name.html" in {
    val c = StringSolver()
    c.add("""file1.dat ->  file1_201001.dat """)
    c.add("""file2.dat -> file2_201002.dat""")
    renderer(c)
    c.solve("file5.dat") should equal ("file5_201005.dat")
  }
  
  "13. Rename with counter http://www.unix.com/shell-programming-scripting/166181-rename-files-sub-directories-sequential-numbers.html   " in {
    val c = StringSolver()
    c.add("""asd.dat | 1.dat.txt  
sdfrds.dat  | 2.dat.txt  
asdx.dat  | 3.dat.txt""", 1)
    renderer(c)
    c.solve("qwesd.dat") should equal ("4.dat.txt")
  }
  
  "14. Append timestamp to name http://www.unix.com/shell-programming-scripting/172750-how-rename-file-using-shellscript.html" in {
    val c = StringSolver()
    c.add("""abc.log  | 2011120706:54:59  | abc.2011120706:54:59.log""", 2)
    renderer(c)
    c.solve("""ppp.log  | 2011120703:54:59""") should equal ("ppp.2011120703:54:59.log")
    c.solve("""shd.log  | 2011100706:51:59""") should equal ("shd.2011100706:51:59.log")
    
  }
  "15. Rename multiple jpg files http://stackoverflow.com/questions/11901555/rename-multiple-jpg-files" in {
    val c = StringSolver()
    c.add("""0001webcamimage.jpg ->  0001.jpg""")
    c.add("""0002webcamimage.jpg ->  0002.jpg""")
    renderer(c)
    c.solve("""0003webcamimage.jpg""") should equal ("0003.jpg")
  }
  "16. Rename directory structure http://www.unix.com/shell-programming-scripting/172964-complex-renaming-then-moving-files.html " in {
    val c = StringSolver()
    c.add("""out.pdb | Experiment1  | out_Experiment1.pdb""", 2)
    renderer(c)
    c.solve("""out.pdb | Experiment2""") should equal ("out_Experiment2.pdb")
  }
  "17. Rename music files http://kangry.com/topics/viewcomment.php?index=9075" in {
    val c = StringSolver()
    c.add("""music01helpless.mp3 | music01.mp3""", 1)
    renderer(c)
    c.solve("""music02hopeless.mp3""") should equal ("music02.mp3")
    c.solve("""music03merciless.mp3""") should equal ("music03.mp3")
  }
  "18. Rename images by less 400  http://unix.stackexchange.com/questions/34014/bulk-rename-files-with-numbering" in {
    val c = StringSolver()
    c.add("""Image401.jpg -> Image001.jpg""")
    c.add("""Image500.jpg -> Image100.jpg""")
    //c.add("""Image501.jpg -> Image101.jpg""")
    renderer(c)
    c.solve("""Image402.jpg""") should equal ("Image002.jpg")
    c.solve("""Image403.jpg""") should equal ("Image003.jpg")
  }
  "19. Rename files by -1  http://www.unix.com/shell-programming-scripting/197485-multiple-file-rename.html " in {
    val c = StringSolver()
    c.add("""file02.dat -> file01.jpg""")
    c.add("""file10.jpg -> file09.jpg""")
    renderer(c)
    c.solve("""file14.jpg""") should equal ("file13.jpg")
  }
  "20. Add extension to filename http://www.unix.com/shell-programming-scripting/176262-rename-file-add-extensions.html" in {
    val c = StringSolver()
    c.add("""myfileaa -> myfileaa.txt""")
    renderer(c)
    c.solve("""myfileab""") should equal ("myfileab.txt")
    c.solve("""myfileac""") should equal ("myfileac.txt")
    c.solve("""myfilead""") should equal ("myfilead.txt")
  }
  "21.Print all text files of an archive http://www.unix.com/shell-programming-scripting/32584-print-all-files-directory.html" in {
    val c = StringSolver()
    c.add("""abc.txt -> lpr abc.txt; rm abc.txt""")
    renderer(c)
    c.solve("""asd.txt""") should equal ("lpr asd.txt; rm asd.txt")
    c.solve("""qwe.txt""") should equal ("lpr qwe.txt; rm qwe.txt")
  }
  "22.Move all files starting with - http://www.cyberciti.biz/faq/linuxunix-move-file-starting-with-a-dash/ " in {
    val c = StringSolver()
    c.add("""-abc.txt  | mv --abc.txt myfolder""", 1)
    c.add("""abc.txt  | """, 1)
    renderer(c)
    c.solve("""-xyz.wmv""") should equal ("mv --xyz.txt myfolder")
    c.solve("""xyz.wmv""") should equal ("")
  }
  "23.Print all doc files in a folder without opening each one of them.  http://forums.techguy.org/business-applications/485943-printing-multiple-files-folder.html " in {
    val c = StringSolver()
    c.add("""myfile.doc | thefile.doc | thirddoc | lpr myfile.doc; lpr thefile.doc;...""", 3)
    renderer(c)
    c.solve("""a.doc | b.doc | c.doc""") should equal ("lpr a.doc; lpr b.doc; lpr c.doc;")
  }
  "24.Convert jpg files in the directory to pdf files. For example convert, http://unix.stackexchange.com/questions/29869/converting-multiple-image-files-from-jpeg-to-pdf-format" in {
    val c = StringSolver()
    c.add("""myimage.jpg | convert myimage.jpg myimage.pdf""", 1)
    renderer(c)
    c.solve("""nicepicture.gif""") should equal ("convert nicepicture.gif nicepicture.pdf")
  }
  "25. Rename file with datetime appended to filename http://social.msdn.microsoft.com/forums/en-US/sqlintegrationservices/thread/1f5cc7b3-43c8-4d18-9c3b-445e76add1eb/" in {
    val c = StringSolver()
    //c.setVerbose(true)
    //c.setTimeout(1500)
    c.add("""betty.rpt | 09.24.08  | 15:30 |  betty0924081530.rpt""", 3)
    renderer(c)
    c.solve("""betty.rpt | 09.24.08  | 15:30""") should equal ("""betty0924081530.rpt""")
    c.solve("""abc.txt   | 11.04.12  | 09:11""")  should equal ("abc1104120911.txt")
    c.solve("""image.jpg | 12.09.11  | 11:50""")  should equal ("image1209111150.jpg")
  }
  "26. Rename files of type e_1.dat, e_5.dat, e_8.dat, etc. to 1.dat, 2.dat, 3.dat, http://www.unix.com/unix-dummies-questions-answers/127741-moving-files-out-multiple-directories-renaming-them-numerical-order.html" in {
    val c = StringSolver()
    c.add("""e_1.dat | 1.dat 
e_5.dat | 2.dat 
e_8.dat | 3.dat""", 1)
    renderer(c)
    c.solve("""e_9.dat""") should equal ("4.dat")
    c.solve("""e_12.dat""") should equal ("5.dat")
  }
  "27. Rename files http://www.unix.com/shell-programming-scripting/94164-mv-command-rename-multiple-files-retain-some-portion-original-file-nam.html" in {
    val c = StringSolver()
    c.add("""001file1a.txt  | 11.12.2012  | 001renamedfile1a11122012.txt""", 2)
    renderer(c)
    c.solve("""011file2.txt  | 11.12.2012""") should equal ("011renamedfile211122012.txt")
    c.solve("""101file1a.txt  | 11.12.2012""") should equal ("101renamedfile1a11122012.txt")
  }
  "28. Rename pictures http://www.unix.com/shell-programming-scripting/38510-moving-renaming-multiple-files.html" in {
    val c = StringSolver()
    c.add("""pic01.bmp | pic0001.bmp  """, 1)
    c.add("""pic142.bmp | pic0142.bmp  """, 1)
    c.add("""pic2340.bmp | pic2340.bmp  """, 1)
    renderer(c)
    c.solve("""pic02.bmp""") should equal ("pic0002.bmp")
    c.solve("""pic871.bmp""") should equal ("pic0871.bmp")
  }
  "29. Insert the contents of one text file into all the files found http://www.unix.com/shell-programming-scripting/42272-find-append-move-rename-multiple-files.html " in {
    val c = StringSolver()
    c.add("""abc.log | abc.101 """, 1)
    renderer(c)
    c.solve("""test.log""") should equal ("""test.101""")
    c.solve("""ghi.log""") should equal ("""ghi.101""")
  }
  "30. Rename files and moving files http://www.unix.com/unix-dummies-questions-answers/86-rename-multiple-files.html " in {
    val c = StringSolver()
    c.add("""arch.PROD.1_1 | /u01/TEST/arch.TEST.1_1 """, 1)
    renderer(c)
    c.solve("""arch.PROD.1_2""") should equal ("""/u01/TEST/arch.TEST.1_2""")
    c.solve("""arch.PROD.1_3""") should equal ("""/u01/TEST/arch.TEST.1_3""")
  }
  "31. Renaming files and moving them to different subdirectories based on the location in the file name " in {
    val c = StringSolver()
    c.add("""pic_delhi_20121015_001.jpg  | dir\delhi\20121015-001.jpg""", 1)
    c.add("""pic_mumbai_20121015_001.jpg | dir\mumbai\20121015-001.jpg""",1)
    renderer(c)
    c.solve("""pic_delhi_20121015_002.jpg""") should equal ("""dir\delhi\20121015-002.jpg""")
    c.solve("""pic_delhi_20111013_001.jpg""") should equal ("""dir\delhi\20111013-001.jpg""")
  }
  "32. Renaming files and moving them to different subdirectories based on date in the filename.  " in {
    val c = StringSolver()
    c.add("""11022011-001.jpg  | dir\11Feb2011\001.jpg 
11022011-002.jpg  | dir\11Feb2011\002.jpg 
21092007-001.jpg  | dir\21Sep2007\001.jpg""",1)
    renderer(c)
    c.solve("""21092007-002.jpg""") should equal ("""dir\21Sep2007\002.jpg""")
  }
  "33. Renaming movie files and moving them to different subdirectories based on the year of the movie present in the file name." in {
    val c = StringSolver()
    c.add("""TheDarkNightRises[2012]BluRay.wmv | 2012\TheDarkNightRises.wmv 
Mirror[2005]HD.wmv | 2005\Mirror.wmv""",1)
    renderer(c)
    c.solve("""Batman[2001]BluRay.wmv""") should equal ("""2001\Batman.wmv""")
  }
}