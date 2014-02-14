package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec 

class BenchMarks extends WordSpec  with ShouldMatchers {
  import Program._
  import ProgramSet._
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
    c.add("""Image732.jpg -> Image332.jpg""")
    //c.add("""Image501.jpg -> Image101.jpg""")
    renderer(c)
    c.solve("""Image402.jpg""") should equal ("Image002.jpg")
    c.solve("""Image403.jpg""") should equal ("Image003.jpg")
  }
  "19. Rename files by -1  http://www.unix.com/shell-programming-scripting/197485-multiple-file-rename.html " in {
    val c = StringSolver()
    c.setPosition(2)
    c.add("""file02.dat -> file01.jpg""")
    c.setPosition(10)
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
  "34. There are lots of jpg files present in a directory, all having different names (and no particular name pattern). We want to move all files created on a particular date to a different subdirectory, named as the date of file creation. We need to do this based on the file's attribute. " in {
    val c = StringSolver()
    c.add("""xyz.jpg | 09-12-2010  | 09122010\xyz.jpg""",2)
    renderer(c)
    c.solve("""abcsf.jpg | 19-11-1995""") should equal ("""19111995\abcsf.jpg""")
    c.solve("""qwe.jpg | 09-12-2010""") should equal ("""09122010\qwe.jpg""")
  }
  "35. Problem: Rename and move files to different subfolders as shown below:  " in {
    val c = StringSolver()
    c.add("""imagetaiwan001.jpg  | taiwan\pic-001.jpg 
imagetokyo001.jpg | tokyo\pic-001.jpg""",1)
    renderer(c)
    c.solve("""imagetaiwan002.jpg """) should equal ("""taiwan\pic-002.jpg""")
    c.solve("""imagetokyo002.jpg""") should equal ("""tokyo\pic-002.jpg""")
    c.solve("""imagelondon001.jpg""") should equal ("""london\pic-001.jpg""")
  }
  "36. Concatenate files into a new file and place it in a different subdirectory.   " in {
    val c = StringSolver()
    val input = (for(i <- 1 to 27) yield ("report_16032012_part" + i + ".pdf")) mkString(" | ")
    
    c.add(s"""$input | 16032012\\report.pdf""",27)
    renderer(c)
    val input2 = (for(i <- 1 to 35) yield ("ppt_12052010_part" + i + ".pdf")) mkString(" | ")
    c.solve(input2) should equal ("""12052010\ppt.pdf""")
  }
  "37. We have lots of mp3 files. we need to move each file to a different subdirectory based on the year of creating of the file, which is to be extracted from the file attribute. " in {
    val c = StringSolver()
    c.add(s"""breathless.mp3  | 05 Oct 2005 | 2005\\breathless.mp3""",2)
    renderer(c)
    c.solve("until_you.mp3  | 11 Sep 2005") should equal ("2005"+"\\"+"until_you.mp3")
    c.solve("no_promises.mp3 | 10 Jan 2010") should equal ("""2010\no_promises.mp3""")
  }
  "38. Merge files, name each merged file as '<$1>_book.pdf' and put them in different subdirectories as shown below:" in {
    val c = StringSolver()
    c.add(s"""microprocessor_chapter1.pdf | microprocessor_book.pdf 
cormen_ch1.pdf  | cormen_book.pdf""", 1)
    renderer(c)
    c.solve("microprocessor_chapter2.pdf") should equal ("""microprocessor_book.pdf""")
    c.solve("asrf_chap1.pdf") should equal("""asrf_book.pdf""")
    c.solve("asr2_chap2.pdf") should equal("""asr2_book.pdf""")
    c.solve("asdff_c1.pdf") should equal("""asdff_book.pdf""")
    c.solve("cormen_ch40.pdf") should equal("""cormen_book.pdf""")
  }
  
  "39. There are lots of different doc, jpg and mp3 files present in a directory. Move and rename all files to different subdirectories based on the creation year (from file attribute) and file type" in {
    val files = List("""c:\solar_eclipse.jpg""", """c:\northpole.jpg""", """c:\polar_bear.jpg""", """c:\abc.doc""", """c:\xyz.doc""", """c:\abtd.doc""", """c:\melt-the-snow.mp3""", """c:\some_tears.mp3""", """c:\again.mp3""")
    val Some((c, c2, s)) = Service.getPartition(List((files(0), "images"),(files(1), "images"),(files(3), "documents"),(files(4), "documents"),(files(6), "songs")))
    renderer(c)
    c2.foreach(renderer(_))
    (files map (c.solve(_)) map s) should equal (List.fill(3)("images") ++ List.fill(3)("documents") ++ List.fill(3)("songs"))
  }
  
  "40. Move and rename movies to different directories as shown below (based on video quality" in {
    val c = StringSolver()
    c.add("""Social_Network_BluRay_2009.avi    ->  BluRay\Social_Network_2009.avi """)
    c.add("""LetMeSee_DVDRip_1995.avi -> DVDRip\LetMeSee_1995.avi """)
    c.add("""Ice-Age_DVDScr_2011.avi -> DVDScr\Ice-Age_2011.avi""")
    renderer(c)
    c.solve("""PursuitOfHappiness_BluRay_2001.avi""") should equal ("""BluRay\PursuitOfHappiness_2001.avi""")
    c.solve("""PursuitOfHappiness_DVDRip_2001.avi""") should equal ("""DVDRip\PursuitOfHappiness_2001.avi""")
    c.solve("""PursuitOfHappiness_DVDScr_2001.avi""") should equal ("""DVDScr\PursuitOfHappiness_2001.avi""")
  }
  
  "41. Move files into different subdirectories based on the month of file creation, as present in timestamp in the filename" in {
    val c = StringSolver()
    c.add("""16515066 Sep 18 00:57 test1.txt | sep_bkp_files\test1.txt 
60864 Aug 18 04:34 hello.csv  | aug_bkp_files\hello.csv 
22824 Sep 18 01:30 sample2.txt  | sep_bkp_files\sample2.txt""", 1)
    renderer(c)
    c.solve("22824 Mar 89 21:30 sam.txt") should equal ("""mar_bkp_files\sam.txt""")
    c.solve("12824 Sep 19 21:10 xyz.csv") should equal ("""sep_bkp_files\xyz.csv""")
  }
  
  "42.We have a lot of .txt files present in a directory. For each .txt file do the following" in {
    val c = StringSolver()
    c.add("""file1.txt | other.txt | more.txt | command file1.txt;lpr file1.txt;command other.txt;lpr other.txt...""", 3)
    renderer(c)
    c.solve("""file1.txt | other.txt | more.txt""") should equal ("""command file1.txt;lpr file1.txt;command other.txt;lpr other.txt;command more.txt;lpr more.txt""")
  }
  
  "43. We have lots of files of the form [Date]-doc-[iter].pdf. For each file, split by date, merge and print the result" in {
    val files = for(i <- (1986 to 1995).toList; j <- 1 to 9) yield s"date$i-doc-$j.pdf"
    val c = StringSolver()
    c.add("""date1986-doc-1.pdf -> date1986/doc-1.pdf""")
    c.add("""date1987-doc-5.pdf -> date1987/doc-5.pdf""")
    renderer(c)
    c.solve("""date1991-doc-3.pdf""") should equal ("""date1991/doc-3.pdf""")
    val files_after = for(i <- (1986 to 1995).toList) yield { s"date$i" :: (for(j <- (1 to 9).toList) yield s"doc-$j.pdf") }
    val m = StringSolver()
    m.add(files_after.head, "convert date1986/doc-1.pdf date1986/doc-2.pdf... date1986-doc.pdf")
    renderer(m)
    m.solve(files_after.tail.head)(0) should equal("convert date1987/doc-1.pdf date1987/doc-2.pdf date1987/doc-3.pdf date1987/doc-4.pdf date1987/doc-5.pdf date1987/doc-6.pdf date1987/doc-7.pdf date1987/doc-8.pdf date1987/doc-9.pdf date1987-doc.pdf")
    // ...
    val final_pdfs = for(i <- (1986 to 1995).toList) yield s"""date${i}-doc.pdf"""
    val n = StringSolver()
    n.add(List(final_pdfs.head), "lpr " + final_pdfs.head)
    renderer(n)
    n.solve(final_pdfs.tail.head) should equal ("lpr " + final_pdfs.tail.head)
  }
  
  "44. There is a folder containing header.txt file, and lots of files of the form test01.txt, test02.txt,... " in {
    val files = "header.txt" :: (for(j <- (1 to 9).toList) yield s"test$j.txt")
    val Some((c, s)) = Service.getFilter(List(("header.txt", false), ("test1.txt", true), ("test2.txt", true)))
    s should equal ("test")
    c.solve("header.txt") should not equal ("test")
    c.solve("test3.txt") should equal ("test")
    
    val filtered = files.tail
    val mapping = filtered map { case file => s"cat header.txt $file > $file; lpr $file"}
    val m = StringSolver()
    m.add(List(files.head), mapping.head)
    renderer(m)
    for((file, map) <- (files.tail zip mapping.tail)) {
      m.solve(file) should equal (map)
    }
  }
  
  "45. There is a folder containing lots of jpg and pdf files. Delete pdf, Convert jpg to pdf, Print all pdf files." in {
    val files = (for(j <- (1 to 9).toList) yield (s"test$j" + (if(j % 2 == 0) ".jpg" else ".pdf")))
    val Some((c, s)) = Service.getFilter(List(("test1.jpg", false), ("test2.pdf", true), ("test4.pdf", true)))
    s should equal (".pdf")
    val pdfs = files filter (c.solve(_) == ".pdf")
    val n = StringSolver()
    // Delete pdfs
    n.add(pdfs, "rm " + pdfs.head + " " + pdfs.tail.head + "...")
    renderer(n)
    n.solve(pdfs)(0) should equal ("rm " + pdfs.mkString(" "))
    
    val m = StringSolver()
    m.add("test1.jpg -> convert test1.jpg test1.pdf;lpr test4.pdf")
    renderer(m)
    m.solve("test4.jpg") should equal ("convert test4.jpg test4.pdf;lpr test4.pdf")
  }
}