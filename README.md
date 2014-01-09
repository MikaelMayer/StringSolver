Flash-fill
==========

Scala version of FlashFill for Excel 2013 by Gulwani et Al. See http://rise4fun.com/QuickCode/dates

Sample usage:

    import ch.epfl.lara.synthesis.flashfill._

    object Test {
       val c = FlashFill()
	   c.add("file499.pdf -> 01input.pdf")
	   c.add("report_761.pdf -> 02report.pdf")
	   c.add("credits##.pdf -> 03credits.pdf")
	   println(c.solve("input%^$.pdf")) // prints "04input.pdf"
	}
	
	object Test2 {
       val c = FlashFill()
	   c.add(List("Alg1.txt", "Alg2.txt", "Alg3.txt"), "convert Alg1.txt Alg001.pdf; convert Alg2.txt Alg002.pdf;...")
	   println(c.solve("Math1.txt | Math2.txt | Math3.txt | Math4.txt"))
	   // prints "convert Math1.txt Math001.pdf; convert Math2.txt Math002.pdf; convert Math3.txt Math003.pdf; convert Math4.txt Math004.pdf;"
	   
	   val c = FlashFill()
	   c.add(List("Alg001.pdf", "Alg002.pdf", "Alg003.pdf"), "convert Alg001.pdf Alg002.pdf... AlgBook.pdf")
	   println(c.solve("Math1.txt | Math2.txt | Math3.txt | Math4.txt"))
	   // prints "convert Alg001.pdf Alg002.pdf Alg003.pdf Alg004.pdf AlgBook.pdf
	}
	
	
	