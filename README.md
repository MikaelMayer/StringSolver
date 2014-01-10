Flash-fill
==========

Scala version of FlashFill for Excel 2013 by Gulwani et Al. See http://rise4fun.com/QuickCode/dates

Build using sbt 0.13 and scala 2.10.3.

Key features:

- Supports incremental numbering.
- "..." continues an expression if there is a loop.

Sample usage:

```Scala
import ch.epfl.lara.synthesis.flashfill._

object Test {
   val c = FlashFill()
   c.add("file499.pdf -> 01file.pdf")
   c.add("report_761.pdf -> 02report.pdf")
   c.add("credits##.pdf -> 03credits.pdf")
   
   /* Prints:
	* a 2-digit number incrementing starting at 1 continuing the numbers
	* of previous first output until [the end of the first number]
	* + the first input until [the end of the first lowercase word]
	* + the first input starting at [the first '.']
	*/ 
   println(Printer(c.solve().get))
   
   /* Prints:
	* 04input.pdf
	**/
   println(c.solve("input%^$.pdf"))
}

object Test2 {
   val c = FlashFill()
   c.add(List("Alg1.txt"), "convert Alg1.txt Alg001.pdf")
   
   /* Prints:
	* the constant string 'convert ' + the first input
	  + the constant string ' ' + the first input until [the end of the first word]
	  + a 3-digit number incrementing starting at 1 continuing the first number in previous output
	  + the constant string '.pdf'
	*/ 
   println(Printer(c.solve().get))
   
   /* Prints:
	* "convert Math2.txt Math002.pdf"
	*/ 
   println(c.solve("Math2.txt"))

   
   val c2 = FlashFill()
   c2.add(List("Alg001.pdf", "Alg002.pdf", "Alg003.pdf"), "convert Alg001.pdf Alg002.pdf... AlgBook.pdf")
   
   /* Prints:
	* the constant string 'convert ' + concatenates all inputs separated by ' '
	* + the constant string ' ' + the first input until [the end of the first word]
	* + the constant string 'Book' + the first input starting at [the last non-number]
	*/
   println(Printer(c2.solve().get))
   
   /* Prints:
	* convert Math1.pdf Math2.pdf Math3.pdf Math4.pdf MathBook.pdf
	*/
   println(c2.solve("Math1.txt | Math2.txt | Math3.txt | Math4.txt"))
   // prints "
}
```

The other ways to add input in Flashfill are the following, given that c is a flashfill instance.

```Scala
// Exactly one input and one output
c.add("input1 -> output1")
// c.add returns a set of solution programs. You can .takeBest on it

// Three inputs and two outputs
c.add("input1 | input2 | input3 | output1 | output2", 3)

// Three inputs and two outputs
c.add(List("input1", "input2", "input3"), List("output1", "output2"))

// Three inputs and one output
c.add(List("input1", "input2", "input3"), "output1)
```

To solve and print an existing FlashFill instance, do the following:
```Scala
val c = FlashFill()
c.add(List("a","b"),List("ab","ba"))

// If one output was provided (equivalent to c.solve(0))
c.solve() match {    // The first input + the second input
  case Some(prog) => println(Printer(prog))
  case None =>
}

// Retrieve the second output program (0-index based)
c.solve(1) match {   // The second input + the first input
  case Some(prog) => println(Printer(prog)) 
  case None =>
}

// Returns "cd | dc"
c.solve("c | d")

// Returns List("cd", "dc")
c.solve(List("c", "d"))
```

