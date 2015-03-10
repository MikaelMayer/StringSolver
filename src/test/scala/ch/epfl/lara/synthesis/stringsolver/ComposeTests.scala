package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ComposeTests  extends FlatSpec with ShouldMatchers {
  import Program._
  import ProgramSet._
  import ScalaRegExp._
  import StringSolver._
  import Implicits._
  import Evaluator._
  
  def renderer(c: StringSolver): Unit = {
    c.solve() match {
      case Some(prog) =>
        println(Printer(prog))
      case None =>
        println("No corresponding program")
    }
  }
  
  "Your test" should "work faster" in {
    import ch.epfl.lara.synthesis.stringsolver._ 
    import ImperativeProgram._
    import CurrentInstance._ 
    import ProgramTypes._
    NEW
    "a.pdf b.pdf c.pdf"==>("a.pdf", "b.pdf", "...")
    val split = SPLIT
    NEW
		yes ==> ("1.a.pdf", "2.b.pdf")
		no ==> ("a.1.pdf", "b.pdf", "1.a.doc")
		val filter = FILTER
		NEW
		==> ("1.a.pdf", "2.a.pdf")
		==> ("1.b.pdf", "2.b.pdf")
		val partition = PARTITION
		NEW
		List("1.a.pdf", "2.a.pdf") ==> "convert 1.a.pdf 2.a.pdf... a.pdf"
		val reduce = REDUCE
		NEW
		("convertA", "convertB") ==> "convertA;convertB;..."
		val reduce2 = REDUCE
		
		val program = (split | filter | partition | (reduce as map) | reduce2 )
		program("a.jpg b.doc 1.a.pdf 1.b.pdf 2.a.pdf 2.b.pdf") should equal ("convert 1.a.pdf 2.a.pdf a.pdf;convert 1.b.pdf 2.b.pdf b.pdf;")
		program in Powershell to "filterpartition.ps1"
		// .\filterpartition "a.jpg b.doc 1.a.pdf 1.b.pdf 1.c.pdf 2.a.pdf 2.b.pdf 3.a.pdf"
  }
  
  "SPLIT and REDUCE as map" should "work" in {
    import ch.epfl.lara.synthesis.stringsolver._ 
    import ImperativeProgram._ 
    import CurrentInstance._ 
    import ProgramTypes._
    NEW
    "a b c" ==> ("a", "b", "...")
    NEW
    ("a", "b") ==> "ab...001"
    ("b", "c", "d") ==> "bc...002"
    ((SPLIT as map) | (REDUCE as map)) in Powershell to "script.ps1"
  }
}