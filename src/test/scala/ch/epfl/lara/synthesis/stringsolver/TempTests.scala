package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TempTests  extends FlatSpec with ShouldMatchers {
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import StringSolver._
  import Implicits._
  import Evaluator._
  
  def renderer(c: StringSolver): Unit = {
    println(Printer(c.solve().get))
  }
  
  "Your test" should "work faster" in {
    val c = StringSolver()
    c.setTimeout(5)
    //c.setExtraTimeToComputeLoops(1000)
    c.setVerbose(true)
    c.add("""01 01 ab 01 -> 01-01-ab-01""")
    c.add("""01 01 08 cd -> 01-01-08-cd""")
    renderer(c)
    c.solve("""02 03 ab 04""") should equal("02-03-ab-04")
    //c.add("""mathAnalyse2.log ->  math_analyse_2.txt""")
    renderer(c) // Should produce a loop.
  }
}