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
    c.setTimeout(3)
    c.setVerbose(true)
    c.add("""mathAnalyse1.log ->  math_analyse_1.txt""")
    //c.add("""mathAnalyse2.log ->  math_analyse_2.txt""")
    renderer(c)
    c.solve("mathAnalyse3.log") should equal ("math_analyse_3.txt")
  }
}