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
    c.setTimeout(20)
    c.setVerbose(true)
    c.add("""Along Came Ruth (1924) | After Midnight (1927) | Our Dancing Daughters (1928, plus sequels) |  touch 'Along Came Ruth (1924).avi' 'After Midnight (1927).avi'...""", 3)
    //c.add("""mathAnalyse2.log ->  math_analyse_2.txt""")
    renderer(c)
  }
}