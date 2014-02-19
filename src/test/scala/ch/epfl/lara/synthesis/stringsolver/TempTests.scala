package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TempTests  extends FlatSpec with ShouldMatchers {
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
    val c = StringSolver()
    c.setVerbose(true)
    c.setTimeout(10)
    c.setLoopLevel(1)
    c.add(List("no corresponding program to invert the words"), "words the invert to program corresponding no")
    c.add(List("what if there are two"), "two are there if what")
    renderer(c)
    c.solve("the test should succeed") should equal ("""succeed should test the""")
  }
}