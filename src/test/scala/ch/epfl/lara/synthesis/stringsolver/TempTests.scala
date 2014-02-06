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
    c.setLoopLevel(0)
    c.add(List("birthday",".txt"), "birthday01.txt")
    renderer(c)
    c.add(List("log",".txt"), "log02.txt")
    renderer(c)
    c.solve("truc | .avi") should equal ("""truc03.avi""")
  }
}