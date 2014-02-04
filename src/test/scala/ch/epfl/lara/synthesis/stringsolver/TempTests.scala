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
    val f = StringSolver()
    val c = f.add("april August mar Dec -> 4 8 03 12")
    println(Printer(c.takeBest))
    f.solve("february March oct May") should equal ("2 3 10 5")
  }
}