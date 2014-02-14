package ch.epfl.lara.synthesis.stringsolver


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec 

class WeightsTrain extends WordSpec  with ShouldMatchers {
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
  
  //val specifications = Number(SString())
  
}