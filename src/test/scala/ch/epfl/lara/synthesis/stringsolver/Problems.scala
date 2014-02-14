package ch.epfl.lara.synthesis.stringsolver

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


class BenchMarkTest extends FlatSpec with ShouldMatchers {
  import Program._
  import ProgramSet._
  import ScalaRegExp._
  import StringSolver._
  import Implicits._
  import Evaluator._

  "StringSolver" should "solve the book concatenation problem" in {
     val c = StringSolver()
    c.setTimeout(10)
    try {
    c.add(IndexedSeq("Algorithm1.pdf", "Algorithm2.pdf", "Algorithm3.pdf", "Algorithm4.pdf"), "convert Algorithm1.pdf Algorithm2.pdf... AlgorithmBook.pdf")
    } catch {
      case e: Throwable =>
        println(c.getStatistics())
        throw e
    }
    val program = c.solve().get
    println(Printer(program))
    program("Algorithm1.pdf", "Algorithm2.pdf", "Algorithm3.pdf", "Algorithm4.pdf") should equal ("convert Algorithm1.pdf Algorithm2.pdf Algorithm3.pdf Algorithm4.pdf AlgorithmBook.pdf")
  }
  
  it should "Find words after the first dot (2 tok necessary)" in {
     val c = StringSolver()
    c.setUseNumbers(true)
    c.setUseDots(true)
    c.setLoopLevel(0)
    c.add(IndexedSeq("Dr.Best is"), "Best")
    c.add(IndexedSeq("Mrs.Amazonia is Dr. architect"), "Amazonia")
    c.add(IndexedSeq("Who is M.Bean?"), "Bean")
    c.add(IndexedSeq("Who is Ms.Apple?"), "Apple")
     val program = c.solve().get
     println(Printer(program))
     program("Where Ms.Jellyfish !") should equal ("Jellyfish")
  }
  
  "Problem: Renaming files" should "be solved" in {
    val c = StringSolver()
    c.add(List("AB1234.gif"), "AB-0111-1.gif")
    println(Printer(c.solve().get))
    c.add(List("B3245.gif"), "B-0111-2.gif")
    println(Printer(c.solve().get))
    c.add(List("AB2541.jpg"), "AB-0111-3.jpg")
    //println(Printer(c.solve().get))
    //c.solve(List("AB11422.gif"))(0) should equal ("AB-0111-4.gif")
    c.add(List("AB11422.gif"), "AB-0111-4.gif")
    val prog0 = c.solve()
    println(Printer(c.solve().get))
    c.solve(List("AB5973.jpg"))(0) should equal ("AB-0111-5.jpg")
  }
  
  "Problem: Refractoring the code" should "work for longer inputs" in {
    val c = StringSolver()
    c.setTimeout(3)
    c.setVerbose(true)
    c.add(List(""""Prof. Kathleen S. Fisher" ==> p5 ==> "Fisher, Kay.""""), """p5("Prof. Kathleen S. Fisher") should equal ("Fisher, Kay.")""")
    c.add(List(""""Bill Gates, Sr." ==> p5 ==> "Gates, B.""""), """p5("Bill Gates, Sr.") should equal ("Gates, B.")""")
    println(Printer(c.solve().get))
    c.solve(List(""""George Ciprian Necula" ==> p5 ==> "Necula, G.""""))(0) should equal ("""p5("George Ciprian Necula") should equal ("Necula, G.")""")
  }
}
