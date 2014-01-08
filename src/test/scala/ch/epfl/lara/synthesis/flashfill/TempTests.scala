package ch.epfl.lara.synthesis.flashfill

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TempTests  extends FlatSpec with ShouldMatchers {
  import ch.epfl.lara.synthesis.flashfill._
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import FlashFill._
  import Implicits._
  import Evaluator._
  def renderer(c: FlashFillSolver): Unit = {
    println(Printer(c.solve().get))
  }
  
  "Your test" should "work faster" in {
    val c = FlashFill()
    c.add("""file1.dat ->  file1_201001.dat """)
    c.add("""file2.dat -> file2_201002.dat""")
    renderer(c)
    c.solve("file5.dat") should equal ("file5_201005.dat")
  }
}