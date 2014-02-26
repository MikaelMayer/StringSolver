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
    c.setTimeout(30)
    c.setLoopLevel(1)
    val original = """<div id="content" class="content">
<h2>Publications</h2>
<h4>Main Publications</h4>
<li><a href="http://lara.epfl.ch/~gvero/gveroETAL13CompleteCompletionTypesWeights.pdf">Complete Completion using Types and Weights</a>, T. Gvero, V. Kuncak, I. Kuraj and R. Piskac, ACM SIGPLAN Conf. Programming Language Design and Implementation (PLDI 2013). pages TO APPEAR, Seattle, Washington, USA, June 2013.</li><li><a href="http://lara.epfl.ch/~kuncak/papers/GveroETAL11InteractiveSynthesisofCodeSnippets.pdf">Interactive Synthesis of Code Snippets</a>, T. Gvero, V. Kuncak and R. Piskac, Computer Aided Verification (CAV) Tool Demo 2011, pages 418-423, Snowbird, UT, USA, July 2011.</li><li><a href="http://mir.cs.illinois.edu/~marinov/publications/DanielETAL11ReAssertDemo.pdf">ReAssert: A Tool for Repairing Broken Unit Tests</a>, B. Daniel, D. Dig, T. Gvero, V. Jagannath, J. Jiaa, D. Mitchell, J. Nogiec, S.H. Tan, and D. Marinov, International Conference on Software Engineering, Demo Papers (ICSE Demo 2011), pages 1010-1012, Waikiki, Honoulu, Hawaii, USA, May 2011</li><li><a href="http://mir.cs.illinois.edu/reassert/pubs/symreassert.pdf">On Test Repair using Symbolic Execution</a>, B. Daniel, T. Gvero, and D. Marinov, International Symposium on Software Testing and Analysis (ISSTA 2010), pages 207-218, Trento, Italy, July 2010</li><li><a href="http://mir.cs.illinois.edu/~marinov/publications/GligoricETAL10UDITA.pdf">Test Generation through Programming in UDITA </a>, M. Gligoric, T. Gvero, V. Jagannath, S. Khurshid, V. Kuncak, and D. Marinov, International Conference on Software Engineering (ICSE 2010), pages 225-234, Cape Town, South Africa, May 2010. (This paper won an <a href="http://www.sigsoft.org/awards/disPapAwd.htm"> ACM SIGSOFT Distinguished Paper Award</a><a>.)</a></li><li><a href="http://mir.cs.illinois.edu/~marinov/publications/GligoricETAL09OptimizingGeneration.pdf">Optimizing Generation of Object Graphs in Java PathFinder</a>, M. Gligoric , T. Gvero, S. Lauteburg, D. Marinov, and S. Khurshid, 2nd IEEE International Conference on Software Testing, Verification and Validation (ICST 2009), Denver, CO, April 2009.</li><li><a href="http://mir.cs.illinois.edu/~marinov/publications/GveroETAL08JPFStateExtensions.pdf">State extensions for Java PathFinder
</a>, T. Gvero, M. Gligoric, S. Lauterburg, M. d'Amorim, D. Marinov, and S. Khurshid, International Conference on Software Engineering, Demo Papers
(ICSE Demo 2008), pages 863-866, Leipzig, Germany, May 2008.</li>
<h4>Technical Reports</h4>
<ul><li><a href="https://infoscience.epfl.ch/record/170040">Code Completion using Quantitative Type Inhabitation</a>, T. Gvero, V. Kuncak, and R. Piskac, EPFL, IC Technical Report EPFL-REPORT-170040. 2011.</li><li><a href="http://infoscience.epfl.ch/record/140989">On Test Generation through Programming in UDITA</a>, M. Gligoric, T. Gvero, V. Jagannath, S. Khurshid, V. Kuncak, and D. Marinov, EPFL, IC Technical Report LARA-REPORT-2009-05. 2009.</li><li><a href="http://infoscience.epfl.ch/record/128816">On Delayed Choice Execution for Falsification</a>, M. Gligoric , T. Gvero, S. Khurshid, V. Kuncak, and D. Marinov, Technical Report LARA-REPORT-2008-08, EPFL, IC, 2008.</li></ul></div>"""
    
    c.add(List(original),
"""Complete Completion using Types and Weights
T. Gvero, V. Kuncak, I. Kuraj and R. Piskac, 2013.
http://lara.epfl.ch/~gvero/gveroETAL13CompleteCompletionTypesWeights.pdf

Interactive Synthesis of Code Snippets
T. Gvero, V. Kuncak and R. Piskac, 2011.
http://lara.epfl.ch/~kuncak/papers/GveroETAL11InteractiveSynthesisofCodeSnippets.pdf
...""")
    renderer(c)
    println(c.solve(original))
  }
}