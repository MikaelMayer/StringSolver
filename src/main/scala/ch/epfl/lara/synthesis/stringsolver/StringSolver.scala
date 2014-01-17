package ch.epfl.lara.synthesis.stringsolver

import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashMap => MMap}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.matching.Regex

/**StringSolver object
 * Used to create a StringSolver instance to solve input/output problems.
 */
object StringSolver {
  import Programs._  
  import ProgramsSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  final val debugActive = false
  
  private type PrevNumberString = String
  type Output_state = String //case class Output_state(value: String, position: Int)
  type S = String
  case class Input_state(inputs: IndexedSeq[String], prevNumberOutputs: IndexedSeq[String])
  type σ = Input_state
  type Regular_Expression = RegExp
  type W[Node] = Map[(Node, Node), Set[SAtomicExpr]]
  type Start = Int
  type End = Int
  type Index = Int
  
  //def debug(s: String) = if(debugActive) println(s)
  
  def apply(): StringSolver = new StringSolver()
  
  def apply(input: List[List[String]], output: List[String]): Option[Program] = {
    val solver = apply()
    (input zip output) foreach { case (i, o) =>
      solver.add(i, o)
    }
    solver.solve()
  }
  
  
  implicit def indexedSeqToInputState(arr: IndexedSeq[String]) = Input_state(arr, IndexedSeq[String]())
}

/**
 * Instance solving the problem iteratively
 */
class StringSolver {
  import Programs._  
  import ProgramsSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  import StringSolver._
  private var ff = new StringSolverAlgorithms()
  private var currentPrograms: IndexedSeq[STraceExpr] = null
  private var singlePrograms  = ArrayBuffer[IndexedSeq[STraceExpr]]()
  private var previousOutputs: IndexedSeq[String] = Array[String]()

  private var inputList = List[List[String]]()
  private var outputList = List[List[String]]()
  
  private var proportion_compute_vs_merge = 0.35f

  /**
   * Use dots ... to trigger manual loop research
   */
  def setUseDots(b: Boolean) = ff.useDots = b
  
  /**
   * Use numbering from previous input option
   */
  def setUseNumbers(b: Boolean) = ff.numbering = b
  
  /**
   * Loop level. 0 will not look for loops
   */
  def setLoopLevel(i: Int) = ff.DEFAULT_REC_LOOP_LEVEL = i
  
  /**
   * Timeout in seconds to add a new input/output example.
   * This is approximate. Default is 30s
   */
  def setTimeout(seconds: Int) = ff.TIMEOUT_SECONDS = seconds
  
  /**
   * If looking for loops, what could be the maximum separator length
   */
  def setMaxSeparatorLength(length: Int) = ff.MAX_SEPARATOR_LENGTH = length
  
  /**
   * If only interesting positions (aka word, special chars and digit separators)
   * are considered when looking for loops
   */
  def setOnlyInterestingPositions(b: Boolean) = ff.onlyInterestingPositions = b
  
  /**
   * Outputs programs steps. Useful for debugging an other.
   */
  def setVerbose(b: Boolean) = ff.verbose = b
  
  //def setPreferCounter(b: Boolean)
  
  /**
   * @returns an indexed seq with numbers derived from the previous output.
   */
  private def previousNumbers(str: IndexedSeq[String] = previousOutputs): IndexedSeq[String] = {
    if(ff.numbering) {
      str map { _.numbers }
    } else {
      IndexedSeq()
    }
  }
  
  def getStatistics(): String = ff.statistics()
  
  def setAdvancedStats(b: Boolean) = ff.advanced_stats = b
  
  /**Adds a new inputs/outputs example.
   **/
  def add(input: Seq[String], output: Seq[String]): Seq[STraceExpr] = {
    if(!(output.exists(out => out.exists(_.isDigit)))) { // If not digit for output, we don't use numbers.
      setUseNumbers(false)
    }
    val prevNumbers = previousNumbers()
    
    inputList = inputList ++ List(input.toList)
    outputList = outputList ++ List(output.toList)
    
    val iv = input.toIndexedSeq
    val ov = output.toIndexedSeq
    
    val fetchPrograms = future {
      for(out <- ov) yield
      ff.generateStr(Input_state(iv, prevNumbers), out, ff.DEFAULT_REC_LOOP_LEVEL)
    }
    var tmp = ff.DEFAULT_REC_LOOP_LEVEL
    val newProgramSets : IndexedSeq[STraceExpr] = try {
      Await.result(fetchPrograms, (ff.TIMEOUT_SECONDS * (if(currentPrograms == null) 1f else proportion_compute_vs_merge)).seconds)
    } catch {
      case e: TimeoutException  => 
        ff.DEFAULT_REC_LOOP_LEVEL = 0 // No loops this time
        ff.timeout = true
        Await.result(fetchPrograms, (ff.TIMEOUT_SECONDS * proportion_compute_vs_merge).seconds)
      case e: Throwable => throw e
    }
    ff.DEFAULT_REC_LOOP_LEVEL = tmp
    ff.timeout = false
    if(currentPrograms == null) {
      currentPrograms = newProgramSets
    } else {
      if(ff.verbose) println("Computing intersection with previous programs...")
      val intersectionsFuture = future {
        for(i <-0 until currentPrograms.length) yield {
          //println(s"Intersecting programs $i")
          intersect(currentPrograms(i), newProgramSets(i))
        }
        //(currentPrograms zip newProgramSets) map { case (a, b) => intersect(a, b) }
      }
      val intersections = try {
        Await.result(intersectionsFuture, (ff.TIMEOUT_SECONDS * (1-proportion_compute_vs_merge)).seconds)
      } catch {
        case e: TimeoutException  => 
          if(ff.verbose) println("Intersection took too much time! Returning empty")
          currentPrograms map {_ => SEmpty}
          //throw e
      }
      currentPrograms = intersections
    }
    singlePrograms += newProgramSets
    previousOutputs = output.toIndexedSeq
    
    if(debugActive) verifyCurrentState()
    newProgramSets
  }
 
  /**Adds a new input/output example.
   * If the best program already matches the input/output example,
   * it is not recomputed.
   **/
  def add(input: Seq[String], output: String): STraceExpr = {
    val res = add(input, IndexedSeq(output))
    res(0)
  }
  
  /**
   * Adds a new input/output example
   */
  def add(inputoutput: String): STraceExpr = {
    val arrow = inputoutput.indexOf("->")
    if(arrow != -1) {
      val input = inputoutput.substring(0, arrow).trim()
      val output = inputoutput.substring(arrow+2).trim()
      add(List(input), output)
    }else throw new Exception("No separator such as | or -> found")
  }
  /**
   * Adds a new input/output example with |
   * @param inputoutput the input/output, can be multiline, separated by |
   * @param ninput number of inputs per line.
   */
  def add(inputoutput: String, ninputs: Int = 1): Seq[STraceExpr] = {
    if(inputoutput contains '\n') {
      inputoutput.split('\n').map{add(_, ninputs)}.last
    } else {
      val pipe = inputoutput.indexOf("|")
      if(pipe != -1) {
        val elems = inputoutput.split("\\|").map(_.trim()).toList
        add(elems.take(ninputs), elems.drop(ninputs))
      } else throw new Exception("No separator such as | or -> found")
    }
  }
  
  /**
   * Solved a piped example. Returns a piped example.
   * AB | CD | XY  => EF | GH
   * 
   * If multiline, solve the complete instance, e.g.
   * a | A | a1    a | A | a1
   * b | B | b1 => b | B | b1 
   * c             c | C | c1
   * d             d | D | d1
   */
  def solve(input: String): String = {
    if(input.indexOf('\n') != -1) {
      val problems = input.split('\n').map(_.split("\\|").toList.map(_.trim())).toList
      val iolength = (0 /: problems) { case (a, io) => Math.max(a, io.length)}
      val ilength  = (iolength /: problems) { case (a, io) => Math.min(a, io.length)}
      var adding_inputoutput = true
      (for(i <- problems.toList) yield {
        if(i.length == iolength) {
          if(adding_inputoutput) {
            add(i.take(ilength), i.drop(ilength))
            i.mkString(" | ")
          } else throw new Exception("All examples must be at the beginning")
        } else if(i.length == ilength) {
          adding_inputoutput = false
          i.mkString(" | ") +" | "+solve(i).mkString(" | ")
        } else throw new Exception("All examples must be at the beginning")
      }) mkString "\n"
    } else {
      val elems = input.split("\\|").map(_.trim()).toList
      solve(elems).mkString(" | ")
    }
  }
  
  /** Returns the best solutions to the problem for the whole output */
  def solveAll(): List[Option[Program]] = for(i <- (0 until currentPrograms.length).toList) yield solve(i)
  
  /** Returns the best solution to the last problem for the whole output */
  def solveLasts(): List[Option[Program]] = for(i <- (0 until currentPrograms.length).toList) yield solveLast(i)
  
  /** Returns the best solution to the problem for the whole output */
  def solve(nth: Int = 0): Option[Program] = if(currentPrograms != null) try {
    val res = Some(currentPrograms(nth).takeBest)
    if(debugActive) verifyCurrentState()
    res
  } catch {
    case _: java.lang.Error => None
    case _: Exception => None
  } else None
  
  /** Returns the best solution to the problem for the whole output */
  def solveLast(nth: Int = 0): Option[Program] = if(singlePrograms != null) try {
    val res = Some(singlePrograms(singlePrograms.length - 1)(nth).takeBest)
    if(debugActive) verifyCurrentState()
    res
  } catch {
    case _: java.lang.Error => None
    case _: Exception => None
  } else None
  
  def takeBest[T <: Program](s: ProgramSet[T]): Program = s.takeBest

  /**
   * Solves for a new instance of input.
   */
  def solve(input: Seq[String]): Seq[String] = {
    //println(s"Solving for input $input")
    val res = currentPrograms map (progSet => 
      try {
        //println(s"progSet $progSet")
        val prog = progSet.takeBest
        val r = evalProg(prog)(Input_state(input.toIndexedSeq, previousNumbers()))
        r.asString
      } catch {
        case _: java.lang.Error => ""
        case _: Exception => ""
      }
    )
    previousOutputs = res.toIndexedSeq
    res
  }
  
  /**
   * Solves using the last input/output example
   */
  def solveLast(input: Seq[String]): Seq[String] = {
    //println(s"Solving for input $input")
    val res = singlePrograms(singlePrograms.length - 1) map (progSet => 
      try {
        //println(s"progSet $progSet")
        val prog = progSet.takeBest
        val r = evalProg(prog)(Input_state(input.toIndexedSeq, previousNumbers()))
        r.asString
      } catch {
        case _: java.lang.Error => ""
        case _: Exception => ""
      }
    )
    previousOutputs = res.toIndexedSeq
    res
  }

  /**
   * Verifies the current state so that the resulting program
   * works for everybody.
   */
  private def verifyCurrentState() = {
    var previousOutputsTmp = IndexedSeq[String]()
    for((inputs, outputs) <- (inputList zip outputList);
        (output, progs) <- (outputs zip currentPrograms);
        prog = progs.takeBest
    ) {
      val pn = previousNumbers(previousOutputsTmp)
      previousOutputsTmp = outputs.toIndexedSeq
      evalProg(prog)(Input_state(inputs.toIndexedSeq, pn)) match {
        case StringValue(res) =>
          if(ff.useDots) {
            val reg = output.split("\\Q...\\E").map(Pattern.quote(_)).mkString(".*").r.anchored
            assert(reg.findFirstIn(res) != None)
          } else {
            assert(res == output)
          }
        case BottomValue => 
          assert(false)
        case _ =>
      }
    }
  }
}

class StringSolverAlgorithms {
  import Programs._  
  import ProgramsSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  import StringSolver._
  
  // Parameter: Are we using dots to describe not finished loops.
  var useDots = true
  // Adds the last output number-only as input for the next program.
  var numbering = true
  
  final val dots = "..."
 
  var TIMEOUT_SECONDS = 30
  var DEFAULT_REC_LOOP_LEVEL = 1
  var MAX_SEPARATOR_LENGTH = 1
 

  @volatile private var mTimeout = false
  def timeout = mTimeout
  def timeout_=(v: Boolean): Unit = {
    mTimeout = v
    if(v) {
      ifTimeOut.success(SEmpty)
      ifTimeOut = promise[STraceExpr]
    }
  }
  private var ifTimeOut = promise[STraceExpr]
  
  var onlyInterestingPositions = false
  
  var verbose = false

  
  /**synthesis algorithm*/
  def generateStringProgram(S: Set[(σ, S)]) = {
    var T = Set.empty[(Set[σ], STraceExpr)]
    for((σ, s) <- S)
      T = T + ((Set(σ),generateStr(σ, s, DEFAULT_REC_LOOP_LEVEL))) 
    T = generatePartition(T)
    val σp = for((σ, s) <- S) yield σ
    var B = Map[Set[σ], Bool]()
    
    for((σt, et) <- T) {
      B += σt -> generateBoolClassifier(σt, σp -- σt)
    }

    val P  = T.toList.sortBy{case (σi, ei) => sizePrograms(ei)} map
    { case (σi, ei) => (B(σi), ei) }
    
    SSwitch(P)
  }
  
  def generatePartition(s: Set[(Set[σ], STraceExpr)]): Set[(Set[σ], STraceExpr)] = ???
  
  def generateBoolClassifier(σ1: Set[σ], σ2: Set[σ]) = ???
  
  /**
   * learnd the set of all SubStr expressions
     in our language that can be used to extract a given substring
     from a given string.
   */
  implicit val cacheGenerateStr = MMap[(Input_state, Output_state, Int), STraceExpr]()
  def generateStr(σ: Input_state, s: Output_state, rec_loop_level: Int) = cached((σ, s, rec_loop_level), cacheGenerateStr) {
    //debug(s"generateStr on: $σ, $s and $rec_loop_level")
    val ñ = (0 to s.length).toSet
    val ns = 0
    val nt = s.length
    val ξ = (for(i <- 0 until s.length; j <- (i+1) to s.length) yield i->j).toSet
    if(verbose) println(s"Building constant map for $s")
    var W = Map[(Int, Int), Set[SAtomicExpr]]()
    for((i, j) <- ξ if !timeout) {
      W += (i,j) -> (Set(SConstStr(s.e(i, j-1))))
    }
    if(verbose) println("Looking for longest substrings...")
    val longestSizeFirst = (ij : (Int, Int)) => ij._1 - ij._2
    var interestingEdges1 = (for(
      i <- 0 until s.length;
      j <- (i+1) to s.length;
      σvi <- σ.inputs
      if σvi.indexOf(s.substring(i, j)) >= 0) yield (i, j)).toSet
    // Removes edges which belong to a greater substring if they are not themselves longest numbers
    val longestInterestingEdges = ((interestingEdges1.toList filterNot {
      case (i, j) =>
        val isNumber = s.substring(i, j).isNumber
        (((interestingEdges1 contains ((i, j+1))) && (isNumber implies s(j).isDigit)) ||
         ((interestingEdges1 contains ((i-1, j))) && (isNumber implies s(i-1).isDigit)))
    }))sortBy(longestSizeFirst)
     
    if(verbose) println(s"found ${longestInterestingEdges.length} longest substrings:\n"+(longestInterestingEdges map { case (i, j) => s.e(i, j-1)}))
    val remaining_edges = ξ.filter{ case (i,j ) => !longestInterestingEdges.contains((i, j)) }.toList.sortBy(longestSizeFirst)
    for((i, j) <- longestInterestingEdges.toIterable ++ remaining_edges if !timeout) {
      W += (i,j) -> (W.getOrElse((i, j), Set()) ++ (generateSubString(σ, s.e(i, j-1))))
    }
    if(timeout && verbose) println("exited loop of generateStr because timed out")
    /*val W: W[Int] = ξ ==> { case (i, j) =>
      generateSubString(σ, s.e(i, j-1)) ++ Set(SConstStr(s.e(i, j-1)))
    }*/
    val previous = SDag(ñ, ns, nt, ξ, W): STraceExpr
    val Wp =  generateLoop(σ, s, W, rec_loop_level)(previous, preferredStart=longestInterestingEdges.map(_1).toSet, preferredEnd = longestInterestingEdges.map(_2).toSet)
    SDag(ñ, ns, nt, ξ, Wp): STraceExpr
  }
  
  /**
   * Specializes a DAG by removing all positions except those between k1 and k2
   */
  def specializeDag(dag: STraceExpr, k1: Int, k2: Int, orElse: => STraceExpr): STraceExpr = dag match {
    case SDag(ñ, ns: Int, nt, ξ, a) =>
      val x = ξ.asInstanceOf[Set[(Int, Int)]]
      val nn = ñ.asInstanceOf[Set[Int]]
      val aa = a.asInstanceOf[W[Int]]
      def ok(i: Int): Boolean = k1 <= i && i <= k2
      def ok2(ij: (Int, Int)): Boolean = ok(ij._1) && ok(ij._2)
      SDag(nn.filter(ok), k1, k2, x.filter(ok2), aa.filterKeys(ok2))
    case e => orElse
  }
  /**
   * Specializes a DAG by removing all edges except the one between k1 and k2
   */
  def extractDag(dag: STraceExpr, k1: Int, k2: Int, orElse: => STraceExpr): STraceExpr = dag match {
    case SDag(ñ, ns: Int, nt, ξ, a) =>
      val x = ξ.asInstanceOf[Set[(Int, Int)]]
      val nn = ñ.asInstanceOf[Set[Int]]
      val aa = a.asInstanceOf[W[Int]]
      def ok1(i: Int): Boolean = k1 == i
      def ok2(i: Int): Boolean = i == k2
      def ok3(ij: (Int, Int)): Boolean = ok1(ij._1) && ok2(ij._2)
      SDag(Set(k1, k2), k1, k2, x.filter(ok3), aa.filterKeys(ok3))
    case e => orElse
  }
  
  /**
   * In this section, we discuss how to infer the set of all Loop constructors
      that can be used to generate some unknown part of a given
      output string s from a given input state σ. In the process, we would
      also identify the unknown part of the output string that the Loop
      constructor can generate. Procedure GenerateLoop performs this
      task effectively, and involves the following steps:
      1. Guess three positions within the output string k1, k2, and k3.
      2. Unify the set of trace expressions that can generate s[k1 : k2]
      with the set of trace expressions that can generate s[k2 : k3] to
      obtain a new set of string expressions, say ~e that uses the loop
      iterator w. The unification algorithm is explained below.
      3. Obtain the set of substrings obtained by running the string expressions
      ~e on input σ. If this set contains a singleton string that
      matches s[k1 : k3] for some k3, then we conclude that s[k1 : k3]
      can be generated by Loop(w : ~e). Otherwise ignore.
      The unification algorithm is same as the intersection algorithm
      except with the following replacement to Eq. 2 in Figure 4.
      IntersectPos(k1; k2) = (k2 - k1)w + k1 if k1 != k2
      The key idea above is to guess a set of loop bodies by unifying the
      sets of trace expressions associated with the substrings s[k1 : k2]
      and s[k2 : k3], and then test the validity of the conjectured set
      of loops. For performance reasons, we do not recursively invoke
      GenerateLoop (in the call that it makes to GenerateStr). This
      allows us to discover all single loops. Nested loops may be discovered
      by controlling the recursion depth.
   */
  var w_id = 0
  def generateLoop(σ: Input_state, s: Output_state, W: W[Int], rec_loop_level: Int)(current: STraceExpr, preferredStart: Set[Int], preferredEnd: Set[Int]): W[Int] = {
    if(rec_loop_level <= 0) return W
    if(verbose) println(s"Find loop for $σ => $s")
    //var WpLite = W // Create a copy?
    var Wp = W // Do not create a copy
    val LITE = 0
    val FULL = 1
    
    val w = Identifier(if(w_id <= 25) ('a' + w_id.toChar).toChar.toString else "w" + (w_id-25)); w_id += 1
    val positionToCheck: Int => Boolean = if(onlyInterestingPositions) {
      new (Int => Boolean) {
        val l = ScalaRegExp.convertToken(LowerTok)
        val u = ScalaRegExp.convertToken(UpperTok)
        val n = ScalaRegExp.convertToken(NumTok)
        val w = ScalaRegExp.convertToken(SpaceTok)
        val acceptable: Set[Int] = s"[^a-zA-Z0-9]|$l|$u|$n|$w".r.findAllMatchIn(s).toSet.flatMap{(m: Regex.Match) =>Set(m.start(0), m.end(0))}
        def apply(i: Int) = acceptable(i)
      }
    } else {i => true}
    
    
    def subDag(k1: Int, k2: Int, liteOrFull: Int): STraceExpr = {
      if(liteOrFull == LITE) {
        current match {
          case sd: SDag[_] =>
            extractDag(current, k1, k2, SEmpty)
          case _ =>
            SEmpty
        }
      } else {
        if(rec_loop_level == 1) {
          specializeDag(current, k1, k2, generateStr(σ, s.substring(k1, k2), rec_loop_level - 1))
        } else {
          generateStr(σ, s.substring(k1, k2), rec_loop_level - 1)
        }
      }
    }
    val preferedStartFirst: Iterable[Int] => Iterable[Int] = { (i: Iterable[Int]) =>
      i.filter(preferredStart) ++ i.filterNot(preferredStart)
    }
    val preferedEndFirst: Iterable[Int] => Iterable[Int] = { (i: Iterable[Int]) =>
      i.filter(preferredEnd) ++ i.filterNot(preferredEnd)
    }
    
    // Priority if dots found in string.
    val endingRange: Iterable[Int] = if(useDots) { s.indexOf("...") match {
        case -1 => Range(2, s.length-1) // Nothing can be done.
        case k3 => k3 :: preferedEndFirst((Range(2, s.length-1).toList.filterNot(_ == k3))).toList
      }
    } else Range(2, s.length-1)

    // Two loops versions, one with lite loops (no more than 1 expression in the loop)
    // the other allows more expressions.
    for(liteOrFull <- LITE to FULL;
        k3 <- endingRange if positionToCheck(k3);
        ksep <- preferedStartFirst(k3-1 to 1 by -1) if positionToCheck(ksep);
        e2 = subDag(ksep, k3, liteOrFull);
        k2 <- preferedEndFirst(ksep to (ksep - MAX_SEPARATOR_LENGTH) by -1) if positionToCheck(k2);
        optionSeparator = if(ksep > k2) Some(ConstStr(s.substring(k2, ksep))) else None;
        if(k2 == ksep || ProgramsSet.isCommonSeparator(optionSeparator.get.s));
        k1 <- preferedStartFirst(k2-1 to 0 by -1) if positionToCheck(k1);
        e1 = subDag(k1, k2, liteOrFull)) {
      if(timeout) return Wp
      if(verbose) println(s"Going to unify '${s.substring(k1, k2)}' and '${s.substring(ksep, k3)}' separated by '${s.substring(k2, ksep)}'")
      val (e, time) = timedScope(if(liteOrFull == LITE) {
        unify(e1, e2, w)  // If unify results only in constants
      } else {
        // If full, can take much more time per unification.
        val res = future{unify(e1, e2, w)}
        Await.result(first(res, ifTimeOut.future), 10.days) 
      })
      stats_unifications += 1
      stats_time_unifications += time
      if(sizePrograms(e) != 0) {
        val bestLoop =  e.takeBest
        val prog = Loop(w, bestLoop, optionSeparator)
        if(bestLoop.uses(w)) {
          val resulting_strings = 
            Evaluator.evalProg(prog)(σ) match {
              case StringValue(p) if p != "" => List(p)
              case _ => Nil
            }
          resulting_strings.toList match {
            case List(res) =>
              val k4 = k1 + res.length
              if(k4 <= s.length && s.substring(k1, k4) == res) {
                Wp = Wp + (((k1, k4))->(Wp((k1, k4)) ++ Set(SLoop(w, e, optionSeparator))))
                if(useDots) {
                  if(k4 < s.length && s.substring(k4, Math.min(s.length, k4+dots.length)) == dots) {
                    Wp = Wp + (((k1, k4+dots.length))->(Wp((k1, k4+dots.length)) ++ Set(SLoop(w, e, optionSeparator))))
                  }
                }
              } else if(useDots) { // If we use dots '...' to match the remaining.
                val positionNotMatch: Option[Int] = (k1 until k4) find { k => k < s.length && s(k) != res(k-k1) }
                positionNotMatch match {
                  case Some(p) if s(p) == dots(0) =>
                    if(p + dots.length <= s.length && s.substring(p, p + dots.length) == dots) {
                      Wp = Wp + (((k1, p+dots.length))->(Wp((k1, p+dots.length)) ++ Set(SLoop(w, e, optionSeparator))))
                      if(verbose) println(s"Found dotted loop: $s (returns $res)")
                    }
                  case _ =>
                }
              }
            case Nil =>
          }
        }
      }
    }
    Wp
  }
  
  /**
   * Generate all atomic expressions which can generate a string s from input states.
   */
  implicit val cacheGenerateSubstring = MMap[(Input_state, String), Set[SAtomicExpr]]()
  def generateSubString(σ: Input_state, s: String) = cached((σ, s), cacheGenerateSubstring){
    var result = Set.empty[SAtomicExpr]
    if(verbose) println(s"Going to extract $s from $σ")
    for(vi <- 0 until σ.inputs.length if !timeout) {
      val σvi =  σ.inputs(vi)
      for((k, m) <- s substringWithCaseOf σvi if !timeout) {
        val Y1 = generatePosition(σvi, k)
        val Y2 = generatePosition(σvi, k + s.length)
        
        if(debugActive) {
          for(y1 <- Y1; y <- y1) {
            assert(evalProg(y)(IndexedSeq(σvi)) == IntValue(k))
          }
          for(y2 <- Y2; y <- y2) {
            assert(evalProg(y)(IndexedSeq(σvi)) == IntValue(k + s.length))          
          }
        }

        val newResult = SSubStr(InputString(vi), Y1, Y2, m)
        result = result + newResult
      }
      // If σvi is empty or does not contain any number, it should generate all possible numbers expressions for various sizes.
      if(s.isNumber) {
        for((start, end, steps) <- s subnumberIncNegativeOf σvi) { // Numbers that can be obtained from σvi by changing by steps for example.
          val Y1 = generatePosition(σvi, start)
          val Y2 = generatePosition(σvi, end+1)
          val possibleLengths = (if(s(0) != '0') {// It means that the generated length might be lower.
            (1 to s.length).toSet
          } else Set(s.length))
          if(!possibleLengths.isEmpty)
          result = result + SNumber(SSubStr(InputString(vi), Y1, Y2, SSubStrFlag(List(NORMAL))), possibleLengths.map(IntLiteral.apply), SAnyInt(0), SIntSet(Set(steps)))
        }
      }
    }
    // Generates numbers from previous numbers in output strings.
    if(s.isNumber) {
      if(σ.prevNumberOutputs.length == 0) {
        val i = s.toInt
        // For any number i, the set of matching counters are IntLiteral(i), Linear(1, i), Linear(2, i) up to Linear(i, i)
        result = result + SNumber(SAny(PrevStringNumber(0)), Set(s.length), SIntSet(Set(i)), SAnyInt(1))
      } else {
        for(vi <- 0 until σ.prevNumberOutputs.length) {
          val σvi =  σ.prevNumberOutputs(vi)
          for((start, end, steps) <- s addedNumberFrom σvi) { // Numbers that can be obtained from σvi by incrementing by steps for example.
            val Y1 = generatePosition(σvi, start)
            val Y2 = generatePosition(σvi, end+1)
            val sourceLength = end+1 - start
            val sourceLengthPossibilities = if(σvi(start) == '0') {
              Set(sourceLength)
            } else {
              (1 to sourceLength).toSet
            }
            val possibleLengths = sourceLengthPossibilities intersect (if(s(0) != '0') {// It means that the generated length might be lower.
            (1 to s.length).toSet
          } else Set(s.length))
            if(!possibleLengths.isEmpty)
            result = result + SNumber(SSubStr(PrevStringNumber(vi), Y1, Y2, SSubStrFlag(List(NORMAL))), possibleLengths.map(IntLiteral.apply), SAnyInt(0), SIntSet(Set(steps)))
          }
        }
      }
    }
    result
  }
  
  /**
   * Compute the set of all tokenseq between two given positions.
   */
  private var computedForString = ""
  private var computedForList = List[Token]()
  private var cacheComputeTokenSeq = MMap[(Start, End), Set[(TokenSeq, (List[Start], List[End]))]]()

  def computetokenSeq(s: String, listTokens: List[Token]): MMap[(Start, End), Set[(TokenSeq, (List[Start], List[End]))]] =
    if(s == computedForString && (listTokens eq computedForList)) cacheComputeTokenSeq else {
    val finalstart = 0
    val finalend = s.length-1
    var res = MMap[(Start, End), Set[(TokenSeq, (List[Start], List[End]))]]()
    def addMapping(i: Start, j: End, s: TokenSeq, index: (List[Start], List[End])) = 
      if(s.t.isEmpty || s.t.exists(_ != NonDotTok))
        res += (i, j) -> (res.getOrElse((i, j), Set()) + ((s, index)))
    //def removeMapping(t: TokenSeq) = res //= res.mapValues(s => s.filterNot(_._1 == t))
    val tokenPositions: Map[Token, (List[Start], List[End])] =
      (listTokens map { token =>
      token -> ScalaRegExp.computePositionsOfToken(token, s)
    }) toMap
    // Maps a position to a set of tokens with indexes
    val startTokens: Map[Start,List[(Token, (List[Start], List[End]))]] = tokenPositions.toList
       .flatMap{ case (tok, indexes@(liststart, listend)) => liststart map (i => (tok, i, listend))}
       .groupBy{case (tok, start, end) => start}
       .mapValues(list => list map { case (tok, start, index) => (tok, tokenPositions(tok)) })
    
    // Maps a position and a token to its end position
    val endTokensFromStart: Map[Start, Map[Token, End]] =
      tokenPositions.toList
       .flatMap{ case (tok, indexes@(liststart, listend)) => liststart map(i => (tok, i, listend))}
       .groupBy{case (tok, start, end) => start}
       .mapValues(list =>
         list map {
           case (tok, start, end) => 
             val ss = s.substring(start)
             val closestEnd = ScalaRegExp.computePositionsEndingWith(tok, ss)
             (tok, start + closestEnd.head)} toMap)
    
    // enumerate tokens sequence of length 0
    //val epsilonRange = (finalstart to finalend).toList zip (-1 until finalend).toList
    for(i <- finalstart to finalend) {
      addMapping(i, i-1, TokenSeq(), ((finalstart to finalend).toList, (-1 until finalend).toList))
    }
    // enumerate tokens sequence of length 1
    //addMapping(finalstart, finalstart-1, TokenSeq(StartTok)) // Simple token starting at 0
    //addMapping(finalend+1, finalend, TokenSeq(EndTok)) // Simple token starting at 0
    for(i <- finalstart to finalend) {
      for((tok, index) <- startTokens.getOrElse(i, Nil)
          if tok != StartTok && tok != EndTok
          ) { // But can be empty
        val end = endTokensFromStart(i)(tok)
        if(end >= i)
        addMapping(i, end, TokenSeq(tok), index) // Simple token starting at 0
      }
    }
    // enumerate tokens sequences of two tokens.
    val currentMapping = res
    for(((start, end), tokseqset) <- currentMapping; (tokseq, index) <- tokseqset if tokseq.t.size == 1 && end != finalend) {
      val contiguousTokens = startTokens(end + 1)
      val lastToken = tokseq.t.lastOption.getOrElse(null)
      for((tok2, index2) <- contiguousTokens if tok2 != lastToken) {
        val endnew = endTokensFromStart(end + 1)(tok2)
        val newTokenseq = TokenSeq(tokseq.t ++ List(tok2))
        addMapping(start, endnew, newTokenseq, ScalaRegExp.computePositionsOfRegExp(newTokenseq, s))
      }
    }
    // enumerate tokens sequences of three tokens.
    /*val currentMapping2 = res
    for(((start, end), tokseqset) <- currentMapping2; (tokseq, index) <- tokseqset if tokseq.t.size == 2 && end != finalend) {
      val contiguousTokens = startTokens(end + 1)
      val lastToken = tokseq.t.lastOption.getOrElse(null)
      for((tok2, index2) <- contiguousTokens if tok2 != lastToken) { // To prevent NonDotTok to appear twice.
        val endnew = endTokensFromStart(end + 1)(tok2)
        val newTokenseq = TokenSeq(tokseq.t ++ List(tok2))
        addMapping(start, endnew, newTokenseq,  ScalaRegExp.computePositionsOfRegExp(newTokenseq, s))
      }
    }*/
    // Add startoken and endtoken if it is related.
    val currentMapping3 = res
    for(((start, end), tokseqset) <- currentMapping3; (tokseq, (liststart, listend)) <- tokseqset) {
      if(start == finalstart) {
        addMapping(start, end, TokenSeq(StartTok::tokseq.t), (List(liststart.head), List(listend.head)))
      }
      if(start == finalend) {
        addMapping(start, end, TokenSeq(tokseq.t ++ List(EndTok)), (List(liststart.last), List(listend.last)))
      }
    }
    // Remove single NonDotTok anywhere.
    //removeMapping(TokenSeq(NonDotTok))
    //removeMapping(TokenSeq(NonDotTok, NonDotTok))
    //removeMapping(TokenSeq(NonDotTok, NonDotTok, NonDotTok))
    cacheComputeTokenSeq = res
    computedForString = s
    computedForList = listTokens
    res
  }
  
  /**
   * Returns a set of matching tokens before the end of the string or after the start of it beginning.
   * The first integer is the start or the end of the matching token.
   * The second integer is n such that the token is the nth one.
   */
  /*def before = true
  def after = false
  def matchingTokenSeq(s: String, atPos: Int, beforeOrAfter: Boolean=false, listTokens: List[Token]=Programs.listTokens): Iterable[(Start, TokenSeq, Index)] = {
    // Iterates from smaller tokenseq to bigger ones.
    if(beforeOrAfter) {
      val ms = computetokenSeq(s, listTokens) 
      for(i <- atPos to 0 by -1;
          (tokseq, index) <- ms.getOrElse((i, atPos), Set.empty)) yield (i, tokseq, index)
    } else {
      val ms = computetokenSeq(s, listTokens)
      for(j <- atPos to (s.length-1);
          (tokseq, index) <- ms.getOrElse((atPos, j), Set())) yield (j, tokseq, index)
    }
  }*/
  /**
   * Returns a list of (Start, End) for tokens matching at the position, with the common index)
   */
  def matchingTokenSeq(s: String, atPos: Int, listTokens: List[Token]=Programs.listNonEmptyTokens)
      : Iterable[(Start, End, TokenSeq, TokenSeq, List[Index])] = {
    val ms = computetokenSeq(s, listTokens)
    for(i <- atPos to 0 by -1;
        j <- (atPos-1) until s.length;
        if(i != atPos || j != atPos -1);
        (tok1, (liststart1, listend1)) <- ms.getOrElse((i, atPos-1), Set());
        (tok2, (liststart2, listend2)) <- ms.getOrElse((atPos, j), Set())) yield {
          val res1 = RegexpPositionsInString.computePositionsEndingWith(tok1, s).map(_ + 1)
          val res2 = RegexpPositionsInString.computePositionsStartingWith(tok2, s)
          val intersections = res1 intersect res2
          //val intersections = (listend1 map {case end =>  end + 1}) intersect (liststart2 map { case start => start })
          (i, j, tok1, tok2, intersections)
    }
  }
  /**
   * Returns an integer c such that position k is the cth for regexp in string
   */
  def th_match_of(k: Int, _for: RegExp, in: String): Int =  {
    ScalaRegExp.computePositionsStartingWith(_for, in).indexOf(k) + 1
  }
  
  /**
   * Returns the total number of matches of this regex in the string in.
   */
  def total_number_of_matches(_for: RegExp, in: String): Int = {
    ScalaRegExp.computePositionsEndingWith(_for, in).length
  }
  
  var cache_hit = 0
  var cache_call = 0
  var advanced_stats = false
  var advanced_cache = Map[Any, Int]()
  
  /**
   * Initialize the statistics
   */
  def initStats() = { cache_hit = 0; cache_call = 0 }
  initStats()
  
  /**
   * Generate a cache
   */
  def cached[T, A](s: T, cache: MMap[T, A])(f: => A) = {
    cache_call += 1
    if(cache contains s) {
      cache_hit += 1
      if(advanced_stats) advanced_cache = advanced_cache + (s -> (advanced_cache.getOrElse(s, 0) + 1))
    }
    cache.getOrElseUpdate(s, f)
  }
  /*def cached[U, T](f: U => T)(implicit cache: MMap[U, T]): U => T = u => {
    cache_call += 1
    if(cache contains u) {
      cache_hit += 1
      if(advanced_stats) advanced_cache = advanced_cache + (u -> (advanced_cache.getOrElse(u, 0) + 1))
    }
    
    cache.getOrElseUpdate(u, f(u))
  }
  def cached[U, V, T](f: (U, V) => T)(implicit cache: MMap[(U, V), T]): (U, V) => T = (u, v) => {
    cache_call += 1
    if(cache contains (u, v)) {
      cache_hit += 1
      if(advanced_stats) advanced_cache = advanced_cache + ((u, v) -> (advanced_cache.getOrElse((u, v), 0) + 1))
    }
    cache.getOrElseUpdate((u, v), f(u, v))
  }
  def cached[U, V, W, T](f: (U, V, W) => T)(implicit cache: MMap[(U, V, W), T]): (U, V, W) => T = (u, v, w) => {
    cache_call += 1
    if(cache contains (u, v, w)) {
      cache_hit += 1
      if(advanced_stats) advanced_cache = advanced_cache + ((u, v, w) -> (advanced_cache.getOrElse((u, v, w), 0) + 1))
    }
    cache.getOrElseUpdate((u, v, w), f(u, v, w))
  }*/
  
  /**
   * Generates a set of algebraic positions for a given position and a string.
   */
  implicit val cache = MMap[(String, Int), Set[SPosition]]()
  def generatePosition(σ: String, k: Int) = cached((σ, k), cache){
    if(verbose) println(s"Generating position $k")
    var result = Set[SPosition](SCPos(k), SCPos(-σ.length+k-1))
    implicit val (tokenSet, mapping) = Reps(σ)
    for((_, _, r1@TokenSeq(t_list1), r2@TokenSeq(t_list2), intersections) <- matchingTokenSeq(σ, atPos=k, listTokens=tokenSet)) {   
        val c = intersections.indexOf(k)
        if( c >= 0) { // This if false for strange tokenizations.
          //val c = th_match_of(k1, _for=r12, in=s)
          val cp = intersections.length
          //val cp = total_number_of_matches(_for=r12, in=s)
          assert(cp >= 1)
          val r1p = generateRegex(r1, σ) // Expands all tokens.
          val r2p = generateRegex(r2, σ)
          val res = SPos(r1p, r2p, Set(c + 1, -(cp-c)))
          result += res
        }
    }
    result
  }
  
  def generateRegex(r: Regular_Expression, s: String)(implicit map: Map[Token, List[Token]]): SRegExp = {
    r match {
      case TokenSeq(l) =>
        STokenSeq(l map ((t: Token) => IParts(s, t)))
    }
  }
  
  /**
   * Creates the equivalence class of a token.
   */
  //implicit val cacheIParts = MMap[String, Map[Token,List[Token]]]()
  def IPart_s(s: String) = {
    val listTokens = Programs.listNonEmptyTokens
    val res: Map[Token,List[Token]] = listTokens.map (tok => (tok, ScalaRegExp.computePositionsOfToken(tok, s)))
      .groupBy( t => t._2)
      .mapValues(t => t map (_._1))
      .values
      .map(t => (t.head, t))
      .toMap
    res
  }
  def IParts(ss: String, t: Token)(implicit map: Map[Token,List[Token]] = IPart_s(ss)): SToken = if(t == StartTok || t == EndTok) SToken(Set(t))(Programs.listTokens) else SToken(map(t).toSet)(Programs.listTokens)
  
  private var cacheReps = MMap[String, (List[Token], Map[Token,List[Token]])]()
  /** Returns a subset of equivalent tokens */
  def Reps(s: String): (List[Token], Map[Token,List[Token]]) = cached(s, cacheReps){
    val res = IPart_s(s)
    (res.values.toList
    .map(t => t.head), res)
  }
  
  private def insert[Repr, That](e: Repr, p: List[Repr], n: Int, betterThan: (Repr, Repr) => Boolean): List[Repr] = n match {
    case 0 => List(e)
    case i => if(p == Nil) List(e) else if(betterThan(e, p.head)) e::p.take(i-1)
      else {
        p.head::insert(e, p.tail, n-1, betterThan)
      }
  }
  private def insert5(e: (String, Int), p: List[(String, Int)]) = {
    insert(e, p, 5, { (si: (String, Int), tj: (String, Int)) => si._2 > tj._2})
  }
  
  var stats_unifications = 0
  var stats_time_unifications = 0L
  def statistics(): String = {
    val average = if(stats_unifications == 0) 0 else stats_time_unifications.toFloat/stats_unifications
    //("Number of elements in the cache 1:" + cacheIParts.size) + "\n"+
    ("Number of elements in cacheGenerateStr:" + cacheGenerateStr.size) + "\n"+
    ("Number of elements in cacheGenerateSubstring:" + cacheGenerateSubstring.size) + "\n"+
    ("Number of elements in the cache:" + cache.size) + "\n"+
    ("Number of times unified DAGS:" + stats_unifications) + s" in average of $average ns\n"+
    ("Number of cache reuse:" + cache_hit + "/" + cache_call) + "\n"+
    (if(advanced_stats) {
      val most_elements = advanced_cache.foldLeft(Nil: List[(String, Int)])
        { case (p, (e, i) )=> insert5((e.toString, i), p)}
      most_elements mkString "\n"
    } else "")
  }
  
  /**
   * Empty the caches.
   */
  def emptyCaches() = {
    //cacheIParts.clear()
    cacheGenerateStr.clear()
    cacheGenerateSubstring.clear()
    cache.clear()
  }
  
  def min(a: Int, b: Int, c: Int) = {
    if(a < b) {
      if(a < c) {
        a
      } else {
        c
      }
    } else { // b <= a
      if(b < c) {
        b
      } else {
        c
      }
    }
  }
  def LevenshteinDistance(s: String, t: String): Int = {
    val m = s.length
    val n = t.length
    // for all i and j, d[i,j] will hold the Levenshtein distance between
    // the first i characters of s and the first j characters of t;
    // note that d has (m+1)*(n+1) values
  
    val d = Array.ofDim[Int](m+1, n+1)
   
    //clear all elements in d // set each element to zero
   
    // source prefixes can be transformed into empty string by
    // dropping all characters
    (1 to m) foreach { i =>
      d(i)(0) = i
    }
   
    // target prefixes can be reached from empty source prefix
    // by inserting every characters
    (1 to n) foreach { j =>
      d(0)(j) = j
    }
   
    (1 to n) foreach { j =>
        (1 to m) foreach { i =>
            if(s(i-1) == t(j-1))
              d(i)(j) = d(i-1)(j-1)       // no operation required
            else
              d(i)(j) = min(
                        d(i-1)(j) + 1,  // a deletion
                        d(i)(j-1) + 1,  // an insertion
                        d(i-1)(j-1) + 1 // a substitution
                      )
          }
      }
  
    return d(m)(n)
  }
  
}