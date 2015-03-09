/**
 *     _____ _       _         _____     _             
 *    |   __| |_ ___|_|___ ___|   __|___| |_ _ ___ ___ 
 *    |__   |  _|  _| |   | . |__   | . | | | | -_|  _|
 *    |_____|_| |_| |_|_|_|_  |_____|___|_|\_/|___|_|  
 *                        |___|      
 * 
 *  File:   Stringsolver.scala
 *  Author: Mikaël Mayer
 *  Date:   27.11.2013
 *  Purpose;Provides all algorithms related to synthesis
 */
package ch.epfl.lara.synthesis.stringsolver

import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashMap => MMap}
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.matching.Regex

import ProgramSet._

/**
 * An extension delivers start and end position to which it applies,
 * and a way to create a SSpecialConversion out of a SSubStr.
 */
trait Extension {
  def apply(s: String, output: String): Seq[(Int, Int, SSubStr => SSpecialConversion)]
}

/** Class to use StriSynth very easily
 */
object CurrentInstance {
  def HELP = println("""
NEW       Trigger learning of a new program. Automatic first time.

          Map and Reduce
"input" ==> "output"
("input", index) ==> "output"
(List("input1", ...), index) ==> "output"
List("input1", ...) ==> "output"

         Split examples.
"input" ==> ("output1", "output2", ..., "...")

         Partition examples.
PARTITIONTYPE("example1-1", "example1-2", ...)

         Filter examples.
"input1" ==> YES
List("input1", ...) ==> YES
"input2" ==> NO
List("input2", ...) ==> NO
      
         Exporting the program
PROGRAM in Powershell to "script.ps1"
PROGRAM in Scala to "script.scala"
PROGRAM in Bash to "script.sh"
        
HELP     Displays this help
  """)
  private sealed trait LearningType
  private object MAPTYPE extends LearningType { override def toString = "map/reduce" }
  private object PARTITIONTYPE extends LearningType { override def toString = "partition" }
  private object FILTERTYPE extends LearningType { override def toString = "filter" }
  private object SPLITTYPE extends LearningType { override def toString = "split" }
  private object ALL extends LearningType { override def toString = "split" }
  
  private var _currentSolver: StringSolver = null;
  private var currentType: LearningType = ALL
  
  private var programToRecompute = true;
  
  private def createNew(): Unit = {
    _currentSolver = StringSolver()
  }
  
  def NEW = {
    currentType = ALL
    CurrentInstance.createNew()
  }
  import Program._
  def PROGRAM: ReduceProgram = ReduceProgram(_currentProgram)
  def REDUCE: ReduceProgram = ReduceProgram(_currentProgram)
  //def MAP: Mapper[List[String], String] = MapProgram(_currentProgram)
  def MAP: TransformProgram = TransformProgram(_currentProgram)
  def PARTITION: PartitionProgram = _currentPartitionProgram
  def FILTER: FilterProgram = _currentFilterProgram
  
  def CANCEL = currentType match {
    case ALL => println("Nothing to cancel")
    case MAPTYPE => 
      currentSolver.cancelLast()
      solve()
    case PARTITIONTYPE =>
      if(partitionExamples.nonEmpty) {
        partitionExamples = partitionExamples.init
        solve()
      } else println("Nothing to cancel")
    case FILTERTYPE => 
      if(filterExamples.nonEmpty) {
        filterExamples = filterExamples.init
        solve()
      } else println("Nothing to cancel")
    case SPLITTYPE =>
      _currentSolver.cancelLast()
      solve()
  }
  def SPLIT: SplitProgram = _currentSplitProgram
  
  def currentSolver = if(_currentSolver == null) {
    NEW
    _currentSolver
  } else _currentSolver
  
  import StringSolver.{InputOutputExample, Input_state, SplitExample, PartitionExample}
  
  private var _currentProgram: Program = null
  private var _currentSplitProgram: SplitProgram = null
  private var _currentPartitionProgram: PartitionProgram = null
  private var _currentFilterProgram: FilterProgram = null
  private var partitionExamples = List[PartitionExample]()

  def solve(): Unit = currentType match {
    case ALL => println("No example given. Type DOC to get the documentation")
    case MAPTYPE => _currentSolver.solve() match {
      case Some(program) =>
        _currentProgram = program
        println(_currentProgram)
      case None => 
        println("No map/reduce program found. To cancel the last example, please type CANCEL. To reset, call NEW")
    }
    case PARTITIONTYPE =>
      partitionExamples.length match {
        case 0 => println("Please write two partition examples like ==>(\"part1\", \"part2\") before continuing")
        case 1 => println("Please write one more partition example like ==>(\"part1\", \"part2\")")
        case n =>
          val input = partitionExamples.zipWithIndex.flatMap{case (p, i) => p.partition.map(e => (e, i.toString))};
          Service.getPartition(input.toList) match {
            case Some((c, c2, f)) =>
              c.solve() match {
                case Some(p) =>
                  _currentPartitionProgram = PartitionProgram(p)
                   println(_currentPartitionProgram)
                case None =>
                  println("No PARTITION program found. CANCEL the last example or NEW to create a new program")
              }
            case None =>
              println("No PARTITION program found. CANCEL the last example or NEW to create a new program")
          }
      }
      
    case FILTERTYPE => 
      if(filterExamples.length == 0) println("Need at least one OK==>(example) to learn FILTER. Type RESET to reset")
      else {
        Service.getFilter(filterExamples.flatten) match {
          case Some((ss, m)) =>
            ss.solve() match {
              case Some(s) =>
                _currentFilterProgram = FilterProgram(s, m)
                println(_currentFilterProgram)
              case None =>
                println("No FILTER program found. Add a new example, CANCEL the last example or NEW to create a new program")
            }
          case None =>
            println("No FILTER program found. Add a new example, CANCEL the last example or NEW to create a new program")
        }
      }
    case SPLITTYPE =>
      _currentSolver.solve() match {
      case Some(program) =>
        _currentSplitProgram = SplitProgram(program)
        println(_currentSplitProgram)
      case None => 
        println("No split program found. To cancel the last example, please type CANCEL. To reset, call NEW")
    }
  }
  
  implicit class StringWrapper(input: String) {
    def ==>(output: String): Unit = {
      currentType match {
        case ALL | MAPTYPE =>
          if(currentType != MAPTYPE) println("Learning MAP and/or REDUCE")
          currentType = MAPTYPE
          currentSolver.add(InputOutputExample(Input_state(IndexedSeq(input), 0), output, false))
          solve()
        case _ => println("Impossible to add a map or reduce example. Learning a " + currentType + " program. To reset, please invoke NEW.")
      }
    }
  }
  
  implicit class StringIndexWrapper(inputIndex: (String, Int)) {
    def ==>(output: String): Unit = {
      currentType match {
        case ALL | MAPTYPE =>
          if(currentType != MAPTYPE) println("Learning MAP and/or REDUCE")
          currentType = MAPTYPE
          currentSolver.add(InputOutputExample(Input_state(IndexedSeq(inputIndex._1), inputIndex._2-1), output, true))
          solve()
        case _ => println("Impossible to add a map example. Learning a " + currentType + " program. To reset, please invoke NEW.")
      }
    }
  }
  
  implicit class TupleListWrapper(inputsIndex: List[String]) {
    def ==>(output: String): Unit = {
      currentType match {
        case ALL | MAPTYPE =>
        if(currentType != MAPTYPE) println("Learning MAP and/or REDUCE")
        currentType = MAPTYPE
        currentSolver.add(InputOutputExample(Input_state(inputsIndex.toIndexedSeq, 0), output, false))
        solve()
        case _ => println("Impossible to add a map/reduce example. Learning a " + currentType + " program. To reset, please invoke NEW.")
      }
    }
  }
  
  implicit class TupleListIndexWrapper(inputsIndex: (List[String], Int)) {
    def ==>(output: String): Unit = {
      currentType match {
        case ALL | MAPTYPE =>
        if(currentType != MAPTYPE) println("Learning MAP and/or REDUCE")
        currentType = MAPTYPE
        currentSolver.add(InputOutputExample(Input_state(inputsIndex._1.toIndexedSeq, inputsIndex._2-1), output, true))
        solve()
        case _ => println("Impossible to add a map/reduce example. Learning a " + currentType + " program. To reset, please invoke NEW.")
      }
    }
  }

  implicit class TupleWrapper2(inputs: (String, String)) {
    def ==>(output: String): Unit = List(inputs._1, inputs._2) ==> output
  }
  implicit class TupleWrapper3(inputs: (String, String, String)) {
    def ==>(output: String): Unit = List(inputs._1, inputs._2, inputs._3) ==> output
  }
  implicit class TupleWrapper4(inputs: (String, String, String, String)) {
    def ==>(output: String): Unit = List(inputs._1, inputs._2, inputs._3, inputs._4) ==> output
  }
  implicit class TupleWrapper2Index(inputs: ((String, String), Int)) {
    def ==>(output: String): Unit = (List(inputs._1._1, inputs._1._2), inputs._2) ==> output
  }
  implicit class TupleWrapper3Index(inputs: ((String, String, String), Int)) {
    def ==>(output: String): Unit = (List(inputs._1._1, inputs._1._2, inputs._1._3), inputs._2) ==> output
  }
  implicit class TupleWrapper4Index(inputs: ((String, String, String, String), Int)) {
    def ==>(output: String): Unit = (List(inputs._1._1, inputs._1._2, inputs._1._3, inputs._1._4), inputs._2) ==> output
  }
  
  implicit class SplitWrapper(input: String) {
    def ==>(output1: String, output2: String, outputs: String*): Unit = ==>(output1::output2::outputs.toList)
    def ==>(outputs: List[String]): Unit = {
      currentType match {
        case ALL | SPLITTYPE =>
        if(currentType != SPLITTYPE) println("Learning SPLIT")
        currentType = SPLITTYPE
        currentSolver.add(SplitExample(input, outputs.takeWhile(s => s != "...")))
        solve()
        case _ => println("Impossible to add a split example. Learning a " + currentType + " program. To reset, please invoke NEW.")
      }
    }
  }
  
  def ==>(partition: String*) = currentType match {
      case ALL | PARTITIONTYPE =>
      if(currentType == ALL) {
        partitionExamples = Nil
        println("Learning PARTITION")
      }
      currentType = PARTITIONTYPE
      partitionExamples = partitionExamples ++ List(PartitionExample(partition.toList))
      solve()
      case _ => println("Impossible to add a partition example. Learning a " + currentType + " program. To reset, please invoke NEW.")
  }
  var filterExamples: List[List[(String, Boolean)]] = Nil
  
  sealed abstract class FilterToken(positive: Boolean) {
    private def addExamples(strs: List[String]) {
      filterExamples = filterExamples ++ List(strs.map(s => (s, positive)))
      solve()
    }
    private def addAndCheck(accepted: List[String]) = currentType match {
      case ALL | FILTERTYPE =>
      if(currentType == ALL) {
        filterExamples = Nil
        println("Learning FILTER")
      }
      currentType = FILTERTYPE
      addExamples(accepted)
      case _ => println("Impossible to add a filter example. Learning a " + currentType + " program. To reset, please invoke NEW.")
    }
    def ==>(accepted: String*): Unit = ==>(accepted.toList)
    def ==>(accepted: List[String]): Unit = addAndCheck(accepted)
  }
  object YES extends FilterToken(true)
  object NO extends FilterToken(false)
  val OK, Ok, ok, Yes, yes = YES
  val NOTOK, NotOk, Notok, notok, No, no = NO
  
  sealed trait MapConstant
  object map extends MapConstant
	
	implicit class MapWrapper[In, Out](e: ExportableWithType[In, Out]) {
	  def as(dummy: MapConstant) = Mapper(e)
	}
}

/**StringSolver object
 * Used to create a StringSolver instance to solve input/output problems.
 */
object StringSolver {
  import Program._  
  import ProgramSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  final val debugActive = false
  
  private type PrevNumberString = String
  type Output_state = String //case class Output_state(value: String, position: Int)
  type S = String
  case class Input_state(inputs: IndexedSeq[String], position: Int)
  type σ = Input_state
  type Regular_Expression = RegExp
  type W[Node] = Map[(Node, Node), Set[SAtomicExpr]]
  type Start = Int
  type End = Int
  type Index = Int
  
  case class InputOutputExample(inputState: Input_state, output: Output_state, indexSet: Boolean)
  
  case class SplitExample(input: String, outputs: List[String])  
  //def debug(s: String) = if(debugActive) println(s)
  
  case class PreExample(index: Int, output: String)
  
  /*implicit class Wrapper(index: Int) {
    def ==>(output: String) = PreExample(index, output)
  }
  
  implicit class Wrapper2(input: String) {
    def index(remaining: PreExample) = InputOutputExample(Input_state(IndexedSeq(input), remaining.index-1), remaining.output, true)
    def index(i: Int) = Input_state(IndexedSeq(input), i-1)
    def ==>(output: String) = InputOutputExample(Input_state(IndexedSeq(input), 0), output, false)
  }
  
  implicit class WrapperInputIndex(inputIndex: (String, Int)) {
    def ==>(output: String) = InputOutputExample(Input_state(IndexedSeq(inputIndex._1), inputIndex._2-1), output, true)
  }
  
  implicit class Wrapper3(inputs: List[String]) {
    def index(remaining: PreExample) = InputOutputExample(Input_state(inputs.toIndexedSeq, remaining.index-1), remaining.output, true)
    def index(i: Int) = Input_state(inputs.toIndexedSeq, i-1)
    def ==>(output: String) = InputOutputExample(Input_state(inputs.toIndexedSeq, 0), output, false)
  }
  
  implicit class WrapperSplit(input: String) {
    def ==>(output1: String, outputs: String*): SplitExample = {
      ==>(output1::outputs.toList);
    }
    def ==>(outputs: List[String]): SplitExample = {
      SplitExample(input, outputs.takeWhile(s => s != "..."))
    }
  }*/
  
  case class PartitionExample(partition: List[String])
  
  implicit def toPartitionExample2(input: (String, String)) = PartitionExample(List(input._1, input._2))
  implicit def toPartitionExample3(input: (String, String, String)) = PartitionExample(List(input._1, input._2, input._3))
  implicit def toPartitionExample3(input: (String, String, String, String)) = PartitionExample(List(input._1, input._2, input._3, input._4))
  

  
  def apply(): StringSolver = new StringSolver()
  
  def apply(example: InputOutputExample, remaining: InputOutputExample*): Program = {
    val examples = StringSolver()
    for(e <- (example::remaining.toList)) {
      if(e.indexSet) {
        examples.add(e.inputState, e.output)
      } else {
        examples.add(e.inputState.inputs, e.output)
      }
    }
    examples.solve().getOrElse(null)
  }
  
  def apply(input: List[List[String]], output: List[String]): Option[Program] = {
    val solver = apply()
    (input zip output) foreach { case (i, o) =>
      solver.add(i, o)
    }
    solver.solve()
  }
  
  def apply(example: SplitExample, remaining: SplitExample*): SplitProgram = {
    val examples = StringSolver()
    for(e <- (example::remaining.toList)) {
      examples.add(e);
    }
    val res = examples.solve().getOrElse(null)
    if(res != null) SplitProgram(res) else null
  }
  
  //def apply(example: PartitionExample, remaining: PartitionExample*): PartitionProgram = {
  //  null //Service.getPartition(examples, c, c2, opt)
  //}
  
  //implicit def indexedSeqToInputState(arr: IndexedSeq[String]) = Input_state(arr, IndexedSeq[String]())
}

/**
 * Instance solving the problem iteratively
 */
class StringSolver {
  import Program._  
  import ProgramSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  import StringSolver._
  private var ff = new StringSolverAlgorithms()
  private var currentPrograms: IndexedSeq[STraceExpr] = null
  private var singlePrograms  = ArrayBuffer[IndexedSeq[STraceExpr]]()
  //private var previousOutputs: IndexedSeq[String] = Array[String]()

  private var inputList = List[List[String]]()
  private var outputList = List[List[String]]()
  
  private var extra_time_to_merge = 2f
  private var extra_time_to_compute_loop = 0.5f
  private var extra_time_to_resolve = 2f
  
  private var index_number = 0
  
  def copy(): StringSolver= { // TODO: better copy method
    val d = new StringSolver()
    d.currentPrograms = this.currentPrograms
    d.singlePrograms = ArrayBuffer(this.singlePrograms : _*)
    d.inputList = this.inputList
    d.outputList = this.outputList
    d.extra_time_to_merge = extra_time_to_merge
    d.extra_time_to_compute_loop = extra_time_to_compute_loop
    d.extra_time_to_resolve = extra_time_to_resolve
    d.index_number = index_number
    d.ff = ff.copy()
    d
  }
  
  /**
   * Proportion of the original time to give to compute loops
   */
  def setExtraTimeToComputeLoops(f: Float) = {extra_time_to_compute_loop = f; this}
  
  /**
   * Proportion of the original time to resolve from computing loops
   */
  def setExtraTimeToResolve(f: Float) = {extra_time_to_resolve = f; this}
  
  /**
   * Possibility to reset the counter used to number files
   */
  def resetCounter() = { index_number = 0; this}
  
  /**
   * Sets the current input/output position example to i
   */
  def setPosition(i: Int) = {index_number = i; this}
  
  /**
   * Use dots ... to trigger manual loop research
   */
  def setUseDots(b: Boolean) = {ff.useDots = b; this}
  
  /**
   * Use numbering from previous input option
   */
  def setUseNumbers(b: Boolean, undoBuffer: UndoBuffer = null) = {
    if(undoBuffer != null) undoBuffer.add(((last: Boolean) => () => ff.numbering = last)(ff.numbering));
    ff.numbering = b;
    this
  }
  
  /**
   * Loop level. 0 will not look for loops
   */
  def setLoopLevel(i: Int) = {ff.DEFAULT_REC_LOOP_LEVEL = i; this}
  
  /**
   * Timeout in seconds to add a new input/output example.
   * This is approximate. Default is 15s
   */
  def setTimeout(seconds: Int) = {ff.TIMEOUT_SECONDS = seconds; this}
  
  /**
   * If looking for loops, what could be the maximum separator length
   */
  def setMaxSeparatorLength(length: Int) = {ff.MAX_SEPARATOR_LENGTH = length; this}
  
  /**
   * If only interesting positions (aka word, special chars and digit separators)
   * are considered when looking for loops
   */
  def setOnlyInterestingPositions(b: Boolean) = {ff.onlyInterestingPositions = b; this}
  
  /**
   * Outputs programs steps. Useful for debugging an other.
   */
  def setVerbose(b: Boolean) = {ff.verbose = b; this}
  def isVerbose = ff.verbose
  
  /**
   * Allows to iterate over inputs.
   */
  def setIterateInput(b: Boolean) = ff.iterateInput = b
  
   /**
   * Allows to use the example index for positions
   */
  def setUseIndexForPosition(b: Boolean, undoBuffer: UndoBuffer = null) = {
    if(undoBuffer != null) undoBuffer.add(((last: Boolean) => () => ff.useIndexForPosition = last)(ff.useIndexForPosition));
    ff.useIndexForPosition = b
  }
  
   /**
   * Retrieves statistics
   */
  def getStatistics(): String = ff.statistics()
  
   /**
   * Advanced stats.
   */
  def setAdvancedStats(b: Boolean) = ff.advanced_stats = b
  
  /**
   * Extra time to merge as a proportion of timeout
   */
  def setExtraTimeToMerge(f: Float) = extra_time_to_merge = f
  
  /* Undo redo mechanism
   */
  sealed trait Undo {
    def undo()
  }
  case class UndoAction(f : () => Unit) extends Undo { def undo() = f() }
  import collection.mutable.ListBuffer
  case class UndoBuffer(l: ListBuffer[Undo] = ListBuffer[Undo]()) extends Undo {
    UndoList = this::UndoList
    def undo() = l.toList.reverse.foreach(el => el.undo())
    def add(f: () => Unit) = l += UndoAction(f)
  }
  var UndoList = List[Undo]()
  
  def cancelLast(): Unit = UndoList match {
    case head::tail => UndoList = tail
      head.undo()
    case Nil => println("Nothing to cancel")
  }
  
  /**Adds a new inputs/outputs example.
   **/
  def add(input: Seq[String], output: Seq[String]): Seq[STraceExpr] = {
    val undo = UndoBuffer()
    if(!(output.exists(out => out.exists(_.isDigit)))) { // If not digit for output, we don't use numbers.
      setUseNumbers(false, undo)
    }
    if(inputList != Nil && input == inputList.last) { // Parsing case. We use index for position
      setUseIndexForPosition(true, undo)
    }
    undo.add(((last: List[List[String]]) => () => inputList = last)(inputList))
    undo.add(((last: List[List[String]]) => () => outputList = last)(outputList))
    undo.add(((last: Int) => () => index_number = last)(index_number))
    
    inputList = inputList ++ List(input.toList)
    outputList = outputList ++ List(output.toList)
    
    val iv = input.toIndexedSeq
    val ov = output.toIndexedSeq
    val tmpIndexNumber = index_number
    
    val fetchPrograms = future {
      for(out <- ov) yield
      ff.generateStr(Input_state(iv, tmpIndexNumber), out, ff.DEFAULT_REC_LOOP_LEVEL)
    }
    index_number += 1
    
    var tmp = ff.DEFAULT_REC_LOOP_LEVEL
    val newProgramSets : IndexedSeq[STraceExpr] = try {
      Await.result(fetchPrograms, ff.TIMEOUT_SECONDS.seconds)
    } catch {
      case e: TimeoutException  => 
        if(output.exists(_.indexOf("...") != -1 && ff.useDots)) { // Stop first phase of computing GenerateStr if there are still dots to compute.
          ff.timeoutGenerateStr = true
          try {
            Await.result(fetchPrograms, (ff.TIMEOUT_SECONDS * extra_time_to_compute_loop).seconds)
          } catch {
            case e: TimeoutException =>
              ff.DEFAULT_REC_LOOP_LEVEL = 0
              ff.timeout = true
              Await.result(fetchPrograms, (ff.TIMEOUT_SECONDS * extra_time_to_resolve).seconds)
          }
        } else {
          ff.timeoutGenerateStr = true
          ff.DEFAULT_REC_LOOP_LEVEL = 0 // No loops this time, especially if there is no
          ff.timeout = true
          Await.result(fetchPrograms, (ff.TIMEOUT_SECONDS * extra_time_to_resolve).seconds)
        }
      case e: Throwable => throw e
    }
    ff.DEFAULT_REC_LOOP_LEVEL = tmp
    ff.timeout = false
    add(newProgramSets)
  }
  
  /**Adds a new program set
   **/
  def add(newProgramSets: IndexedSeq[STraceExpr]): IndexedSeq[STraceExpr] = {
    if(currentPrograms == null) {
      currentPrograms = newProgramSets
    } else {
      val waiting_seconds =  (ff.TIMEOUT_SECONDS * extra_time_to_merge).toInt
      if(ff.verbose) println(s"Computing intersection with previous programs... (waiting $waiting_seconds seconds)")
      val intersectionParams = for(i <-0 until currentPrograms.length) yield {
        IntersectParam(None, currentPrograms(i).examplePosition, newProgramSets(i).examplePosition, false, ff.useIndexForPosition)
      }
      val intersectionsFuture = future {
        for(i <-0 until currentPrograms.length) yield {
          //println(s"Intersecting programs $i")
          intersect(currentPrograms(i), newProgramSets(i))(intersectionParams(i))
        }
        //(currentPrograms zip newProgramSets) map { case (a, b) => intersect(a, b) }
      }
      val intersections = try {
        Await.result(intersectionsFuture, waiting_seconds.seconds)
      } catch {
        case e: TimeoutException  => 
          if(ff.verbose) println(s"Intersection took too much time! Resolving what has been done... (waiting ${ff.TIMEOUT_SECONDS * extra_time_to_resolve} seconds)")
          intersectionParams.foreach{ f => f.timeout = true}
          try {
          Await.result(intersectionsFuture, (ff.TIMEOUT_SECONDS * extra_time_to_resolve).seconds)
          } catch {
            case e: TimeoutException =>
              if(ff.verbose) println("Resolving took too much time! Intersection cancelled")
              currentPrograms map {_ => SEmpty}
          }
          //throw e
      }
      currentPrograms = intersections
    }
    singlePrograms += newProgramSets
    if(debugActive) verifyCurrentState()
    newProgramSets
  }
  
  /**Adds a new program set
   **/
  def add(newProgramSets: STraceExpr): STraceExpr = {
    add(IndexedSeq(newProgramSets))(0)
  }
 
  /**Adds a new input/output example.
   * If the best program already matches the input/output example,
   * it is not recomputed.
   **/
  def add(input: Seq[String], output: String): STraceExpr = {
    val res = add(input, IndexedSeq(output))
    res(0)
  }
  
  /**Adds a new input/output example.
   * If the best program already matches the input/output example,
   * it is not recomputed.
   **/
  def add(input: String, output: String): STraceExpr = {
    val res = add(IndexedSeq(input), IndexedSeq(output))
    res(0)
  }
  
  /**Adds a new input/output example.
   * If the best program already matches the input/output example,
   * it is not recomputed.
   **/
  def add(inputOutput: Input_state, output: Output_state ): STraceExpr = {
    this.index_number = inputOutput.position
    val res = add(inputOutput.inputs, IndexedSeq(output))
    res(0)
  }
  
  /**Adds a new input/output example.
   * If the best program already matches the input/output example,
   * it is not recomputed.
   **/
  def add(input: String, output: String, index: Int): STraceExpr = {
    this.index_number = index
    val res = add(IndexedSeq(input), IndexedSeq(output))
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
   * Adds a new input/output example using the InputOutputExample API.
   */
  def add(e: InputOutputExample) {
    if(e.indexSet) {
      add(e.inputState, e.output)
    } else {
      add(e.inputState.inputs, e.output)
    }
  }
  
  /** Adds a new input/output example
   */
  def add(e: SplitExample) {
    for((output, i) <- e.outputs.zipWithIndex) {
      add(e.input, e.outputs(i), i+1)
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
  def solve(input: String, rawMode: Boolean = false): String = {
    if(input.indexOf('\n') != -1 && !rawMode) {
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
      if(input.indexOf('|') != -1 && !rawMode) {
        val elems = input.split("\\|").map(_.trim()).toList
        solve(elems).mkString(" | ")
      } else {
        solve(List(input)).mkString
      }
    }
  }
  
  /** Returns the best solutions to the problem for the whole output */
  def solveAll(): List[Option[Program]] = for(i <- (0 until currentPrograms.length).toList) yield solve(i)
  
  /** Returns the best solution to the last problem for the whole output */
  def solveLasts(): List[Option[Program]] = for(i <- (0 until currentPrograms.length).toList) yield solveLast(i)
  
  def solve(): Option[Program] = solve(0)
  
  /** Returns the best solution to the problem for the whole output */
  def solve(nth: Int): Option[Program] = if(currentPrograms != null) try {
    val res = Some(currentPrograms(nth).takeBest)
    if(debugActive) verifyCurrentState()
    res
  } catch {
    case e: java.lang.Error => 
      if(isVerbose) {
        println(e.getMessage())
        println(e.getStackTrace().mkString("\n"))
      }
      None
    case e: Exception => if(isVerbose) {
        println(e.getMessage())
        println(e.getStackTrace().mkString("\n"))
      }
    None
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
        val r = evalProg(prog)(Input_state(input.toIndexedSeq, index_number))
        r.asString
      } catch {
        case _: java.lang.Error => ""
        case _: Exception => ""
      }
    )
    index_number += 1
    //previousOutputs = res.toIndexedSeq
    res
  }
  
  /**
   * Solves using the last input/output example
   * CAREFUL: It increments the index_number, leading to undesirable results.
   */
  def solveLast(input: Seq[String]): Seq[String] = {
    //println(s"Solving for input $input")
    val res = singlePrograms(singlePrograms.length - 1) map (progSet => 
      try {
        //println(s"progSet $progSet")
        val prog = progSet.takeBest
        val r = evalProg(prog)(Input_state(input.toIndexedSeq, index_number))
        r.asString
      } catch {
        case _: java.lang.Error => ""
        case _: Exception => ""
      }
    )
    index_number += 1
    //previousOutputs = res.toIndexedSeq
    res
  }

  /**
   * Verifies the current state so that the resulting program
   * works for everybody.
   */
  private def verifyCurrentState() = {
    var previousOutputsTmp = IndexedSeq[String]()
    var tmpIndex = 0
    for((inputs, outputs) <- (inputList zip outputList).view;
        index = { val tmp = tmpIndex; tmpIndex += 1; tmp };
        (output, progs) <- (outputs zip currentPrograms).view;
        prog = progs.takeBest
    ) {
      previousOutputsTmp = outputs.toIndexedSeq
      evalProg(prog)(Input_state(inputs.toIndexedSeq, index)) match {
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
  import Program._  
  import ProgramSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  import StringSolver._
  
  // Parameter: Are we using dots to describe not finished loops.
  var useDots = true
  // Adds the last output number-only as input for the next program.
  var numbering = true
  // If a substring to extract is a space, should it be extracted from the inputs
  var extractSpaces = false
  // Use date conversion (english)
  def useDates: Boolean = extensions.indexOf(Dates) != -1
  def useDates_=(b: Boolean) = {
    if(b) extensions = (Dates::extensions).distinct else extensions = extensions.filterNot(_ ==Dates)
  }
  
  var extensions = List[Extension]()
  def copy(): StringSolverAlgorithms = {
    val a = new StringSolverAlgorithms()
    a.useDots = useDots
    a.numbering = numbering
    a.extractSpaces = extractSpaces
    a.TIMEOUT_SECONDS = TIMEOUT_SECONDS
    a.DEFAULT_REC_LOOP_LEVEL = DEFAULT_REC_LOOP_LEVEL
    a.MAX_SEPARATOR_LENGTH = MAX_SEPARATOR_LENGTH
    a.onlyInterestingPositions = onlyInterestingPositions
    a.verbose = verbose
    a
  }
  
  useDates = true
  
  final val dots = "..."
  def dotsAtPosition(s: String, k: Int) = s.substring(k, Math.min(k + dots.length, s.length)) == dots
 
  var TIMEOUT_SECONDS = 15
  var DEFAULT_REC_LOOP_LEVEL = 1
  var MAX_SEPARATOR_LENGTH = 1
  var onlyInterestingPositions = false
  
  var verbose = false
  
  // Possibility to iterate over input
  var iterateInput = true
  
  // Set to true if we want to extract stuff from the same string.
  var useIndexForPosition = false

  @volatile private var mTimeout = false
  @volatile private var mTimeoutPhaseGenerateStr = false
  def timeoutGenerateStr = mTimeoutPhaseGenerateStr
  def timeoutGenerateStr_=(v: Boolean) = {
    if(verbose && v) println("TimeoutGeneratestr")
    mTimeoutPhaseGenerateStr = v
  }
  def timeout = mTimeout
  def timeout_=(v: Boolean): Unit = {
    if(verbose && v) println("Timeout")
    mTimeout = v
    mTimeoutPhaseGenerateStr = v
    if(v) {
      ifTimeOut.success(SEmpty) // TODO : Timeout should allow the loop to finish its iteration.
      // So that if the loop is found, it will finish to compute it.
      ifTimeOut = promise[STraceExpr]
    }
  }
  private var ifTimeOut = promise[STraceExpr]
  
  

  
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
    val dotsPositions = "\\.\\.\\.".r.findAllMatchIn(s).map(mt => mt.start(0)).toSet
    
    /**
     * Heuristict to deal with substrings of longest size first.
     */
    val longestSizeFirst = (ij : (Int, Int)) => ij._1 - ij._2
    var interestingEdges1 = (for(
      i <- 0 until s.length;
      j <- (i+1) to s.length;
      σvi <- σ.inputs
      if σvi.indexOf(s.substring(i, j)) >= 0) yield (i, j)).toSet
    // Removes edges which belong to a greater substring if they are not themselves longest numbers
    //if(verbose) println(s"found ${interestingEdges1.size} substrings:\n"+(interestingEdges1 map { case (i, j) => s"[$i,$j]"+s.substring(i, j)}))
   
    val longestInterestingEdges = ((interestingEdges1.toList filterNot {
      case (i, j) =>
        val isNumber = s.substring(i, j).isNumber
        ((((interestingEdges1 contains ((i, j+1))) && (!dotsPositions(j))) && (isNumber implies s(j).isDigit)) ||
         ((interestingEdges1 contains ((i-1, j))) && (isNumber implies s(i-1).isDigit)))
    }))sortBy(longestSizeFirst)
     
    if(verbose) println(s"found ${longestInterestingEdges.length} longest substrings:\n"+(longestInterestingEdges map { case (i, j) => s"[$i,$j]"+s.substring(i, j)}))
    val remaining_edges = ξ.filter{ case (i,j ) => !longestInterestingEdges.contains((i, j)) }.toList.sortBy(longestSizeFirst)
    
    /**
     * Heuristic to compute separators start positions.
     * Takes the positions having the same chars before the dots
     */
    val presentSeparators = s.toList.zipWithIndex.flatMap{ case (char, i) => if(ProgramSet.isCommonSeparator(char.toString)) List(i) else Nil}
    val preferredStart = longestInterestingEdges.map(_1).toSet ++ presentSeparators
    val preferredEnd = longestInterestingEdges.map(_2).toSet ++ presentSeparators

    var preferredSeparatorStart: Set[Int] = presentSeparators.toSet
    var preferredSeparatorEnd: Set[Int] = preferredSeparatorStart.map(_+1)
    
    if(!dotsPositions.isEmpty) {
      val middleCandidates = (for(dpos <- dotsPositions;
          i <- (dpos-1) to 0 by -1;
          ss = s.substring(i, dpos);
          endpos <- Pattern.quote(ss).r.findAllMatchIn(s).map(_.end(0))
          if(endpos != dpos)
      ) yield endpos)
        .groupBy(i => i).toList
        .sortBy{ case (key, value) => -value.size }
        .map(_._1)
      if(middleCandidates.nonEmpty) {
        preferredSeparatorEnd = middleCandidates.toSet
        preferredSeparatorStart = preferredSeparatorEnd ++ preferredSeparatorEnd.map(_ - 1)
      }
    }
    
    for((i, j) <- longestInterestingEdges.toIterable ++ remaining_edges if !timeout && !mTimeoutPhaseGenerateStr) {
      W += (i,j) -> (W.getOrElse((i, j), Set()) ++ (generateSubString(σ, s.substring(i, j), i)))
    }
    if(timeout && verbose) println("exited loop of generateStr because timed out")

    val previous = SDag(ñ, ns, nt, ξ, W): STraceExpr
    
    val Wp =  generateLoop(σ, s, W, rec_loop_level)(
        previous, preferredStart=preferredStart++preferredSeparatorStart, preferredSeparatorStart=preferredSeparatorStart, preferredEnd = preferredEnd++preferredSeparatorEnd, preferredSeparatorEnd=preferredSeparatorEnd)
    SDag(ñ, ns, nt, ξ, Wp).setIndex(σ.position): STraceExpr
  }
  
  /**
   * Specializes a DAG by removing all positions except those between k1 and k2. Removes all corresponding edges
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
   * Specializes a DAG by removing all edges except the one between k1 and k2 and other local positions
   */
  def extractDag(dag: STraceExpr, k1: Int, k2: Int, notablePositions: Set[Int], orElse: => STraceExpr): STraceExpr = dag match {
    case SDag(ñ, ns: Int, nt, ξ, a) =>
      val x = ξ.asInstanceOf[Set[(Int, Int)]]
      val nn = ñ.asInstanceOf[Set[Int]]
      val aa = a.asInstanceOf[W[Int]]
      def ok1(i: Int): Boolean = k1 == i  || notablePositions(i)
      def ok2(i: Int): Boolean = i == k2  || notablePositions(i)
      def ok3(ij: (Int, Int)): Boolean = ok1(ij._1) && ok2(ij._2) && ij._1 >= k1 && ij._2 <= k2
      SDag(Set(k1, k2)++notablePositions, k1, k2, x.filter(ok3), aa)
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
  def generateLoop(σ: Input_state, s: Output_state, W: W[Int], rec_loop_level: Int)
                  (current: STraceExpr, preferredStart: Set[Int], preferredSeparatorStart: Set[Int], preferredEnd: Set[Int], preferredSeparatorEnd: Set[Int]): W[Int] = {
    if(rec_loop_level <= 0) return W
    if(verbose) println(s"Looking for loops for $σ => $s")
    //var WpLite = W // Create a copy?
    var Wp = W // Do not create a copy
    val LITE = 0
    val FULL = 1
    
    val preferredPositions = preferredStart ++ preferredEnd
    if(verbose) {
      println(s"preferredPositions = $preferredPositions")
    }
    
    val w = Identifier(if(w_id <= 25) ('a' + w_id.toChar).toChar.toString else "w" + (w_id-25)); w_id += 1
    val positionToCheckEnd: (Int, Int) => Boolean = new ((Int, Int) => Boolean) {
        val l = ScalaRegExp.convertToken(LowerTok)
        val u = ScalaRegExp.convertToken(UpperTok)
        val n = ScalaRegExp.convertToken(NumTok)
        val w = ScalaRegExp.convertToken(SpaceTok)
        val acceptable: Set[Int] = s"[^a-zA-Z0-9 ]|$l|$u|$n|$w".r.findAllMatchIn(s).toSet.flatMap{(m: Regex.Match) =>Set(m.end(0))}
        def apply(i: Int, liteOrFull: Int) = if(onlyInterestingPositions) {
          acceptable(i)
        } else if(liteOrFull == LITE) acceptable(i) else true
      }
    val positionToCheckStart: (Int, Int) => Boolean = new ((Int, Int) => Boolean) {
        val l = ScalaRegExp.convertToken(LowerTok)
        val u = ScalaRegExp.convertToken(UpperTok)
        val n = ScalaRegExp.convertToken(NumTok)
        val w = ScalaRegExp.convertToken(SpaceTok)
        val acceptable: Set[Int] = s"[^a-zA-Z0-9 ]|$l|$u|$n|$w".r.findAllMatchIn(s).toSet.flatMap{(m: Regex.Match) =>Set(m.start(0))}
        def apply(i: Int, liteOrFull: Int) = if(onlyInterestingPositions) {
          acceptable(i)
        } else if(liteOrFull == LITE) acceptable(i) else true
      }
    
    def subDag(k1: Int, k2: Int, liteOrFull: Int): STraceExpr = {
      if(liteOrFull == LITE) {
        current match {
          case sd: SDag[_] =>
            extractDag(current, k1, k2, preferredPositions, SEmpty)
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
    def preferredSeparatorStartFirst(i: Iterable[Int], liteOrFull: Int): Iterable[Int] = {
      i.filter(preferredSeparatorStart) ++ (if(liteOrFull == LITE) Nil else i.filterNot(preferredSeparatorStart))
    }
    def preferredStartFirst(i: Iterable[Int], liteOrFull: Int): Iterable[Int] = {
      i.filter(preferredStart) ++ (if(liteOrFull == LITE) Nil else i.filterNot(preferredStart))
    }
    def preferredSeparatorEndFirst(i: Iterable[Int], liteOrFull: Int): Iterable[Int] = {
      i.filter(preferredSeparatorEnd) ++ (if(liteOrFull == LITE) Nil else i.filterNot(preferredSeparatorEnd))
    }
    def preferredEndFirst(i: Iterable[Int], liteOrFull: Int): Iterable[Int] = {
      i.filter(preferredEnd) ++ (if(liteOrFull == LITE) Nil else i.filterNot(preferredEnd))
    }
    
    // Priority if dots found in string.
   def endingRange(liteOrFull: Int): Iterable[Int] = if(useDots) { s.indexOf("...") match {
        case -1 => Range(2, s.length+1) // Nothing can be done.
        case k3 => k3 :: preferredEndFirst((Range(2, s.length+1).toList.filterNot(_ == k3)), liteOrFull).toList
      }
    } else Range(2, s.length+1)

    // Two loops versions, one with lite loops (no more than 1 expression in the loop)
    // the other allows more expressions.
    if(verbose) println(s"Acceptable starts:"+preferredStart.toList.sortBy(i=>i))
    if(verbose) println(s"Acceptable sep starts:"+preferredSeparatorStart.toList.sortBy(i=>i))
    if(verbose) println(s"Acceptable ends:"+preferredEnd.toList.sortBy(i=>i))
    if(verbose) println(s"Acceptable sep ends:"+preferredSeparatorEnd.toList.sortBy(i=>i))
    for(liteOrFull <- (LITE to FULL).view;
        //dummy = (if(verbose) println(s"Looping ${if(liteOrFull == LITE) "LITE" else "FULL"}") else ());
        k3_range = endingRange(liteOrFull).filter(positionToCheckEnd(_, liteOrFull));
        //dummy2 = (if(verbose) println(s"k3 range: $k3_range") else ());
        k3 <- k3_range.view;
        //dummy4 = (if(verbose) println(s"k3 =: $k3") else ());
        ksep_range = preferredSeparatorEndFirst(k3-1 to 1 by -1, liteOrFull).filter(positionToCheckStart(_, liteOrFull));
        //dummy3 = (if(verbose) println(s"ksep range: $ksep_range") else ());
        ksep <- ksep_range.view;
        //dummy5 = (if(verbose) println(s"ksep $ksep") else ());
        e2 = subDag(ksep, k3, liteOrFull);
        k2_range = preferredSeparatorStartFirst(ksep to (ksep - MAX_SEPARATOR_LENGTH) by -1, FULL);
        //dummy7 = (if(verbose) println(s"k2_range $k2_range") else ());
        k2 <- k2_range.view;
        optionSeparator = if(ksep > k2) Some(ConstStr(s.substring(k2, ksep))) else None;
        //dummy6 = (if(verbose) println(s"k2 $k2") else ());
        if(k2 == ksep || ProgramSet.isCommonSeparator(optionSeparator.get.s));
        k1_range =  preferredStartFirst(k2-1 to 0 by -1, liteOrFull).filter(positionToCheckStart(_, liteOrFull));
        //dummy8 = (if(verbose) println(s"k1_range $k1_range") else ());
        k1 <- k1_range.view;
        //dummy9 = (if(verbose) println(s"k1 $k1") else ());
        e1 = subDag(k1, k2, liteOrFull)) {
      if(timeout) {if(verbose) println("exited loop of generateLoop because timed out"); return Wp }
      if(verbose) println(s"Going to unify '${s.substring(k1, k2)}' and '${s.substring(ksep, k3)}' separated by '${s.substring(k2, ksep)}'")
      val (e, time) = timedScope(if(liteOrFull == LITE) {
        unify(e1, e2, w, σ.position, σ.position, iterateInput)  // If unify results only in constants
      } else {
        // If full, can take much more time per unification.
        val res = future{unify(e1, e2, w, σ.position, σ.position, iterateInput)}
        Await.result(first(res, ifTimeOut.future), 10.days) 
      })
      stats_unifications += 1
      stats_time_unifications += time
      if(sizePrograms(e) != 0) {
        // Loops to find other data on the left
        val bestLoop =  e.takeBest
        if(bestLoop.uses(w)) {
          var stop = false
          val prog = Loop(w, bestLoop, optionSeparator)
          var i = 0
          while(!stop && (i == 0 || useDots && dotsAtPosition(s, k3))) {
            Loop.setStartIndex(prog, i)
            val resulting_strings = 
              Evaluator.evalProg(prog)(σ) match {
              case StringValue(p) if p != "" => Some(p)
              case _ => stop = true; None
            }
            val body = if(i != 0) {replaceSTraceExpr(e)(
              { case l@ Linear(a, v, b) => if(w == v && a >= 0) {
                Linear(a, v, b + a*i)  // Try with a previous step
              } else l
              }
            )} else e
            val newLoop = SLoop(w, body, optionSeparator)
            resulting_strings match {
              case Some(res) =>
                val start: Int = if(i == 0) k1 else {
                  // First matching occurence given that dots do not count.
                  val firstOccurrence = s.indexOf(res)
                  if(firstOccurrence != -1) {
                    firstOccurrence
                  } else if(useDots && s.indexOf(dots) != -1) { // Find a match until three dots
                    (0 until s.length) find {
                      case i =>
                        val dotsafterI = s.indexOf(dots, i)
                        if(dotsafterI != -1) {
                          res.startsWith(s.substring(i, dotsafterI))
                        } else {
                          false
                        }
                    } match {
                      case Some(i) => i
                      case None => stop = true; s.length + 1
                    }
                  } else {
                    stop = true; s.length + 1// nothing to find.
                  }
                }
                val k4 = start + res.length 
                if(k4 <= s.length && s.substring(start, k4) == res) { // The match is exact  && res.length > k3 - k1 || useDot
                  val matchingDots = useDots && k4 < s.length && dotsAtPosition(s, k4)
                  if(matchingDots || start < k1 || k4 > k3) {
                    Wp = Wp + (((start, k4))->(Wp((start, k4)) ++ Set(SLoop(w, e, optionSeparator))))
                    if(matchingDots) { // If dots, then the match can be extended after the dotS.
                      Wp = Wp + (((start, k4+dots.length))->(Wp((start, k4+dots.length)) ++ Set(SLoop(w, e, optionSeparator))))
                      if(verbose) println(s"Found dotted loop in ${s} (returns $res) [${Printer(newLoop.takeBest)}]")
                    } else {
                      if(verbose) println(s"Found loop in ${s} (returns $res) [${Printer(newLoop.takeBest)}] weight=${Weights.weight(newLoop.takeBest)}")
                    }
                  }
                  // Checks if the match can be extended on the left (i.e. by changing the counters offset by -1)
                  
                  
                } else if(useDots) { // If we use dots '...' to match the remaining.
                  val positionNotMatch: Option[Int] = (start until k4) find { k => k < s.length && s(k) != res(k-start) }
                  positionNotMatch match {
                    case Some(p) if s(p) == dots(0) =>
                      if(dotsAtPosition(s, p)) {
                        Wp = Wp + (((start, p+dots.length))->(Wp((start, p+dots.length)) ++ Set(newLoop)))
                        if(verbose) println(s"Found dotted loop in ${s} (returns $res) [${Printer(newLoop.takeBest)}]")
                      }
                    case _ =>
                  }
                }
              case _ =>
            }
            i = i -1
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
  def generateSubString(σ: Input_state, s: String, pos: Int = -1): Set[SAtomicExpr] = cached((σ, s), cacheGenerateSubstring){
    var result = Set.empty[SAtomicExpr]
    if(!extractSpaces && s == " ") return result
    //if(verbose) println(s"Going to extract $s from $σ")
    for(vi <- 0 until σ.inputs.length if !timeoutGenerateStr) {
      val σvi =  σ.inputs(vi)
      for((k, m) <- s substringWithCaseOf σvi if !timeoutGenerateStr) {
        val Y1 = generatePosition(σvi, k)
        val Y2 = generatePosition(σvi, k + s.length)
        
        /*if(debugActive) {
          for(y1 <- Y1; y <- y1) {
            assert(evalProg(y)(Input_state(IndexedSeq(σvi), 1)) == IntValue(k))
          }
          for(y2 <- Y2; y <- y2) {
            assert(evalProg(y)(Input_state(IndexedSeq(σvi), 1)) == IntValue(k + s.length))          
          }
        }*/

        val newResult = SSubStr(InputString(vi), Y1, Y2, m)
        newResult.setPos(σvi, s, k, k + s.length)
        newResult.weightMalus = if(k == pos) {-1} else {0}
        result = result + newResult
      }
      for(extension <- extensions; (start, end, programMaker) <- extension(σvi, s)) {
        val Y1 = generatePosition(σvi, start)
        val Y2 = generatePosition(σvi, end)
        val ss = SSubStr(InputString(vi), Y1, Y2, SSubStrFlag(List(NORMAL)))
        ss.setPos(σvi, s, start, end)
        val program = programMaker(ss)
        result += program
      }
      // TODO : Put this into an extension.
      if(s.isNumber && numbering) {
        for((start, end, offset) <- s subnumberIncNegativeOf σvi) { // Numbers that can be obtained from σvi by changing by steps for example.
          val Y1 = generatePosition(σvi, start)
          val Y2 = generatePosition(σvi, end+1)
          val possibleLengths = (if(s(0) != '0') {// It means that the generated length might be lower.
            SIntSemiLinearSet(1, s.length-1, s.length)
          } else SIntSemiLinearSet(s.length, 1, s.length))
          if(!possibleLengths.isEmpty)
          result = result + SNumber(SSubStr(InputString(vi), Y1, Y2, SSubStrFlag(List(NORMAL))), possibleLengths, offset)
        }
      }
    }
    // Generates numbers from previous numbers in output strings.
    if(s.isNumber && numbering) {
      result += SCounter.fromExample(s, σ.position)
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
    if(verbose) println(s"Compute token seq for " + s.substring(0, Math.min(s.length, 10)) + "...")
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
             //val ss = s.substring(start)
             val closestEnd = ScalaRegExp.computeFirstPositionEndingWith(tok, s, start)
             (tok, start + closestEnd.get)} toMap)
    
    // enumerate tokens sequence of length 0
    //val epsilonRange = (finalstart to finalend).toList zip (-1 until finalend).toList
    for(i <- finalstart to (finalend+1)) {
      addMapping(i, i-1, TokenSeq(), ((finalstart to (finalend+1)).toList, (-1 to finalend).toList))
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
    cacheComputeTokenSeq = res
    computedForString = s
    computedForList = listTokens
    if(verbose) println(s"Computation finished")
    res
  }

  /**
   * Returns a list of (Start, End) for tokens matching at the position, with the common index)
   */
  def matchingTokenSeq(s: String, atPos: Int, listTokens: List[Token]=Program.listNonEmptyTokens)
      : Iterable[(Start, End, TokenSeq, TokenSeq, List[Index])] = {
    val ms = computetokenSeq(s, listTokens)
    for(i <- atPos to 0 by -1;
        j <- (atPos-1) until s.length;
        //if(i != atPos || j != atPos -1); // No doubly empty regexps.
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
      //if(verbose) println("Cache hit")
      cache_hit += 1
      if(advanced_stats) advanced_cache = advanced_cache + (s -> (advanced_cache.getOrElse(s, 0) + 1))
    }
    cache.getOrElseUpdate(s, f)
  }
  /**
   * Generates a set of algebraic positions for a given position and a string.
   */
  implicit val cache = MMap[(String, Int), Set[SPosition]]()
  def generatePosition(σ: String, k: Int) = cached((σ, k), cache){
    if(verbose) println(s"Generating position $k in $σ")
    var result = Set[SPosition](SCPos(k), SCPos(-σ.length+k-1))
    implicit val (tokenSet, mapping) = Reps(σ)
    for((_, _, r1@TokenSeq(t_list1), r2@TokenSeq(t_list2), intersections) <- matchingTokenSeq(σ, atPos=k, listTokens=tokenSet)) {   
        val c = intersections.indexOf(k)
        //println(Printer(r1) + " before, " + Printer(r2) + " after")
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
    val listTokens = Program.listNonEmptyTokens
    val res: Map[Token,List[Token]] = listTokens.map (tok => (tok, ScalaRegExp.computePositionsOfToken(tok, s)))
      .groupBy( t => t._2)
      .mapValues(t => t map (_._1))
      .values
      .map(t => (t.head, t))
      .toMap
    res
  }
  def IParts(ss: String, t: Token)(implicit map: Map[Token,List[Token]] = IPart_s(ss)): SToken = if(t == StartTok || t == EndTok) SToken(Set(t))(Program.listTokens) else SToken(map(t).toSet)(Program.listTokens)
  
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