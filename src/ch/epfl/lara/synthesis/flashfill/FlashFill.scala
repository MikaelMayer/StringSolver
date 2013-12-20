package ch.epfl.lara.synthesis.flashfill

import scala.collection.mutable.{HashMap => MMap}
import java.util.regex.Pattern

/**FlashFill
 * 
 */
object FlashFill {
  import Programs._  
  import ProgramsSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  final val debugActive = true
  def debug(s: String) = if(debugActive) println(s)
  
  def apply(): FlashFillSolver = new FlashFillSolver()
  
  def apply(input: List[List[String]], output: List[String]): Option[Program] = {
    val solver = apply()
    (input zip output) foreach { case (i, o) =>
      solver.add(i, o)
    }
    solver.solve()
  }
  
  /**
   * Instance solving the problem iteratively
   */
  class FlashFillSolver {
    private var ff = new FlashFill()
    
    var inputList = List[List[String]]()
    var outputList = List[String]()
    var currentPrograms: STraceExpr = null
    var previousOutput = ""
      
    /**
     * Set dots option
     */
    def setUseDots(b: Boolean) = ff.useDots = b
    
    /**
     * Set numbering option
     */
    def setUseNumbers(b: Boolean) = ff.numbering = b
    
    /**
     * Set if use loops
     */
    def setLoopLevel(i: Int) = ff.DEFAULT_REC_LOOP_LEVEL = i
      
    /**Adds a new input/output example*/
    def add(input: Seq[String], output: String): STraceExpr = {
      val ii = if(ff.numbering)  (input.toList ++ List(previousOutput.numbers)) else input.toList
      
      inputList = inputList ++ List(ii)
      outputList = outputList ++ List(output)
      
      
      val iv: Array[String] = ii.toArray
      val newProgramSet = ff.generateStr(iv, output, ff.DEFAULT_REC_LOOP_LEVEL)
    
      previousOutput = output
      if(currentPrograms == null) {
        currentPrograms = newProgramSet
      } else {
        val intersection = intersect(currentPrograms, newProgramSet) 
        currentPrograms = intersection
      }
      verifyCurrentState()
      newProgramSet
    }
    /** Returns the best solution to the problem */
    def solve(): Option[Program] = if(currentPrograms != null) try {
      val res = currentPrograms.takeBest
      verifyCurrentState()
      Some(res)
    } catch {
      case _: Exception => None
    } else None
    
    def verifyCurrentState() = {
      if(size(currentPrograms) != 0) {
        val prog = currentPrograms.takeBest
          for((inputs, output) <- (inputList zip outputList)) {
            evalProg(prog)(inputs.toArray: Array[String]) match {
              case StringValue(res) =>
                if(ff.useDots) {
                  val reg = output.split("\\Q...\\E").map(Pattern.quote(_)).mkString(".*").r.anchored
                  assert(reg.findFirstIn(res) != None)
                } else {
                  assert(res == output)
                }
              case ⊥ => assert(⊥ == output)
              case _ =>
            }
          }
        }
    }
  }
}

class FlashFill {
  import Programs._  
  import ProgramsSet._
  import Evaluator._
  import Implicits._
  import scala.language._
  import FlashFill._
  
  // Parameter: Are we using dots to describe not finished loops.
  var useDots = true
  // Adds the last output as the last input for the next program.
  var numbering = true
  
  final val dots = "..."
  
  
  private type σ = IndexedSeq[String]
  private type S = String
  private type Output_string = String
  private type Input_state = σ
  private type Regular_Expression = RegExp
  private type W[Node] = Map[(Node, Node), Set[SAtomicExpr]]
  private type Start = Int
  private type End = Int
  private type Index = Int
  
  var DEFAULT_REC_LOOP_LEVEL = 1
 
  
  /**synthesis algorithm*/
  def generateStringProgram(S: Set[(σ, S)]) = {
    var T = Set.empty[(Set[σ], STraceExpr)]
    for((σ, s) <- S)
      T = T + ((Set(σ),generateStr(σ, s, DEFAULT_REC_LOOP_LEVEL))) 
    T = generatePartition(T)
    val σp = for((σ, s) <- S) yield σ
    var B = Map[Set[σ], Bool]()
    
    for((σt, et) <- T)
      B += σt -> generateBoolClassifier(σt, σp -- σt)
    
    val P  = T.toList.sortBy{case (σi, ei) => size(ei)} map
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
  implicit val cacheGenerateStr = MMap[(Input_state, Output_string, Int), STraceExpr]()
  def generateStr = cached((σ: Input_state, s: Output_string, rec_loop_level: Int) => {
    //debug(s"generateStr on: $σ, $s and $rec_loop_level")
    val ñ = (0 to s.length).toSet
    val ns = 0
    val nt = s.length
    val ξ = (for(i <- 0 until s.length; j <- (i+1) to s.length) yield i->j).toSet
    val W = ξ ==> { case (i, j) =>
      generateSubString(σ, s.e(i, j-1)) ++ Set(SConstStr(s.e(i, j-1)))
    }
    val Wp =  generateLoop(σ, s, W, rec_loop_level)
    SDag(ñ, ns, nt, ξ, Wp): STraceExpr
  })
  
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
  var w_id = 1
  def generateLoop(σ: Input_state, s: Output_string, W: W[Int], rec_loop_level: Int): W[Int] = {
    if(rec_loop_level <= 0) return W
    debug(s"Find loop for $σ => $s")
    var Wp = W // Create a copy?
    val w = Identifier("w" + w_id); w_id += 1
    for(k1 <- 0 until s.length;
        k2 <- (k1+1) until s.length;
        e1 <- Some(generateStr(σ, s.substring(k1, k2), rec_loop_level - 1));
        k3 <- (k2+1) until s.length;
        e2 <- Some(generateStr(σ, s.substring(k2, k3), rec_loop_level - 1))
        ) {
      //val e1 = generateStr(σ, s.substring(k1, k2), rec_loop_level - 1)
      //val e2 = generateStr(σ, s.substring(k2, k3), rec_loop_level - 1)
      //println(s"Unifying ${s.substring(k1, k2)} of size ${size(e1)} and ${s.substring(k2, k3)} of size ${size(e2)}...")
      val e = unify(e1, e2, w)  // If unify results only in constants
      if(size(e) != 0) {
        val bestLoop =  e.takeBest
        val prog = Loop(w, bestLoop)
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
                Wp = Wp + (((k1, k4+1))->(Wp((k1, k4+1)) ++ Set(SLoop(w, e))))
              } else if(useDots) { // If we use dots '...' to match the remaining.
                val positionNotMatch = (k1 until k4) find { k => k<s.length && s(k) != res(k-k1) }
                positionNotMatch match {
                  case Some(p) if s(p) == dots(0) =>
                    if(p + dots.length <= s.length && s.substring(p, p + dots.length) == dots) {
                      Wp = Wp + (((k1, p+dots.length))->(Wp((k1, p+dots.length)) ++ Set(SLoop(w, e))))
                      debug(s"Found dotted loop: $s (returns $res)")
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
  def generateSubString = cached((σ: Input_state, s: String) => {
    var result = Set.empty[SAtomicExpr]
    for(vi <- 0 until σ.length) {
      val σvi = σ(vi)
      for(k <- s substringOf σvi) {
        val Y1 = generatePosition(σvi, k)
        val Y2 = generatePosition(σvi, k + s.length)
        
        for(y1 <- Y1; y <- y1) {
          assert(evalProg(y)(Array(σvi)) == IntValue(k))
        }
        for(y2 <- Y2; y <- y2) {
          assert(evalProg(y)(Array(σvi)) == IntValue(k + s.length))
        }

        val newResult = SSubStr(InputString(vi), Y1, Y2)
        result = result + newResult
      }
      // If σvi is empty or does not contain any number, it should generate all possible numbers expressions for various sizes.
      if(s.isNumber) {
        if(σvi.isEmpty()) {
          val i = s.toInt
          // For any number i, the set of matching counters are IntLiteral(i), Linear(1, i), Linear(2, i) up to Linear(i, i)
          result = result + SNumber(SAny(InputString(vi)), Set(s.length), SIntSet(Set(i)), SAnyInt(1))
        } else {
          for((start, end, steps) <- s subnumberOf σvi) { // Numbers that can be obtained from σvi by incrementing by steps for example.
            val Y1 = generatePosition(σvi, start)
            val Y2 = generatePosition(σvi, end+1)
            result = result + SNumber(SSubStr(InputString(vi), Y1, Y2), Set(s.length), SAnyInt(0), SIntSet(Set(steps)))
          }
        }
      }
    }
    result
  })
  
  /**
   * Compute the set of all tokenseq between two given positions.
   */

  def computetokenSeq(s: String, listTokens: List[Token]): Map[(Start, End), Set[(TokenSeq, List[(Index, Index)])]] = {
    val finalstart = 0
    val finalend = s.length-1
    var res = Map[(Start, End), Set[(TokenSeq, List[(Index, Index)])]]()
    def addMapping(i: Start, j: End, s: TokenSeq, index: List[(Index, Index)]) = res += (i, j) -> (res.getOrElse((i, j), Set()) + ((s, index)))
    val tokenPositions = (listTokens map { token =>
      token -> ScalaRegExp.computePositionsOfToken(token, s)
    }) toMap
    // Maps a position to a set of tokens with indexes
    val startTokens: Map[Start,List[(Token, List[(Index, Index)])]] = tokenPositions.toList
       .flatMap{ case (tok, listpos) => listpos map(el => (tok, el._1, el._2))}
       .groupBy{case (tok, start, end) => start}
       .mapValues(list => list map { case (tok, start, index) => (tok, tokenPositions(tok)) })
    
    // Maps a position and a token to its end position
    val endTokensFromStart: Map[Start, Map[Token, End]] =
      tokenPositions.toList
       .flatMap{ case (tok, listpos) => listpos map(el => (tok, el._1, el._2))}
       .groupBy{case (tok, start, end) => start}
       .mapValues(list => list map { case (tok, start, end) => (tok, end)} toMap)
    
    // enumerate tokens sequence of length 0
    val epsilonRange = (finalstart to finalend).toList zip (-1 until finalend).toList
    for(i <- finalstart to finalend) {
      addMapping(i, i-1, TokenSeq(), epsilonRange)
    }
    // enumerate tokens sequence of length 1
    //addMapping(finalstart, finalstart-1, TokenSeq(StartTok)) // Simple token starting at 0
    //addMapping(finalend+1, finalend, TokenSeq(EndTok)) // Simple token starting at 0
    for(i <- finalstart to finalend) {
      for((tok, index) <- startTokens.getOrElse(i, Nil) if tok != StartTok && tok != EndTok) { // But can be empty
        val end = endTokensFromStart(i)(tok)
        if(end >= i)
        addMapping(i, end, TokenSeq(tok), index) // Simple token starting at 0
      }
    }
    // enumerate tokens sequences of two tokens.
    val currentMapping = res
    for(((start, end), tokseqset) <- currentMapping; (tokseq, index) <- tokseqset if tokseq.t.size == 1 && end != finalend) {
      val contiguousTokens = startTokens(end + 1)
      for((tok2, index2) <- contiguousTokens) {
        val endnew = endTokensFromStart(end + 1)(tok2)
        val newTokenseq = TokenSeq(tokseq.t ++ List(tok2))
        addMapping(start, endnew, newTokenseq, ScalaRegExp.computePositionsOfRegExp(newTokenseq, s))
      }
    }
    // enumerate tokens sequences of three tokens.
    val currentMapping2 = res
    for(((start, end), tokseqset) <- currentMapping2; (tokseq, index) <- tokseqset if tokseq.t.size == 2 && end != finalend) {
      val contiguousTokens = startTokens(end + 1)
      for((tok2, index2) <- contiguousTokens) {
        val endnew = endTokensFromStart(end + 1)(tok2)
        val newTokenseq = TokenSeq(tokseq.t ++ List(tok2))
        addMapping(start, endnew, newTokenseq,  ScalaRegExp.computePositionsOfRegExp(newTokenseq, s))
      }
    }
    // Add startoken and endtoken if it is related.
    val currentMapping3 = res
    for(((start, end), tokseqset) <- currentMapping2; (tokseq, index) <- tokseqset) {
      if(start == finalstart) {
        addMapping(start, end, TokenSeq(StartTok::tokseq.t), List(index.head))
      }
      if(start == finalend) {
        addMapping(start, end, TokenSeq(tokseq.t ++ List(EndTok)), List(index.reverse.head))
      }
    }
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
  def matchingTokenSeq(s: String, atPos: Int, listTokens: List[Token]=Programs.listTokens)
      : Iterable[(Start, End, TokenSeq, TokenSeq, List[Index])] = {
    val ms = computetokenSeq(s, listTokens)
    for(i <- (atPos-1) to 0 by -1;
        j <- atPos until s.length;
        (tok1, indexes1) <- ms.getOrElse((i, atPos-1), Set());
        (tok2, indexes2) <- ms.getOrElse((atPos, j), Set())) yield {
      // TODO : Add a sorted intersect function.
          val intersections = (indexes1 map {case (start, end) =>  end + 1}) intersect (indexes2 map { case (start, end) => start })
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
  
  /**
   * Initialize the statistics
   */
  def initStats() = { cache_hit = 0; cache_call = 0 }
  
  /**
   * Generate the set of positions describing position k in string.
   */
  def cached[U, T](f: U => T)(implicit cache: MMap[U, T]): U => T = u => {
    cache_call += 1
    if(cache contains u) cache_hit += 1
    cache.getOrElseUpdate(u, f(u))
  }
  def cached[U, V, T](f: (U, V) => T)(implicit cache: MMap[(U, V), T]): (U, V) => T = (u, v) => {
    cache_call += 1
    if(cache contains (u, v)) cache_hit += 1
    cache.getOrElseUpdate((u, v), f(u, v))
  }
  def cached[U, V, W, T](f: (U, V, W) => T)(implicit cache: MMap[(U, V, W), T]): (U, V, W) => T = (u, v, w) => {
    cache_call += 1
    if(cache contains (u, v, w)) cache_hit += 1
    cache.getOrElseUpdate((u, v, w), f(u, v, w))
  }
  
  /**
   * Generates a set of algebraic positions for a given position and a string.
   */
  implicit val cache = MMap[(String, Int), Set[SPosition]]()
  def generatePosition = cached((s: String, k: Int) => {
    var result = Set[SPosition](SCPos(k), SCPos(-s.length+k-1))
    val tokenSet = Reps(s)
    //for((k1, r1@TokenSeq(t_list1), index) <- matchingTokenSeq(s, atPos=k-1, before, listTokens=Reps(s))) {
    //  for((k2, r2@TokenSeq(t_list2), index2) <- matchingTokenSeq(s, atPos=k, after, listTokens=Reps(s))) {
        //val r12 = TokenSeq(t_list1 ++ t_list2) // TODO: Merging does not work here. If NonDotToken before the end, then merging with non dot on the right does not work.
    for((k1, k2, r1@TokenSeq(t_list1), r2@TokenSeq(t_list2), intersections) <- matchingTokenSeq(s, atPos=k, listTokens=Reps(s))) {   
        val c = intersections.indexOf(k)
        if( c >= 0) { // This if false for strange tokenizations.
          //val c = th_match_of(k1, _for=r12, in=s)
          val cp = intersections.length
          //val cp = total_number_of_matches(_for=r12, in=s)
          assert(cp >= 1)
          val r1p = generateRegex(r1, s) // Expand all tokens.
          val r2p = generateRegex(r2, s)
          val res = SPos(r1p, r2p, Set(c + 1, -(cp-c)))
          result += res
        }
    }
    result
  })
  
  def generateRegex(r: Regular_Expression, s: String): SRegExp = {
    r match {
      case TokenSeq(l) =>
        STokenSeq(l map ((t: Token) => IParts(s, t)))
    }
  }
  
  /**
   * Creates the equivalence class of a token.
   */
  implicit val cacheIParts = MMap[(String, List[Token]), Map[Token,List[Token]]]()
  def IPart_s = cached((s: String, listTokens: List[Token]) => {
    val res = listTokens.map (tok => (tok, ScalaRegExp.computePositionsOfToken(tok, s)))
      .groupBy( t => t._2)
      .mapValues(t => t map (_._1))
      .values
      .map(t => (t.head, t))
      .toMap
    res
  })
  def IParts(s: String, t: Token) = if(t == StartTok || t == EndTok) SToken(Set(t)) else SToken(IPart_s(s, Programs.listTokens)(t).toSet)
  def Reps(s: String): List[Token] = IPart_s(s, Programs.listTokens)
    .values.toList
    .map(t => t.head)
  
    
  def outputStats() = {
    println("Number of elements in the cache 1:" + cacheIParts.size)
    println("Number of elements in the cache 2:" + cacheGenerateStr.size)
    println("Number of elements in the cache 3:" + cacheGenerateSubstring.size)
    println("Number of elements in the cache 4:" + cache.size)
    println("Number of cache reuse:" + cache_hit + "/" + cache_call)
  }
  
  /**
   * Empty the caches.
   */
  def emptyCaches() = {
    cacheIParts.clear()
    cacheGenerateStr.clear()
    cacheGenerateSubstring.clear()
    cache.clear()
  }
  
  
  
}