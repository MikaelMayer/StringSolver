package ch.epfl.lara.synthesis.flashfill

import ch.epfl.lara.synthesis.flashfill.Programs._
import scala.Char

/**
 * Automata to proceed with regular expressions
 */
object Automata extends ComputePositionsInString { automataobject =>
  type EdgeLabel[Alphabet] = List[(Alphabet, Alphabet)]
  
    
  /**
   * Computes the list of positions where there exists a word recognized by this regexp.
   */
  def computePositionsEndingWith(r: RegExp, s: String): List[Int] = {
    val dfa = convertRegExp(r)
    dfa.recordFinalStates(s)
  }

  
  
  //type Label = EdgeLabel[Char]
  
  trait Label[Self <: Label[Self, Alphabet], Alphabet] extends {
    def ||(other: Self): Self
    def &&(other: Self): Self
    def unary_! : Self
    def accept(s: Alphabet): Boolean
    def isEmpty(): Boolean
    def isEverything(): Boolean
    def mostGeneralLabel: Self
  }
  
  /**
   * Assertion to check that labels are disjoint and/or complete.
   */
  def assertDisjoints[T <: Label[T, _]](labels : List[T]): Unit = {
    for(i <- 0 until labels.size; j <-(i+1) until labels.size) {
      assert((labels(i) && labels(j)).isEmpty)
    }
  }
  def assertComplete[T <: Label[T, _]](labels: List[T]): Unit = {
    assert(labels.reduce(_ || _).isEverything)
  }
  
  def char2string(c: Char) = {
    if(c >= 33 && c <= 127) {
      "'" + c.toString + "'"
    } else if (c == Char.MaxValue){
      "Char.MaxValue"
    } else c.toInt + ".toChar"
  }
  
  object CharLabel {
    def apply(input: (Char, Char)*): CharLabel = CharLabel(input.toList)
  }
  case class CharLabel(val e: EdgeLabel[Char]) extends Label[CharLabel, Char] {
    def ||(other: CharLabel): CharLabel = new CharLabel(automataobject.union(e, other.e))
    def &&(other: CharLabel): CharLabel = new CharLabel(automataobject.inter(e, other.e))
    def unary_! : CharLabel = new CharLabel(automataobject.not(e))
    def accept(s: Char): Boolean = e exists { tuple => tuple._1 <= s && s <= tuple._2 }
    def isEmpty = e.isEmpty
    def isEverything = e != Nil && (e.head._1 == 0 && e.head._2 == Char.MaxValue)
    override def toString = s"CharLabel(${e map { tuple => char2string(tuple._1) + "->" + char2string(tuple._2)} mkString ","})"
    def mostGeneralLabel = CharLabel(allChars)
    
    if(e != Nil) assert((e.take(e.length - 1) zip e.tail) forall { case ((a, b), (c, d)) => b < c})
  }
  implicit def toCharLabel(e: List[(Char, Char)]) = CharLabel(e)
  implicit def fromCharLabel(c: CharLabel): EdgeLabel[Char] = c.e
  
  object DFA {
    def makeEdgesSeq[Node, T <: Label[T, Alphabet] forSome {type Alphabet}](i: Seq[(Node, T, Node)]): Map[Node, List[(T, Node)]] = {
      Map[Node, List[(T, Node)]]() ++ (i.groupBy(tuple => tuple._1) map {
        case (e, l) => e -> (l map { case (e, f, t) => (f, t) }).toList })
    }
    def makeEdges[Node, T <: Label[T, Alphabet] forSome {type Alphabet}](i: (Node, T, Node)*): Map[Node, List[(T, Node)]] = makeEdgesSeq[Node, T](i.toSeq)
  }
  
  /**
   * Deterministic finite-state automaton
   */
  case class DFA[Node, Alphabet, T <: Label[T, Alphabet]](nodes: Set[Node], edges: Map[Node, List[(T, Node)]], start: Node, ending: Set[Node], trap: Option[Node]) {
    def addEdge(e: Node, f: T, t: Node): DFA[Node, Alphabet, T] = {
      this.copy(edges = edges + (e-> ((f, t)::edges.getOrElse(e, List[(T, Node)]()))))
    }
    def recordFinalStates(s: Seq[Alphabet]): List[Int] = {
      var i = 0
      var res = List[Int]()
      if(ending contains start) res = i::res
      val (finalStates, f, lastState) = ((res, i, start) /: s) { case ((res, i, current), s) =>
        if(trap.exists(_ == current)) (res, i+1, current) else
        edges(current) find { case (label, next) => label.accept(s) } match {
          case Some((label, next)) => 
            if(ending contains next) (i::res, i+1, next) else (res, i+1, next)
          case None => (res, i+1, trap.get)
        }
      }
      finalStates.reverse
    }
    edges.values foreach { l =>
      assertComplete[T](l.map(_._1))
      assertDisjoints[T](l.map(_._1))
    }
  }
  
  object NDFA {
    /**
     * Transforms a set of edges into a mapping.
     */
    def makeEdgesSeq[Node, T <: Label[T, Alphabet] forSome {type Alphabet}](i: Seq[(Node, T, Node)]): Map[Node, Map[T, List[Node]]] = {
      Map[Node, Map[T, List[Node]]]() ++ (i.groupBy(tuple => tuple._1) map {
        case (e, l) => e -> (l.groupBy(tuple => tuple._2) map
              { case (f, l) => f -> (l.distinct map { tuple => tuple._3 }).toList }).toMap
        })
    }
    def makeEdges[Node, T <: Label[T, Alphabet] forSome {type Alphabet}](i: (Node, T, Node)*): Map[Node, Map[T, List[Node]]] = makeEdgesSeq[Node, T](i.toSeq)
    def apply[Node, T <: Label[T, Alphabet] forSome {type Alphabet}](nodes: Set[Node], edges: Map[Node, Map[T, List[Node]]], start: Node, ending: Set[Node]) = {
      
    }
  }
  
  case class NDFA[Node, T <: Label[T, Alphabet] forSome {type Alphabet}](nodes: Set[Node], edges: Map[Node, Map[T, List[Node]]], start: Node, ending: Set[Node], trap: Option[Node]) {
    def addEdge(e: Node, f: T, t: Node): NDFA[Node, T] = {
      val imageE = edges.getOrElse(e, Map[T, List[Node]]())
      val imageE2 = t::imageE.getOrElse(f, Nil)
      this.copy(edges = edges + (e->(imageE + (f -> imageE2))))
    }
    def edgesLabels = (edges flatMap { case (n, m) => m.keys }).toSet
    
    edges.values foreach { e =>
      assertComplete[T](e.keys.toList)
      assertDisjoints[T](e.keys.toList)
    }
  }
  
  /**
   * Determinize a NFA.
   */
  def toDFA[Node, Alphabet, T <: Label[T, Alphabet]](d: NDFA[Node, T]): DFA[Int, Alphabet, T] = {
    var visitedNodes = Set[Set[Node]]()
    var current = Set(d.start)
    
    def successors(nodeSet: Set[Node]): Map[T, Set[Node]] = {
      val s = nodeSet map d.edges
      val possibilities = s flatMap { m =>
        val mapping = m map {
          case (label, destNodes) => destNodes map {
            case nodes => (label, nodes) }
        }
        mapping.flatten
      }
      val labels = possibilities.map{tuple => tuple._1}.toSeq
      val mapping = createDisjointSets(labels)
      val flattenedEdges = possibilities.flatMap{ case (edge, nodeDest) => mapping(edge) map { case newEdge => (newEdge, nodeDest)}}
      // Now we need to be sure that labels are disjoint or group them
      val res = flattenedEdges.groupBy(tuple => tuple._1)
      val resMapped = res.mapValues(set => set.map(tuple => tuple._2))
      resMapped
    }
    
    var edges = List[(Int, T, Int)]()
    
    var mapping = Map[Set[Node], Int]()
    var currentIndex = -1
    var nodes = Set[Int]()
    var ending = Set[Int]()
    def getFor(s: Set[Node]): Int = mapping.getOrElse(s, {currentIndex = currentIndex + 1; mapping = mapping + (s -> currentIndex);
      nodes += currentIndex;
      if(!(s intersect d.ending).isEmpty) ending += currentIndex
      currentIndex})
      
    val start = Set(d.start)
    var queue = List(start)
    
    while(! queue.isEmpty) {
      val c = queue.head
      visitedNodes += c
      queue = queue.tail
      successors(c) foreach {
        case (edge, nodeSet) =>
          if(!(visitedNodes contains nodeSet) && !(queue contains nodeSet)) queue = nodeSet :: queue
          edges = (getFor(c), edge, getFor(nodeSet))::edges
      }
    }
    
    DFA[Int, Alphabet, T](
        nodes = nodes,
        edges = DFA.makeEdgesSeq(edges),
        trap = None,
        start = getFor(start),
        ending = ending
    )
  }
  
  /**
   * Undeterminize a DFA.
   */
  def toNDFA[Node, Alphabet, T <: Label[T, Alphabet]](d: DFA[Node, Alphabet, T]): NDFA[Node, T] = {
    NDFA[Node, T](
      nodes = d.nodes,
      edges = d.edges.mapValues{ l => l.map{ case (t, n) => (t, List(n))}.toMap },
      start = d.start,
      ending = d.ending,
      trap = d.trap
    )
  }
  
  
  case object AllChars extends CharClass(allChars)
  
  /**
   * Converts a RegExp into a deterministic automaton
   * which recognizes if a string has a postfix ending with this regexp.
   */
  import Programs._
  def convertRegExp(e: RegExp): DFA[Int, Char, CharLabel] = {
    e match {
      case TokenSeq(Seq()) =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0),
            edges = DFA.makeEdges((0, allChars, 0)),
            trap = None,
            start = 0,
            ending = Set(0)
        )
        dfa
      case TokenSeq(l) =>
        val dfas1 = if(l.head == StartTok) {
          (l.tail map convertToken).toList
        } else { // Do not necessarily start from the beginning
          (l.toList) map convertToken
        }
        val dfas2 = if(l.head == StartTok) {
          dfas1
        } else { // We add the possibility to parse everything
          DFA[Int, Char, CharLabel](
              nodes = Set(0),
              edges = DFA.makeEdges((0, allChars, 0)),
              trap = None,
              start = 0,
              ending = Set(0)
          )::dfas1
        }
        dfas2 match {
          case Nil =>convertRegExp(TokenSeq(Seq()))
          case a::Nil => a
          case l => val ndfas = l.map(toNDFA)
            val final_ndfa = (ndfas.head /: ndfas.tail) { case (a, b) => concatenateNDFA(a, b) }
            toDFA(final_ndfa)
        }
      case _ => throw new Error(s"Impossible to get something different !!! $e")
    }
  }
  
  def allChars = List(((0: Char), Char.MaxValue))
  def noChars = List()
  def isEmpty(x: EdgeLabel[Char]) = x == noChars
  /**
   * 
   */
  
  /**
   * Takes the complement of a list of Chars
   */
  def not(f: List[(Char, Char)]): List[(Char, Char)] = {
    // Take all chars in between and simplify
    val (starts, ends) = f.unzip
    val res =  (0:: ends.map{c => c+1} ) zip (starts.map{c => c-1} ++ List(Char.MaxValue.toInt))
    val res1 = res.filterNot(t => t._1 > t._2).map(t => (t._1.toChar, t._2.toChar))
    res1
  }

  /**
   * Takes the intersection of two lists of chars
   */
  def inter(f: List[(Char, Char)], g: List[(Char, Char)]): List[(Char, Char)] = {
    def rec(f: List[(Char, Char)], g: List[(Char, Char)], res: List[(Char, Char)]): List[(Char, Char)] = (f, g) match {
      case (Nil, _) => res.reverse
      case (_, Nil ) => res.reverse
      case ((a, b)::q, (c, d)::p) if b < c => rec(q, g, res)
      case ((a, b)::q, (c, d)::p) if d < a => rec(f, p, res)
      case ((a, b)::q, (c, d)::p) if a <= c && b == d => rec(q, p, (c, d)::res) // c <= b, d >= a
      case ((a, b)::q, (c, d)::p) if a <= c && b > d => rec(f, p, (c, d)::res) // c <= b, d >= a
      case ((a, b)::q, (c, d)::p) if a <= c && b < d => rec(q, g, (c, b)::res) // c <= b, d >= a
      case _ => rec(g, f, res)
    }
    rec(f, g, Nil)
  }
  /**
   * Takes the diff of two lists of chars
   */
  def union(f: List[(Char, Char)], g: List[(Char, Char)]): List[(Char, Char)] = not(inter(not(f), not(g)))
  def union(f: List[List[(Char, Char)]]): List[(Char, Char)] = if(f == Nil) List() else if(f.length == 1) f.head else f.reduce(union(_, _))
  /**
   * Takes f without g
   */
  def diff(f: List[(Char, Char)], g: List[(Char, Char)]) = inter(f, not(g))
  
  /**
   * Converts a token into a DFA
   */
  def convertToken(t: Token): DFA[Int, Char, CharLabel] = {
    t match {
      case RepeatedToken(c: CharClass) =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1, 2),
            edges = DFA.makeEdges((0, not(c.f), 2),
                                  (0, c.f, 1),
                                  (1, c.f, 1),
                                  (1, not(c.f), 2),
                                  (2, allChars, 2)
                 ),
            start = 0,
            ending = Set(1),
            trap = Some(2)
        )
        dfa
      case RepeatedNotToken(c: CharClass) =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1, 2),
            edges = DFA.makeEdges((0, c.f, 2),
                                  (0, not(c.f), 1),
                                  (1, not(c.f), 1),
                                  (1, c.f, 2),
                                  (2, allChars, 2)),
            start = 0,
            ending = Set(1),
            trap = Some(2)
        )
        dfa
      case SpecialChar(c) =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1, 2),
            edges = DFA.makeEdges((0, not(List((c, c))), 2),
                                  (0, List((c, c)), 1),
                                  (1, allChars, 2),
                                  (2, allChars, 2)),
            trap = Some(2),
            start = 0,
            ending = Set(1)
        )
        dfa
      case StartTok =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1),
            edges = DFA.makeEdges((0, allChars, 1), (1, allChars, 1)),
            trap = Some(1),
            start = 0,
            ending = Set(0)
        )
        dfa
      case EndTok => throw new Exception("End token should not be considered as token for automata")
    }
  }
  
  // requires(content(a) reduce (union(_, _)) == content(b) reduce (union(_, _))
  /**
   * Split sets into subsets so that we can use the same labels for the automata combining a and b.
   */
  def createLabelSubsets(a: List[CharLabel], b: List[CharLabel]): Map[CharLabel, List[CharLabel]] = {
    //assertDisjoints(a)
    assertComplete(a)
    //assertDisjoints(b)
    assertComplete(b)
    val res = ((a map {t =>
      t -> b.flatMap{el_b => val res = el_b && t; if(isEmpty(res)) Nil else List(res)}
    }) ++ (
     b map {t =>
      t -> a.flatMap{el_a => val res = el_a && t; if(isEmpty(res)) Nil else List(res)}
    }
    )).toMap
    
    res.foreach { case (key, value) =>
      key == union(value map (_.e)).e
    }
    res
  } // ensures { res => content(res.keys) == content(a) ++ content(b) &&
    //                  (a ++ b) forall ( la => content(la) == content(res(la) reduce (union (_,_))))
    // }
  
  /**
   * Split sets into subsets so that there is no overlapping between values
   */
  def createDisjointSets[T <: Label[T, _]](a: Seq[T]): Map[T, List[T]] = {
    //assertComplete(a)
    if(a == Nil) return Map[T, List[T]]()

    var chunks: Set[T] = Set(a.head.mostGeneralLabel)
    var l = a
    // Split existing such that we can build the remaining with those chunks
    while(l != Nil) {
      //assertDisjoints(chunks.toList map CharLabel.apply)
      // Ensure that all chunks elements are disjoints
      val elem: T = l.head
      val (not_intersected: Set[T], intersected: Set[T]) = chunks.partition(set => (set && elem).isEmpty())
      val union_intersected = if(intersected.size == 1) intersected else {
        (intersected.head /: intersected.tail) { case (e, f) => e || f}
      }
      if(union_intersected == elem) {
        // Nothing to do
      } else { // need to cut intersected so that
        chunks = (intersected flatMap {
          case e => 
            val i1 = e && elem
            val i2 = e && !elem
            List(i1, i2)
        }) ++ not_intersected
      }
      l = l.tail
    }
    val images = a map { el =>
      val (not_intersected, intersected) = chunks.partition(set => (set && el).isEmpty)
      intersected.toList
    }
    (a zip images).toMap
  }
  
  
  /**
   * Concatenates two NDFA
   * Requires nodes to be labelled 0...a.nodes.size-1
   */
  def concatenateNDFA[Char](a: NDFA[Int, CharLabel], b: NDFA[Int, CharLabel]): NDFA[Int, CharLabel] = {
    //val offsetB = a.nodes.max
    //val offsetA = 0
    val trap = Some(a.nodes.size + b.nodes.size - (if(a.trap != None) 1 else 0) - (if(b.trap != None) 1 else 0))
    val maxA = a.nodes.size - (if(a.trap == None) 0 else 1) // We removed the trap
    def mapNodeB(n: Int): Int = if(b.trap.exists(i => i == n)) trap.get else n + maxA
    def mapNodeA(n: Int): Int = if(a.trap.exists(i => i == n)) trap.get else n + 0
    
    val nodes = (a.nodes map mapNodeA) ++ (b.nodes map mapNodeB)
    //ndfa.ending = a.ending.map{_ + offsetA} ++ b.ending.map{_ + offsetB}
    // The common trap is mapped to new state
    
    // Starting state of a is the starting state of the new automata
    val start = 0
    
    // Create a map between edges in a and in b by taking their intersection and ensuring that
    // labels are disjoint.
    
    val labelsA = a.edgesLabels
    val labelsB = b.edgesLabels
    
    // Maps any label to corresponding sub-labels.
    
    // Maps all previous edges to new edges using the new labelling system.
    // Do not count the ending edges now. We split them afterwards.
    val aedgesnew = a.edges.flatMap {
      case (e, m) =>
        if(a.ending contains e) Nil else {
          val e_mapped = mapNodeA(e)
          m.flatMap {
            case (label, l) =>
              l map { el => (e_mapped, label, mapNodeA(el))}
          }
        }
    }
    val bedgesnew = b.edges.flatMap {
      case (e, m) =>
        val e_mapped = mapNodeB(e)
        m.flatMap {
          case (label, l) =>
            l map { el => (e_mapped, label, mapNodeB(el))}
        }
    }
    
    // Keep all edges in A and B with the following transformations
    // - replace both trap state by the new one
    // - translate a nodes by offset
    // - Map labels to the list of new labels.
    // Adds new edges for final states of A to all reachable states from the start of B
    // Mark final states of A as final only if start of B is final.
    val newEdges = aedgesnew ++ bedgesnew
    
    val endingEdges = (a.ending.toList flatMap {
      case endingNode => 
        val endingNodeMapped = mapNodeA(endingNode)
        val aEndingEdges = a.edges(endingNode)
        // Bind every finishing state of a to nodes after starting of b with same labels
        val bStartEdges = b.edges(b.start)
        val mappingEdges = createLabelSubsets(aEndingEdges.keys.toList, bStartEdges.keys.toList)
        val res = (aEndingEdges flatMap { case (label, nodes) =>
          nodes flatMap {
            case node => mappingEdges(label).map {
              newLabel => (endingNodeMapped, newLabel, mapNodeA(node))
              }
          }
        }) ++
        (bStartEdges flatMap { case (label, t) =>
            (t flatMap { el => mappingEdges(label).map{ newLabel => (endingNodeMapped, newLabel, mapNodeB(el))}})  // Edges linking A and B
            //(t flatMap { el =>mappingEdges(label).map{ newLabel => (mapNodeB(b.start), newLabel, mapNodeB(el))}})     // Edges remaining for state B
        })
        res
    })
    
    val edges = NDFA.makeEdgesSeq(newEdges.toSeq ++ endingEdges.toSeq)
    
    
    val ending = (b.ending map mapNodeB) ++ (if(b.ending contains b.start) (a.ending map mapNodeA) else Set())
    
    val res = NDFA[Int, CharLabel](
      nodes = nodes,
      edges = edges,
      start = start,
      ending = ending,
      trap = trap
    )
    
    res
  }
}