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
  
  trait Label[Self <: Label[Self]] extends {
    def ||(other: Self): Self
    def &&(other: Self): Self
    def unary_! : Self
  }
  
  case class CharLabel(val e: EdgeLabel[Char]) extends Label[CharLabel] {
    def ||(other: CharLabel): CharLabel = new CharLabel(automataobject.union(e, other.e))
    def &&(other: CharLabel): CharLabel = new CharLabel(automataobject.inter(e, other.e))
    def unary_! : CharLabel = new CharLabel(automataobject.not(e))
  }
  implicit def toCharLabel(e: EdgeLabel[Char]) = CharLabel(e)
  implicit def fromCharLabel(c: CharLabel): EdgeLabel[Char] = c.e
  
  /*case class SingleAcceptor(c: Char) extends Label {
    def apply(s: Char): Boolean = s == c
  }
  case class SingleNotAcceptor(c: Char) extends Label {
    def apply(s: Char): Boolean = s != c
  }
  case class MultipleAcceptor(c: String) extends Label {
    def apply(s: Char): Boolean = c.contains(s)
  }
  case class MultipleNotAcceptor(c: String) extends Label {
    def apply(s: Char): Boolean = !c.contains(s)
  }*/
  
  object DFA {
    def makeEdgesSeq[Node, Alphabet, T <: Label[_]](i: Seq[(Node, T, Node)]): Map[Node, List[(T, Node)]] = {
      Map[Node, List[(T, Node)]]() ++ (i.groupBy(tuple => tuple._1) map {
        case (e, l) => e -> (l map { case (e, f, t) => (f, t) }).toList })
    }
    def makeEdges[Node, Alphabet, T <: Label[_]](i: (Node, T, Node)*): Map[Node, List[(T, Node)]] = makeEdgesSeq(i.toSeq)
  }
  
  /**
   * Deterministic finite-state automaton
   */
  case class DFA[Node, Alphabet, T <: Label[_]](nodes: Set[Node], edges: Map[Node, List[(T, Node)]], start: Node, ending: Set[Node], trap: Option[Node]) {
    def addEdge(e: Node, f: T, t: Node): DFA[Node, Alphabet, T] = {
      this.copy(edges = edges + (e-> ((f, t)::edges.getOrElse(e, List[(T, Node)]()))))
    }
    
    def recordFinalStates(s: Seq[Char]): List[Int] = ???
  }
  
  object NDFA {
    /**
     * Transforms a set of edges into a mapping.
     */
    def makeEdgesSeq[Node, Alphabet, T <: Label[_]](i: Seq[(Node, T, Node)]): Map[Node, Map[T, List[Node]]] = {
      Map[Node, Map[T, List[Node]]]() ++ (i.groupBy(tuple => tuple._1) map {
        case (e, l) => e -> (l.groupBy(tuple => tuple._2) map
              { case (f, l) => f -> (l map { tuple => tuple._3 }).toList }).toMap
        })
    }
    def makeEdges[Node, Alphabet, T <: Label[_]](i: (Node, T, Node)*): Map[Node, Map[T, List[Node]]] = makeEdgesSeq(i.toSeq)
  }
  
  case class NDFA[Node, Alphabet, T <: Label[_]](nodes: Set[Node], edges: Map[Node, Map[T, List[Node]]], start: Node, ending: Set[Node], trap: Option[Node]) {
    def addEdge(e: Node, f: T, t: Node): NDFA[Node, Alphabet, T] = {
      val imageE = edges.getOrElse(e, Map[T, List[Node]]())
      val imageE2 = t::imageE.getOrElse(f, Nil)
      this.copy(edges = edges + (e->(imageE + (f -> imageE2))))
    }
    def edgesLabels = (edges flatMap { case (n, m) => m.keys }).toSet
  }
  
  def determinize[Node, Alphabet](d: NDFA[Node, Alphabet, CharLabel]): DFA[Node, Alphabet, Label[_]] = ???
  
  import Programs._
  def convertRegExp(e: RegExp): DFA[Int, Char, CharLabel] = {
    e match {
      case TokenSeq(t) =>
        ???
      case _ =>
        ???
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
            nodes = Set(0, 1),
            edges = DFA.makeEdges((0, not(c.f), 2),
                                  (0, c.f, 1),
                                  (1, c.f, 1),
                                  (1, not(c.f), 2)),
            trap = Some(2),
            start = 0,
            ending = Set(1)
        )
        dfa
      case RepeatedNotToken(c: CharClass) =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1),
            edges = DFA.makeEdges((0, c.f, 2),
                                  (0, not(c.f), 1),
                                  (1, not(c.f), 1),
                                  (1, c.f, 2)),
            trap = Some(2),
            start = 0,
            ending = Set(1)
        )
        dfa
      case SpecialChar(c) =>
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1),
            edges = DFA.makeEdges((0, not(List((c, c))), 2),
                                  (0, List((c, c)), 1),
                                  (1, allChars, 2)),
            trap = Some(2),
            start = 0,
            ending = Set(1)
        )
        dfa
      case StartTok => //???
        val dfa = DFA[Int, Char, CharLabel](
            nodes = Set(0, 1),
            edges = DFA.makeEdges((0, allChars, 1)),
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
    ((a map {t =>
      t -> b.flatMap{el_b => val res = el_b && t; if(isEmpty(res)) Nil else List(res)}
    }) ++ (
     b map {t =>
      t -> a.flatMap{el_a => val res = el_a && t; if(isEmpty(res)) Nil else List(res)}
    }
    )) .toMap
  } // ensures { res => content(res.keys) == content(a) ++ content(b) &&
    //                  (a ++ b) forall ( la => content(la) == content(res(la) reduce (union (_,_))))
    // }
  
  /**
   * Concatenates two NDFA
   * Requires nodes to be labelled 0...n
   */
  def concatenateNDFA[Char](a: NDFA[Int, Char, CharLabel], b: NDFA[Int, Char, CharLabel]): NDFA[Int, Char, CharLabel] = {
    //val offsetB = a.nodes.max
    //val offsetA = 0
    val trap = Some(a.nodes.size + b.nodes.size)
    val maxA = a.nodes.max
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
    val mappingEdges: Map[CharLabel, List[CharLabel]] = createLabelSubsets(labelsA.toList, labelsB.toList)
    
    // Maps all previous edges to new edges using the new labelling system.
    val aedgesnew = a.edges.map {
      case (e, m) =>
        (mapNodeA(e), m.flatMap{
          case (label, l) =>
            mappingEdges(label) map { newLabel =>
              (newLabel, l map mapNodeA)
            }}
        )
    }
    val bedgesnew = b.edges.map {
      case (e, m) =>
        (mapNodeB(e), m.flatMap{
          case (label, l) =>
            mappingEdges(label) map { newLabel =>
              (newLabel, l map mapNodeB)
            }}
        )
    }
    
    // Keep all edges in A and B with the following transformations
    // - replace both trap state by the new one
    // - translate a nodes by offset
    // - Map labels to the list of new labels.
    // Adds new edges for final states of A to all reachable states from the start of B
    // Mark final states of A as final only if start of B is final.
    val newEdges = aedgesnew ++ bedgesnew
    val edges = NDFA.makeEdgesSeq(a.ending.map(mapNodeA).toList flatMap {
      case i => 
        // Bind every finishing state of a to nodes after starting of b with same labels
        val edgesStart = b.edges(b.start) flatMap {
          case (t, l) => val l_mapped = l map { nodeB => (t, mapNodeB(nodeB)) }
            val t_mapped = mappingEdges(t)
            t_mapped map { case t => (t, l)}
          }
        edgesStart flatMap { case (f, t) =>
            t map { el => (i, f, el)}
        }
    })
    
    val ending = (b.ending map mapNodeB) ++ (if(b.ending contains b.start) (a.ending map mapNodeA) else Set())
    
    val res = NDFA[Int, Char, CharLabel](
      nodes = nodes,
      edges = edges,
      start = start,
      ending = ending,
      trap = trap
    )
    
    res
  }
}