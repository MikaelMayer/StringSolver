package ch.epfl.lara.synthesis.flashfill

import ch.epfl.lara.synthesis.flashfill.Programs._

/**
 * Automata to proceed with regular expressions
 */
object Automata { automataobject =>
  
  
  /**
   * A label on an edge accepts 
   */
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
      if(ending contains start) res = -1::res
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

}