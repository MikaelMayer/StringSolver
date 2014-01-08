package ch.epfl.lara.synthesis.flashfill

import scala.collection.GenTraversableOnce
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.BitSet
import scala.collection.mutable.Queue

object ProgramsSet {
  import Programs._
  import scala.language._
  import SubStrFlag._

   /**
     * Returns true if the identifier is used in this program.
     */
    def uses(s: Any, w: Identifier): Boolean = s match {
      case i: Identifier => i == w
      case p: Product => p.productIterator.toList exists { case arg => uses(arg, w) }
      case s: Set[_] => s exists { case arg => uses(arg, w) }
      case _ => false
    }
  
  /**
   * Implicit helpers.
   */
  implicit class addCrossProduct[N](s: Set[N]) {
    def x[M](t: Set[M]): Set[(N, M)] = for { x <- s; y <- t } yield (x, y)
  }
  implicit class addMappingTo[T](t: Set[T]) {
    def ==>[A](w: T => Set[A]): Map[T, Set[A]] = (t.toList map { case el => el -> w(el).filterNot(_ == SEmpty)}).filterNot{case (key, value) => value.isEmpty}.toMap
  }
  implicit def combinations[T <: Program](s: List[ProgramSet[T]]): Stream[List[T]] = {
    def rec(l: List[ProgramSet[T]], res: List[T]): Stream[List[T]] = l match {
      case Nil => Stream(res.reverse)
      case a::q => a flatMap { (prog: T) => rec(q, prog::res) }
    }
    rec(s, Nil)
  }
  def _2[A, B](t: Tuple2[A, B]) = t._2
  def _1[A, B](t: Tuple2[A, B]) = t._1
  
  /**
   * Set of programs described in Programs.scala
   */
  sealed trait ProgramSet[+A <: Program]  extends Traversable[A] { self: Product =>
    def foreach[T](f: A => T): Unit
    def map[T](f: A => T): Stream[T]
    def flatMap[T](f: A => GenTraversableOnce[T]) = map(f).flatten
    def takeBest: A
    override def isEmpty: Boolean = this == SEmpty || ProgramsSet.sizePrograms(this) == 0
    def sizePrograms = ProgramsSet.sizePrograms(this)
    override def toIterable: Iterable[A] = map((i: A) =>i)
    override def toString = this.getClass().getName().replaceAll(".*\\$","")+"("+self.productIterator.mkString(",")+")"
  }
  /**
   * Set of switch expressions described in Programs.scala
   */
  case class SSwitch(s: List[(Bool, STraceExpr)]) extends ProgramSet[Switch] {
    def map[T](f: Switch => T): Stream[T] = {
      for(t <- combinations(s map _2)) yield f(Switch(s map _1 zip t))
    }
    def foreach[T](f: Switch =>T): Unit = {
      for(t <- combinations(s map _2)) f(Switch(s map _1 zip t))
    }
    def takeBest = Switch((s map _1) zip ((s map _2) map (_.takeBest)))
  }
  /**
   * Set of concatenate expressions described in Programs.scala
   */
  type STraceExpr = ProgramSet[TraceExpr]
  case class SDag[Node](ñ: Set[Node], ns: Node, nt: Node, ξ: Set[(Node, Node)], W: Map[(Node, Node), Set[SAtomicExpr]]) extends STraceExpr {
    def foreach[T](f: TraceExpr => T): Unit = {
      f(takeBest)
      // TODO : Iterate.
    }
    def map[T](f: TraceExpr => T): Stream[T] = { // Sort programs according to their sizes.
      // Dynamic programming : Keep at each node all minimal programs.
      f(takeBest) #:: Stream.empty[T]
    }
    def neighbors(n: Node, n_weight: Int): Set[(Int, AtomicExpr, Node)] = {
      for(e <- ξ if e._1 == n;
          versions = W.getOrElse(e, Set.empty);
          //alternatives = versions.toIterable.flatMap(elem => elem.toIterable);
          alternatives = versions.collect{ case s@SConstStr(str) => ConstStr(str)};
          atomic <- versions.map(_.takeBest.withAlternative(alternatives)).toList.sortBy(w => weight(w)).headOption) yield {
        (-weight(atomic) + n_weight, atomic, e._2)
      }
    }
    def takeBest = {
      var minProg = Map[Node, List[AtomicExpr]]()
      var weights = Map[Node, Int]()
      var nodesToVisit = new PriorityQueue[(Int, List[AtomicExpr], Node)]()(Ordering.by[(Int, List[AtomicExpr], Node), Int](e => e._1))
      nodesToVisit.enqueue((0, Nil, ns))
      while(!(minProg contains nt) && !nodesToVisit.isEmpty) {
        val (weight, path, node) = nodesToVisit.dequeue() // Takes the first node with the minimal path.
        minProg += node -> path
        for(e@(newweight, newAtomic, newNode) <- neighbors(node, weight)) {
          nodesToVisit.find{ case (w, p, n) => n == newNode } match {
            case Some((w, p, n)) => // New node already in nodes to visit.
              if(newweight > w && !(minProg contains newNode)) {
                nodesToVisit = nodesToVisit.filterNot{case (w, p, n) => n == newNode}
                nodesToVisit.enqueue((newweight, path.asInstanceOf[List[AtomicExpr]] ++ List(newAtomic).asInstanceOf[List[AtomicExpr]], newNode))
              } // Else we do nothing.
            case None =>
              nodesToVisit.enqueue((newweight, path.asInstanceOf[List[AtomicExpr]] ++ List(newAtomic).asInstanceOf[List[AtomicExpr]], newNode))
          }
        }
      }
      Concatenate(minProg(nt))//TODO : alternative.
    }
    def reduce: SDag[Int] = {
      val nodeMapping = ñ.zipWithIndex.toMap
      var ñ2 = nodeMapping.values.toSet
      val ns2 = nodeMapping(ns)
      val nt2 = nodeMapping(nt)
      var ξ2 = ξ map { case (n, m) => (nodeMapping.getOrElse(n,  -1), nodeMapping.getOrElse(m, -1))} filterNot {
        case (e1, e2) => e1 == -1 || e2 == -1
      }
      var finished = false
      while(!finished) { // Remove non reachable nodes
        finished = true
        // TODO : Optimize this code portion
        val uselessNodes = ñ2 filter { n =>
          n != ns2 && !(ξ2 exists { case (n1, n2) => n2 == n}) ||
          n != nt2 && !(ξ2 exists { case (n1, n2) => n1 == n})
        }
        if(!uselessNodes.isEmpty) {
          ñ2 = ñ2 -- uselessNodes
          ξ2 = ξ2 filterNot { case (n1, n2) => (uselessNodes contains n1) || (uselessNodes contains n2) }
          finished = false
        }
      }
      // W.flatMap 
      val W2 = for(((e1, e2), v) <- W;
                   edge = (nodeMapping.getOrElse(e1, -1), nodeMapping.getOrElse(e2, -1));
                   if ξ2 contains edge)
               yield (edge -> v)
      SDag(ñ2, ns2, nt2, ξ2, W2)
    }
  }
  
  /** Nodes are from 0 to ñ */
  /*case class SDag2(ñ: Int, ns: Int, nt: Int, ξ: BitSet, W: ArrayBuffer[Set[SAtomicExpr]]) extends STraceExpr {
    def foreach[T](f: TraceExpr => T): Unit = {
      ???
      // TODO : Iterate.
    }
    @inline def firstNode(edge: Int): Int = {
      edge % ñ
    }
    @inline def secondNode(edge: Int): Int = {
      edge 
    }
    @inline def makeEdge(m: Int, n: Int): Int = {
      (m+n)(m+n+1)2+m
    }
    def map[T](f: TraceExpr => T): Stream[T] = { // Sort programs according to their sizes.
      // Dynamic programming : Keep at each node all minimal programs.
      f(takeBest) #:: Stream.empty[T]
    }
    def neighbors(n: Int, n_weight: Int): Set[(Int, AtomicExpr, Int)] = {
      for(e <- ξ if e._1 == n; atomic <- W.getOrElse(e, Set.empty).map(_.takeBest).toList.sortBy(w => weight(w)).headOption) yield {
        (-weight(atomic) + n_weight, atomic, e._2)
      }
    }
    def takeBest = {
      var minProg = Map[Int, List[AtomicExpr]]()
      var weights = Map[Int, Int]()
      var nodesToVisit = new PriorityQueue[(Int, List[AtomicExpr], Int)]()(Ordering.by[(Int, List[AtomicExpr], Node), Int](e => e._1))
      nodesToVisit.enqueue((0, Nil, ns))
      while(!(minProg contains nt) && !nodesToVisit.isEmpty) {
        val (weight, path, node) = nodesToVisit.dequeue() // Takes the first node with the minimal path.
        minProg += node -> path
        for(e@(newweight, newAtomic, newNode) <- neighbors(node, weight)) {
          nodesToVisit.find{ case (w, p, n) => n == newNode } match {
            case Some((w, p, n)) => // New node already in nodes to visit.
              if(newweight > w && !(minProg contains newNode)) {
                nodesToVisit = nodesToVisit.filterNot{case (w, p, n) => n == newNode}
                nodesToVisit.enqueue((newweight, path.asInstanceOf[List[AtomicExpr]] ++ List(newAtomic).asInstanceOf[List[AtomicExpr]], newNode))
              } // Else we do nothing.
            case None =>
              nodesToVisit.enqueue((newweight, path.asInstanceOf[List[AtomicExpr]] ++ List(newAtomic).asInstanceOf[List[AtomicExpr]], newNode))
          }
        }
      }
      Concatenate(minProg(nt))
    }
    def reduce: SDag[Int] = {
      val nodeMapping = ñ.zipWithIndex.toMap
      var ñ2 = nodeMapping.values.toSet
      val ns2 = nodeMapping(ns)
      val nt2 = nodeMapping(nt)
      var ξ2 = ξ map { case (n, m) => (nodeMapping(n), nodeMapping(m))}
      var finished = false
      while(!finished) { // Remove non reachable nodes
        finished = true
        val uselessNodes = ñ2 filter { n =>
          n != ns2 && !(ξ2 exists { case (n1, n2) => n2 == n}) ||
          n != nt2 && !(ξ2 exists { case (n1, n2) => n1 == n})
        }
        if(!uselessNodes.isEmpty) {
          ñ2 = ñ2 -- uselessNodes
          ξ2 = ξ2 filterNot { case (n1, n2) => (uselessNodes contains n1) || (uselessNodes contains n2) }
          finished = false
        }
      }
      // W.flatMap 
      val W2 = for(((e1, e2), v) <- W;
                   edge = (nodeMapping(e1), nodeMapping(e2));
                   if ξ2 contains edge)
               yield (edge -> v)
      SDag(ñ2, ns2, nt2, ξ2, W2)
    }
  }*/
  
  type SAtomicExpr = ProgramSet[AtomicExpr]
  /**
   * Set of Loop expressions described in Programs.scala
   */
  case class SLoop(i: Identifier, e: STraceExpr, separator: Option[ConstStr]) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(prog: TraceExpr <- e.toStream) yield f(Loop(i, prog, separator))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(prog <- e) f(Loop(i, prog, separator))
    }
    def takeBest = Loop(i, e.takeBest, separator)//.withAlternative(this.toIterable)
  }
  

  
  /**
   * Set of SubStr expressions described in Programs.scala
   */
  case class SSubStr(vi: StringVariable, p1: Set[SPosition], p2: Set[SPosition], methods: SSubStrFlag) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(pp1 <- p1.toStream; ppp1: Position <- pp1; pp2 <- p2; ppp2: Position <- pp2; method: SubStrFlag <- methods) yield f(SubStr(vi, ppp1, ppp2, method))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(pp1 <- p1; ppp1 <- pp1; pp2 <- p2; ppp2 <- pp2; method <- methods) f(SubStr(vi, ppp1, ppp2, method))
    }
    def takeBest = SubStr(vi, p1.toList.map(_.takeBest).sortBy(weight(_)(true)).head, p2.toList.map(_.takeBest).sortBy(weight(_)(false)).head, methods.takeBest)//.withAlternative(this.toIterable)
  }
  
  /**
   * Returns the weight of a program.
   * 
   * Heuristic rules
   * - A number is preferated to a substring extracted
   * - A substring is preferated to constant strings
   * - A number is better if it a mapping from another number present in an input rather than in the output.
   *     in the case of extracting from input, the step can be anything.
   * - A number is preferated if its length is smaller.
   * - A concatenation is better if the sum of its children weights is better.
   */
  def weight(p: Program)(implicit starting_pos: Boolean = true): Int = p match {
    case CPos(0) => 1
    case CPos(-1) => 1
    case CPos(k) => 10
    case Pos(r1, r2, c) => 
      if(starting_pos) {
        3*weight(r1)+2*weight(r2)+weight(c)
      } else {
        2*weight(r1)+3*weight(r2)+weight(c)
      }
    case Concatenate(l) => l.size
    
    case Loop(w, l, separator) => 10 + weight(l) - 1 + 2*separator.map(_.s.length).getOrElse(0)
    case Number(s@ SubStr(InputString(_), p1, p2, m), l, (o, step)) =>
      10 + weight(s) - 2
    case Number(s@ SubStr(PrevStringNumber(_), p1, p2, m), l, (o, step)) =>
      10 + weight(s) - 1 + (step - 1) + (o - 1)
    case Number(s, l, (o, step)) =>
      10 + weight(s) - 1 + (step-1) // if s is smaller, the better.
    case ConstStr(s) => 10 + s.size*10
    case SubStr(vi, Pos(r1, r2, i), Pos(p1, p2, j), method) if i == j && r1 == Epsilon && p2 == Epsilon && r2 == p1 =>
      10 + weight(r2) + method.id
    case SubStr(vi, CPos(0), CPos(-1), method) => 10
    case SubStr(vi, p, pos, method) => 10 + weight(p)(true) + weight(pos)(false) + method.id
    
    case TokenSeq(t) => t.length // Best for empty sequences.
    case IntLiteral(i) => Math.abs(i)
    case Linear(i,w,j) => i
    case _ => 1
  }
  
  /**
   * Sets of integers for number decomposition.
   */
  type SInt = ProgramSet[IntLiteral]
  case class SIntSet(cl: Set[IntLiteral]) extends SInt {
    def map[T](f: IntLiteral => T): Stream[T] = {
      for(i <- cl.toStream) yield f(i)
    }
    def foreach[T](f: IntLiteral => T): Unit = {
       for(i <- cl) f(i)
    }
    def takeBest = cl.toList.sortBy{ case IntLiteral(k) => Math.abs(k) }.head
  }
  case class SAnyInt(default: Int) extends SInt { 
    def map[T](f: IntLiteral => T): Stream[T] = {
      Stream(f(default))
    }
    def foreach[T](f: IntLiteral => T): Unit = {f(default)}
    def takeBest = IntLiteral(default)
  }
  /**
   * Used to match any program on this string variable
   * Useful to intersect with working sub-expressions.
   */
  case class SAny(vi: PrevStringNumber) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      Stream(f(SubStr2(vi, NumTok, 1)))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      f(SubStr2(vi, NumTok, 1))
    }
    def takeBest = SubStr2(vi, NumTok, 1)
  }
  /**
   * Set of SubStr expressions described in Programs.scala
   */
  case class SNumber(a: SAtomicExpr, length: SIntegerExpr, offset: SInt, step: SInt) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(pp1: AtomicExpr <- a.toStream; l <- length; o: IntLiteral <- offset; s: IntLiteral <- step) yield f(Number(pp1, l, (o.k, s.k)))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(pp1 <- a; l <- length; o <- offset; s <- step) f(Number(pp1, l, (o.k, s.k)))
    }
    def takeBest = Number(a.takeBest, length.toList.sortBy(weight).head, (offset.takeBest.k, step.takeBest.k))//.withAlternative(this.toIterable)
  }
  
  /**
   * Set of Constant string expressions described in Programs.scala
   */
  case class SConstStr(s: String) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      Stream(f(ConstStr(s)))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      f(ConstStr(s))
    }
    def takeBest = ConstStr(s)
  }
  
  type SPosition = ProgramSet[Position]
  /**
   * Set of Constant positions described in Programs.scala
   */
  case class SCPos(k: Int) extends SPosition {
    def map[T](f: Position => T): Stream[T] = {
      Stream(f(CPos(k)))
    }
    def foreach[T](f: Position => T): Unit = {
      f(CPos(k))
    }
    def takeBest = CPos(k)
  }
  /**
   * Set of regexp positions described in Programs.scala
   */
  case class SPos(r1: SRegExp, r2: SRegExp, c: SIntegerExpr) extends SPosition {
    def map[T](f: Position => T): Stream[T] = {
      for(rr1: RegExp <- r1.toStream; rr2: RegExp <- r2; cc <- c) yield f(Pos(rr1, rr2, cc))
    }
    def foreach[T](f: Position => T): Unit = {
      for(rr1 <- r1; rr2 <- r2; cc <- c) f(Pos(rr1, rr2, cc))
    }
    def takeBest = Pos(r1.takeBest, r2.takeBest, c.toList.sortBy(weight).head)
  }
  
  type SRegExp = ProgramSet[RegExp]
  /**
   * Set of regexp described in Programs.scala
   */
  case class STokenSeq(s: List[SToken]) extends SRegExp {
    assert(s forall (_.size != 0))
    def map[T](f: RegExp => T): Stream[T] = {
      for(t <- combinations(s)) yield f(TokenSeq(t))
    }
    def foreach[T](f: RegExp =>T): Unit = {
      for(t <- combinations(s)) f(TokenSeq(t))
    }
    def takeBest = TokenSeq(s map (_.takeBest))
  }
  
  /**
   * Empty set for everything
   */
  case object SEmpty extends ProgramSet[Nothing] with Iterable[Nothing] {
    def map[T](f: Nothing => T): Stream[T] = ???
    override def foreach[T](f: Nothing => T): Unit = ???
    def takeBest = throw new Error("No program found")
    def iterator = Nil.toIterator
    override def toIterable = Nil
    override def isEmpty = true
  }
  
  type SIntegerExpr = Set[IntegerExpr]
  
  object SToken {
    def apply(t: Token*)(l: List[Token]): SToken = SToken(t.toList)(l)
    def apply(s: Traversable[Token])(l: List[Token]): SToken = {
      assert(s forall (i => l.indexOf(i) >= 0))
      if(s.size == 1) {
        val i = l.indexOf(s.head)
        SToken(1L << i)(l)
      } else if(s.size == 2) {
        val mask = s.toList.map(i => 1L << l.indexOf(i)).reduce(_ | _)
        SToken(mask)(l)
      } else {
        val (newMask, _) = ((0L, 1L) /: l) { case ((res, inc), token) => if(s exists (_ == token)) (res + inc, inc << 1) else (res, inc << 1)}
        SToken(newMask)(l)
      }
    }
  }
  /**
   * Set of tokens represented as a 1 at position i in binary format if the token is in the set, 0 otherwise
   * // Works if there are less than 64 tokens.
   */
  case class SToken(mask: Long)(val l: List[Token]) extends ProgramSet[Token] {
    def intersect(other: SToken): SToken = {
      if(other.l eq l) {
        val intersection_mask = mask & other.mask
        SToken(intersection_mask)(l)
      } else {
        val intersection_list = (l ++ other.l).distinct
        val tokens1 = toIterable.toSet
        val tokens2 = other.toIterable.toSet
        val tokens = tokens1 intersect tokens2
        val (newMask, _) = ((0L, 1L) /: intersection_list) { case ((res, inc), token) => if(tokens(token)) (res + inc, inc << 1) else (res, inc << 1)}
        SToken(newMask)(intersection_list)
      }
    }
    override def sizePrograms = java.lang.Long.bitCount(mask)
    def map[T](f: Token => T): Stream[T] = {
      def rec(m: Long, l: List[Token]): Stream[T] = l match {
        case Nil => Stream.empty
        case a::b if (m & 1) != 0 => f(a) #:: rec(m >> 1, b)
        case a::b => rec(m >> 1, b)
      }
      rec(mask, l)
    }
    override def foreach[T](f: Token =>T): Unit = {
      def rec(m: Long, l: List[Token]): Unit = l match {
        case Nil => 
        case a::b if (m & 1) != 0 => f(a); rec(m >> 1, b)
        case a::b => rec(m >> 1, b)
      }
      rec(mask, l)
    }
    override def isEmpty = size == 0
    def takeBest = map((i: Token) => i).toList.sortBy(weight).head
    def contains(t: Token): Boolean = ((1L << l.indexOf(t)) & mask) != 0
    override def toString = "SToken("+this.toList.mkString(",")+")"
  }
  
  /**
   * Constructor for set of flags for SSubStr
   */
  object SSubStrFlag {
    def apply(s: Traversable[SubStrFlag]): SSubStrFlag = {
      //assert(s forall (i => s.indexOf(i) >= 0))
      val l = SubStrFlag.registered
      if(s.size == 1) {
        val i = l.indexOf(s.head)
        SSubStrFlag(1L << i)
      } else if(s.size == 2) {
        val mask = s.toList.map(i => 1L << l.indexOf(i)).reduce(_ | _)
        SSubStrFlag(mask)
      } else {
        val (newMask, _) = ((0L, 1L) /: l) { case ((res, inc), token) => if(s exists (_ == token)) (res + inc, inc << 1) else (res, inc << 1)}
        SSubStrFlag(newMask)
      }
    }
  }
  case class SSubStrFlag(mask: Long) extends ProgramSet[SubStrFlag] with Traversable[SubStrFlag] {
    def intersect(other: SSubStrFlag): SSubStrFlag = if(other.mask == mask) this else SSubStrFlag(other.mask & mask)
    override def sizePrograms = java.lang.Long.bitCount(mask)
    def map[T](f: SubStrFlag => T): Stream[T] = {
      def rec(m: Long, id: Int = 0): Stream[T] = if(m == 0) Stream.empty else if((m & 1) == 1) f(SubStrFlag(id)) #:: rec(m >> 1, id + 1) else rec(m >> 1, id + 1)
      rec(mask)
    }
    override def foreach[T](f: SubStrFlag =>T): Unit = {
      def rec(m: Long, id: Int = 0): Unit = if(m == 0) Stream.empty else if((m & 1) == 1) { f(SubStrFlag(id)) ; rec(m >> 1, id + 1)} else rec(m >> 1, id + 1)
      rec(mask)
    }
    override def isEmpty = mask == 0
    def takeBest = map((i: SubStrFlag) => i).toList.sortBy(weight).head
    override def toString = "SSubStrFlag("+this.toList.mkString(",")+")"
  }
  


  /**
   * Intersection function
   */
  def intersect(ss: Set[SAtomicExpr], tt: Set[SAtomicExpr]): Set[SAtomicExpr] = {
    for(s <- ss; t <- tt; r <- result(intersectAtomicExpr(s, t))) yield r
  }
  def result[T <: Program](a: ProgramSet[T]): Option[ProgramSet[T]] = if(sizePrograms(a)==0) None else Some(a)
  
  def intersect(p1: STraceExpr, p2: STraceExpr)(implicit unify: Option[Identifier] = None): STraceExpr = (p1, p2) match {
    case (p1: SDag[_], p2: SDag[_]) => 
      intersectDag(p1, p2)
    case _ => SEmpty 
  }
  def intersectDag[Node1, Node2, Node3](p1: SDag[Node1], p2: SDag[Node2])(implicit unify: Option[Identifier] = None): STraceExpr = (p1, p2) match {
    case (s1@SDag(ñ1, n1s, n1t, ξ1, w1),
          s2@SDag(ñ2, n2s, n2t, ξ2, w2)) => 
          //println(s"Intersecting two dags of size: ${s1.ñ.size} and ${s2.ñ.size}")
          
          val ξ12 = for((n1, np1) <- ξ1; (n2, np2) <- ξ2) yield ((n1, n2), (np1, np2))
          val W12f = {  (arg : ((Node1, Node2), (Node1, Node2))) => arg match { case ((n1, n2), (np1, np2)) =>
              for(f1 <- w1(n1, np1); f2 <- w2(n2, np2)) yield {
                intersectAtomicExpr(f1, f2)
              }
            }}
          var W12 = Map[((Node1, Node2), (Node1, Node2)), Set[SAtomicExpr]]()
          var edges = Set[((Node1, Node2), (Node1, Node2))]()
          var nodesVisited = Set[(Node1, Node2)]()
          var nodesToVisit = Queue[(Node1, Node2)]((n1s, n2s))
          var nodesToVisitEnd = Queue[(Node1, Node2)]((n1t, n2t))
          val edgeMap = ξ12.groupBy(iijj => iijj._1)
          val edgeMapEnd = ξ12.groupBy(iijj => iijj._2)
          var emptyEdges = Set[((Node1, Node2), (Node1, Node2))]()
          // Alternate between nodes to visit on the end and on the start.
          while(!(nodesToVisitEnd.isEmpty || nodesToVisit.isEmpty)) {
            val nFirst = nodesToVisit.dequeue()
            nodesVisited += nFirst
            for(newEdge <- edgeMap.getOrElse(nFirst, Set.empty) if !(W12 contains newEdge) && !(emptyEdges(newEdge));
                e = newEdge._2) {
              val res = W12f(newEdge).filterNot(_ == SEmpty)
              if(!res.isEmpty) {
                edges += newEdge
                W12 += newEdge -> res
                if(!(nodesVisited contains e) && !(nodesToVisit contains e))
                  nodesToVisit.enqueue(e)
              } else {
                emptyEdges += newEdge
              }
            }
            val nLast = nodesToVisitEnd.dequeue()
            nodesVisited += nLast
            for(newEdge <- edgeMapEnd.getOrElse(nLast, Set.empty) if !(W12 contains newEdge) && !(emptyEdges(newEdge));
                e = newEdge._1) {
              val res = W12f(newEdge).filterNot(_ == SEmpty)
              if(!res.isEmpty) {
                edges += newEdge
                W12 += newEdge -> res
                if(!(nodesVisited contains e) && !(nodesToVisitEnd contains e))
                  nodesToVisitEnd.enqueue(e)
              } else {
                emptyEdges += newEdge
              }
            }
          }
          val ñ = nodesVisited
          //val ñ = ñ1 x ñ2
          
          //println(s"Computing edges...")
          //val W12 = (ξ12 ==> W12f)
          //println(s"Simplifying edges...")
          //val ξ12final = ξ12.filterNot(e => W12.getOrElse(e, Set.empty).isEmpty)
          val ξ12final = edges //edges.filterNot(e => W12.getOrElse(e, Set.empty).isEmpty)
          
          //println(s"Reducing automata...")
          if(!nodesVisited((n1t, n2t))) SEmpty else
          if(ξ12final.size != 0) {
            val res = SDag[(Node1, Node2)](ñ, (n1s, n2s), (n1t, n2t), ξ12final, W12).reduce
            if(sizeDag(res) == 0) SEmpty else res
          } else SEmpty
  }
  def notEmpty[T <: Program](a: ProgramSet[T]): Option[ProgramSet[T]] = if(a == SEmpty) None else Some(a)
  def intersectAtomicExpr(a: SAtomicExpr, b: SAtomicExpr)(implicit unify: Option[Identifier] = None): SAtomicExpr = (a, b) match {
    case (SLoop(i1, e1, sep1), SLoop(i2, e2, sep2)) if i1 == i2 && sep1 == sep2 => SLoop(i1, intersect(e1, e2), sep1) 
    case (SConstStr(aa), SConstStr(bb)) if aa == bb => a
    case (SSubStr(InputString(vi@IntLiteral(i)), pj, pk, m1), SSubStr(InputString(vj@IntLiteral(j)), pl, pm, m2)) =>
      if(i == j || (unify.isDefined && ((i == j + 1) || (i == j - 1)))) {
        val mm = m1 intersect m2
        val pp1 = (for(p1 <- pj; p2 <- pl; res <- notEmpty(intersectPos(p1, p2))) yield res)
        val pp2 = (for(p1 <- pk; p2 <- pm; res <- notEmpty(intersectPos(p1, p2))) yield res)
        if(pp1.isEmpty || pp2.isEmpty || mm.isEmpty) SEmpty else {
          if(i == j) SSubStr(InputString(vi), pp1, pp2, mm)
          else if(i == j - 1 && unify.isDefined) SSubStr(InputString(Linear(1, unify.get, i)), pp1, pp2, mm)
          else if(i == j + 1 && unify.isDefined) SSubStr(InputString(Linear(1, unify.get, j)), pp1, pp2, mm)
          else SEmpty
        }
      } else SEmpty
      case (SSubStr(PrevStringNumber(vi@IntLiteral(i)), pj, pk, m1), SSubStr(PrevStringNumber(vj@IntLiteral(j)), pl, pm, m2)) =>
      if(i == j || (unify.isDefined && ((i == j + 1) || (i == j - 1)))) {
        val mm = m1 intersect m2
        val pp1 = (for(p1 <- pj; p2 <- pl; res <- notEmpty(intersectPos(p1, p2))) yield res)
        val pp2 = (for(p1 <- pk; p2 <- pm; res <- notEmpty(intersectPos(p1, p2))) yield res)
        if(pp1.isEmpty || pp2.isEmpty) SEmpty else {
           if(i == j) SSubStr(PrevStringNumber(vi), pp1, pp2, mm)
           else if(i == j - 1 && unify.isDefined) SSubStr(PrevStringNumber(Linear(1, unify.get, i)), pp1, pp2, mm)
           else if(i == j + 1 && unify.isDefined) SSubStr(PrevStringNumber(Linear(1, unify.get, j)), pp1, pp2, mm)
           else SEmpty
        }
      } else SEmpty
    case (SAny(i), SSubStr(PrevStringNumber(j), pj, pk, m)) => b
    case (SSubStr(PrevStringNumber(j), pj, pk, m), SAny(vi)) => a
    case (SNumber(ss1, l1, o1, s1), SNumber(ss2, l2, o2, s2)) =>
      val ss = intersectAtomicExpr(ss1, ss2)
      val l = l1 intersect l2
      val o = intersectIntSet(o1, o2)
      val s = intersectIntSet(s1, s2)
      if(sizePrograms(ss)>0 && l.size > 0 && sizePrograms(o) > 0 && sizePrograms(s) > 0)
        SNumber(ss, l, o, s)
      else SEmpty
    case _ => SEmpty
  }
  def intersectPos(p1: SPosition, p2: SPosition)(implicit unify: Option[Identifier] = None): SPosition = (p1, p2) match {
    case (SCPos(k1), SCPos(k2)) if k1 == k2 => p1
    case (SPos(r11, r12, c1), SPos(r21, r22, c2)) =>
      val r1 = intersectRegex(r11,r21)
      if(r1 == SEmpty) return SEmpty
      val r2 = intersectRegex(r12,r22)
      if(r2 == SEmpty) return SEmpty
      val c = if(unify.isEmpty) c1 intersect c2 else {
        val res = ((c1 x c2) flatMap { 
          case (a, b) if a == b => List(a)
          case (IntLiteral(k1), IntLiteral(k2)) =>
            if(k1 < k2) List(Linear((k2-k1), unify.get, k1):IntegerExpr)
            else if(k2 < k1) List(Linear((k1-k2), unify.get, k2):IntegerExpr)
            else Nil
          case _ => Nil
        }).toSet
        res: SIntegerExpr
      }
      if(r1 == SEmpty || r2 == SEmpty || c.isEmpty) SEmpty else {
        SPos(r1, r2, c)
      }
    case _ => SEmpty
  }
  def intersectIntSet(p1: SInt, p2: SInt)(implicit unify: Option[Identifier] = None): SInt = (p1, p2) match {
    case (SIntSet(a), SIntSet(b)) => SIntSet(a intersect b)
    case (SAnyInt(default), b) => b
    case (a, SAnyInt(default)) => a
    case (SEmpty, _) => SEmpty
    case (_, SEmpty) => SEmpty
  }
  def intersectRegex(r1: SRegExp, r2: SRegExp): SRegExp = (r1, r2) match {
    case (STokenSeq(s1), STokenSeq(s2)) if s1.length == s2.length =>
      val tokenSeq = s1 zip s2 map { case (t1, t2) => t1 intersect t2}
      if(tokenSeq exists (e => e.size == 0)) SEmpty else STokenSeq(tokenSeq)
    case _ => SEmpty
  }
  def unify(s1: STraceExpr, s2: STraceExpr, w: Identifier) = intersect(s1, s2)(unify=Some(w))

  
  /**
   * Size function
   */
  def sizePrograms(p: ProgramSet[T forSome { type T <: Program} ]): Long = p match {
    case SNumber(s, digits, offset, step) => sizePrograms(s)*digits.size*sizePrograms(offset)*sizePrograms(step)
    case SSwitch(conds) => (1L /: (conds map _2 map sizePrograms)) (_ * _)
    case dag@SDag(ñ1, n1s, n1t, ξ1, w1) => sizeDag(dag)
    case SSubStr(vi, pj, pk, mm) => (pj.toList map sizePrograms).sum * (pk.toList map sizePrograms).sum * mm.sizePrograms
    case SLoop(w, e, _) => sizePrograms(e)
    case SConstStr(s) => 1
    case SCPos(k) => 1
    case SPos(r1, r2, c) => sizePrograms(r1) * sizePrograms(r2) * c.size
    case STokenSeq(tseq) => (1L /: (tseq map { (t:SToken) => t.size})) (_ * _)
    case s@ SToken(_) => s.size
    case SEmpty => 0
    case SAny(_) => 1
    case SAnyInt(i) => 1
    case SIntSet(c) => c.size
    case s @ SSubStrFlag(mask) => s.size
  }
  def sizeDag[Node](p1: SDag[Node]): Long = {
    var sizeNode = Map[Node, Long](p1.ns -> 1)
    def rec(fromN: Node): Long = {
      if(sizeNode contains fromN) sizeNode(fromN) else {
        val res = (for(np <- p1.ñ.toList) yield {
          val pre_sum = (for(f <- p1.W.getOrElse((np, fromN),Set.empty)) yield sizePrograms(f))
          val sum = if(pre_sum exists { i => i >= Integer.MAX_VALUE }) Integer.MAX_VALUE else {
            Math.min(Integer.MAX_VALUE, pre_sum.sum)
          }
          if(sum != 0) {
            Math.min(sum * rec(np), Integer.MAX_VALUE)
          } else 0
        }).sum
        sizeNode += fromN -> res
        res
      }
    }
    rec(p1.nt)
  }
  
}