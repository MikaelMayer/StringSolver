package ch.epfl.lara.synthesis.stringsolver

import scala.collection.GenTraversableOnce
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.BitSet
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

object ProgramsSet {
  import Programs._
  import scala.language._
  import SubStrFlag._
  import Weights._

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
    private var cacheBest: Option[Any] = None
    def takeBest: A = { if(cacheBest.isEmpty) cacheBest = Some(takeBestRaw); cacheBest.get.asInstanceOf[A]}
    def takeBestRaw: A
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
    def takeBestRaw = Switch((s map _1) zip ((s map _2) map (_.takeBest)))
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
    def takeBestRaw = {
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
    def takeBestRaw = {
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
    def takeBestRaw = Loop(i, e.takeBest, separator)//.withAlternative(this.toIterable)
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
    def takeBestRaw = SubStr(vi, p1.toList.map(_.takeBest).sortBy(weight(_)(true)).head, p2.toList.map(_.takeBest).sortBy(weight(_)(false)).head, methods.takeBest)//.withAlternative(this.toIterable)
    private var corresponding_string: (String, String, Int, Int) = ("", "", 0, -1)
    def setPos(from: String, s: String, start: Int, end: Int) = corresponding_string = (from, s, start, end)
  }
  
  def isCommonSeparator(s: String) = s match {
    case "," | " " | ";" | ", " | "; " | "\t" | "  " | ". " | "." | ":" => true
    case _ => false
  }
  
  
  
  /**
   * Sets of integers for number decomposition.
   * The best one is the greatest one in this implementation
   */
  type SInt = ProgramSet[IntLiteral]
  case class SIntSemiLinearSet(start: Int, step: Int, max: Int) extends SInt {
    def map[T](f: IntLiteral => T): Stream[T] = {
      if(step > 0)
      for(i <- start to max by step toStream) yield f(i)
      else if(start <= max)
        Stream(f(start))
      else Stream.empty
    }
    def foreach[T](f: IntLiteral => T): Unit = {
      if(step > 0)
       for(i <- start to max by step toStream) f(i)
      else if(start <= max)
        Stream(f(start))
      else Stream.empty
    }
    def takeBestRaw = if(step == 0) IntLiteral(start) else IntLiteral(start+step*((max-start)/step))
    def apply(elem: Int): Boolean = elem >= start && elem <= max && (step == 0 && start == elem || step != 0 && (elem-start)%step == 0)
  }
  /*case class SAnyInt(default: Int) extends SInt { 
    def map[T](f: IntLiteral => T): Stream[T] = {
      Stream(f(default))
    }
    def foreach[T](f: IntLiteral => T): Unit = {f(default)}
    def takeBestRaw = IntLiteral(default)
  }*/
  /**
   * Used to match any program on this string variable
   * Useful to intersect with working sub-expressions.
   */
  /*case class SAny(vi: PrevStringNumber) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      Stream(f(SubStr2(vi, NumTok, 1)))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      f(SubStr2(vi, NumTok, 1))
    }
    def takeBestRaw = SubStr2(vi, NumTok, 1)
  }*/
  /**
   * Set of SubStr expressions described in Programs.scala
   */
  case class SNumber(a: SAtomicExpr, length: SInt, offset: Int) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(pp1: AtomicExpr <- a.toStream; l: IntLiteral <- length) yield f(NumberMap(pp1.asInstanceOf[SubStr], l.k, offset))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(pp1: AtomicExpr <- a; l <- length) f(NumberMap(pp1.asInstanceOf[SubStr], l.k, offset))
    }
    def takeBestRaw = NumberMap(a.takeBest.asInstanceOf[SubStr], length.takeBest.k, offset)//.withAlternative(this.toIterable)
  }
  
  /**
   * Creates a counter set from a number and its position
   */
  object SCounter {
    //
    def fromExample(number: String, position: Int): SCounter = {
      val numberValue = number.toInt
      val possibleLengths = (if(number(0) != '0') {// It means that the generated length might be lower.
        SIntSemiLinearSet(1, 1, number.length)
      } else SIntSemiLinearSet(number.length, 1, number.length))
      val possibleStarts = if(position == 0) {
        SIntSemiLinearSet(numberValue, 1, numberValue)
      } else {
        SIntSemiLinearSet(numberValue % position, position, numberValue)
      }
       SCounter(possibleLengths, possibleStarts, numberValue, position)
    }
  }
  /**
   * Step = (index - start) / count if the division is applicable
   * Except if count = 0, step can be anything from 1 to infinity.
   */
  case class SCounter(length: SInt, starts: SInt, index: Int, count: Int) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(l <- length.toStream; s: IntLiteral <- starts; step <- if(count == 0) Stream.from(1) else List((index - s.k)/count)) yield f(Counter(l.k, s.k, step))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(l <- length.toStream; s: IntLiteral <- starts; step <- if(count == 0) Stream.from(1) else List((index - s.k)/count)) f(Counter(l.k, s.k, step))
    }
    def takeBestRaw = Counter(length.takeBest.k, starts.takeBest.k, if(count == 0) 1 else (index - starts.takeBest.k)/count)//.withAlternative(this.toIterable)
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
    def takeBestRaw = ConstStr(s)
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
    def takeBestRaw = CPos(k)
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
    def takeBestRaw = Pos(r1.takeBest, r2.takeBest, c.toList.sortBy(weight).head)
  }
  
  type SRegExp = ProgramSet[RegExp]
  /**
   * Set of regexp described in Programs.scala
   */
  case class STokenSeq(s: List[SToken]) extends SRegExp {
    assert(s forall (_.sizePrograms != 0))
    def map[T](f: RegExp => T): Stream[T] = {
      for(t <- combinations(s)) yield f(TokenSeq(t))
    }
    def foreach[T](f: RegExp =>T): Unit = {
      for(t <- combinations(s)) f(TokenSeq(t))
    }
    def takeBestRaw = TokenSeq(s map (_.takeBest))
  }
  
  /**
   * Empty set for everything
   */
  case object SEmpty extends ProgramSet[Nothing] with Iterable[Nothing] {
    def map[T](f: Nothing => T): Stream[T] = ???
    override def foreach[T](f: Nothing => T): Unit = ???
    def takeBestRaw = throw new Error("No program found")
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
    def takeBestRaw = map((i: Token) => i).toList.sortBy(weight).head
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
    def takeBestRaw = map((i: SubStrFlag) => i).toList.sortBy(weight).head
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
          //println("computing edges...")
          
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
          //println("Grouping edges...")
          val edgeMap11 = ξ1.groupBy(_1)
          val edgeMap12 = ξ1.groupBy(_2)
          val edgeMap21 = ξ2.groupBy(_1)
          val edgeMap22 = ξ2.groupBy(_2)
          //println("Gathering edges...")
          val edgeMap = new  {
            def getOrElse(n1n2: (Node1, Node2), orElse: Iterable[((Node1, Node2), (Node1, Node2))]) = {
              for((_, n12) <- edgeMap11.getOrElse(n1n2._1, Set.empty).iterator; (_, n22) <- edgeMap21.getOrElse(n1n2._2, Set.empty))
                yield (n1n2, (n12, n22))
            }
          }
          val edgeMapEnd = new  {
            def getOrElse(n1n2: (Node1, Node2), orElse: Iterable[((Node1, Node2), (Node1, Node2))]) = {
              for((n12, _) <- edgeMap12.getOrElse(n1n2._1, Set.empty).iterator; (n22, _) <- edgeMap22.getOrElse(n1n2._2, Set.empty))
                yield ((n12, n22), n1n2)
            }
          }

          var emptyEdges = Set[((Node1, Node2), (Node1, Node2))]()
          // Alternate between nodes to visit on the end and on the start.
          while(!(nodesToVisitEnd.isEmpty || nodesToVisit.isEmpty)) {
            //println(s"Nodes to visit start: ${nodesToVisit.size}", s"Nodes to visit end: ${nodesToVisitEnd.size}")
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
  def intersectAtomicExpr(a: SAtomicExpr, b: SAtomicExpr)(implicit unify: Option[Identifier] = None): SAtomicExpr = if(a eq b) a else ((a, b) match {
    case (SLoop(i1, e1, sep1), SLoop(i2, e2, sep2)) if sep1 == sep2 =>
      val be2 = replaceSTraceExpr(e2){ case l@Linear(a, i, b) => if(i == i2) Linear(a, i1, b) else l }
      val intersectBody = intersect(e1, be2)
      if(!intersectBody.isEmpty) {
        SLoop(i1, intersectBody, sep1) 
      } else SEmpty
    case (SConstStr(aa), SConstStr(bb)) if aa == bb => a
    //case (SConstStr(aa), SConstStr(bb)) if aa.isNumber == bb.isNumber => a
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
    case (SSubStr(InputString(vi: Linear), pj, pk, m1), SSubStr(InputString(vj: Linear), pl, pm, m2)) =>
      if(vi == vj) {
        val mm = m1 intersect m2
        val pp1 = (for(p1 <- pj; p2 <- pl; res <- notEmpty(intersectPos(p1, p2))) yield res)
        val pp2 = (for(p1 <- pk; p2 <- pm; res <- notEmpty(intersectPos(p1, p2))) yield res)
        if(pp1.isEmpty || pp2.isEmpty || mm.isEmpty) SEmpty else {
          SSubStr(InputString(vi), pp1, pp2, mm)
        }
      } else SEmpty
      /*case (SSubStr(PrevStringNumber(vi@IntLiteral(i)), pj, pk, m1), SSubStr(PrevStringNumber(vj@IntLiteral(j)), pl, pm, m2)) =>
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
      } else SEmpty*/
    //case (SAny(i), SSubStr(PrevStringNumber(j), pj, pk, m)) => b
    //case (SSubStr(PrevStringNumber(j), pj, pk, m), SAny(vi)) => a
    case (SNumber(ss1, l1, o1), SNumber(ss2, l2, o2)) if(o1 == o2)=>
      //val s = intersectIntSet(s1, s2)
      val l = intersectIntSet(l1, l2)
      if(sizePrograms(l) > 0) {
        val ss = intersectAtomicExpr(ss1, ss2)
        if(sizePrograms(ss)>0)
          SNumber(ss.asInstanceOf[SSubStr], l, o1)
        else SEmpty
      } else SEmpty
    case (SCounter(l1, s1, i1, c1), SCounter(l2, s2, i2, c2)) =>
      val s = intersectIntSet(s1, s2)
      val l = intersectIntSet(l1, l2)
      if(sizePrograms(l) > 0 && sizePrograms(s) > 0) {
        if(c1 == c2) {
          if(i1 == i2)
            SCounter(l, s, i1, c1)
          else
            SEmpty
        } else {
          if((i2 - i1) % (c2 - c1) != 0) SEmpty else {
            val newStep = Math.abs((i2 - i1)/(c1-c2))
            val newStart = i1 - c1 * newStep
            val s2 = intersectIntSet(s, SIntSemiLinearSet(newStart, 0, newStart))
            val newStart2 = s2 match {
              case si @ SIntSemiLinearSet(start, step, max) => start
              case _ =>
                -1
            }
            if(c2 != 0 && newStart2 != i2) {
              SCounter(l, s2, i2, c2)
            } else if(c1 != 0 && newStart2 != i1)
              SCounter(l, s2, i1, c1)
            else SEmpty
          }
        }
      } else SEmpty
    case _ => SEmpty
  })
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
            if(k1 < k2 && k1 >= 0) List(Linear((k2-k1), unify.get, k1):IntegerExpr)
            //else if(k2 < k1 && k1 < 0) List(Linear((k2-k1), unify.get, k1):IntegerExpr)
            //else if(k2 < k1 && k2 >= 0) List(Linear((k1-k2), unify.get, k2):IntegerExpr)
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
  def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b%a, a)
  def extendGcd(a: Int, b: Int, s: Int = 0, t: Int= 1, old_s: Int = 1, old_t: Int = 0)(r: Int = b, old_r: Int = a): (Int, Int) = {
    if(r == 0) { (old_s, old_t)
    } else { val quotient = (old_r / r)
      extendGcd(a, b, old_s - quotient*s, old_t - quotient*t, s, t)(old_r - quotient*r, r)
    }
  }
  
  def intersectIntSet(p1: SInt, p2: SInt)(implicit unify: Option[Identifier] = None): SInt = (p1, p2) match {
    case (p1@SIntSemiLinearSet(start1, step1, max1), p2@SIntSemiLinearSet(start2, step2, max2)) => 
      // Multiple cases.
      val newMax = Math.min(max1, max2)
      if(step1 == 0 || step2 == 0) {
        if(step1 == 0) {
          if(p2(start1)) p1 else SEmpty
        } else { // step2 == 0
          if(p1(start2)) p2 else SEmpty
        }
      } else if(step1 == step2) {
        if(start1 == start2) {
          if(max1 <= max2) p1 else p2
        } else if((start2 - start1) % step1 == 0){
          val newStart = Math.max(start2, start1)
          if(newStart <= newMax) {
            SIntSemiLinearSet(newStart, step1, newMax)
          } else SEmpty
        } else SEmpty
      } else { // both steps are different. Will find the first one greater than the two starts.
        // Find a, b such that start1 + a*step1 == start2 + b*step2
        // It means that a*step1-b*step2=start2-start1
        val gcd2 = gcd(step1, step2)
        if((start2 - start1) % gcd2 != 0) SEmpty else {
          val c1 = step1/gcd2
          val c2 = step2/gcd2
          val i = (start2 - start1)/gcd2
          // Solve a*c1+b*c2 == 1 with bezout.
          val (a_wo_i, b_wo_i) = extendGcd(c1, c2)()
          val a = a_wo_i * i
          val b = b_wo_i * i
          // Now start1 + a * step1 == start2 + b * step2
          val newStep = step1 * step2 / gcd2 // The LCM is the new step.
          val possibleStart = start1 + a*step1
          val maxStart = Math.max(start1, start2)
          val base = maxStart - possibleStart
          val startI = (base + ((((newStep - base) % newStep) + newStep)%newStep))/newStep
          val newStart = possibleStart + newStep*startI
          if(newStart <= newMax)
          SIntSemiLinearSet(newStart, newStep, newMax)
          else SEmpty
        }
      }
    /*case (SAnyInt(default), b) => b
    case (a, SAnyInt(default)) => a*/
    case (SEmpty, _) => SEmpty
    case (_, SEmpty) => SEmpty
  }
  def intersectRegex(r1: SRegExp, r2: SRegExp): SRegExp = (r1, r2) match {
    case (STokenSeq(s1), STokenSeq(s2)) if s1.length == s2.length =>
      var i1 = s1
      var i2 = s2
      var res = ListBuffer[SToken]()
      while(i1 != Nil && i2 != Nil) {
        val tmp = i1.head intersect i2.head
        if(tmp.sizePrograms == 0) return SEmpty
        res += tmp
        i1 = i1.tail
        i2 = i2.tail
      }
      if(i1 != Nil || i2 != Nil) return SEmpty // should not happen
      STokenSeq(res.toList)
    case _ => SEmpty
  }
  def unify(s1: STraceExpr, s2: STraceExpr, w: Identifier) = intersect(s1, s2)(unify=Some(w))

  
  /**
   * Size function
   */
  def sizePrograms(p: ProgramSet[T forSome { type T <: Program} ]): Long = p match {
    case SNumber(s, digits, offset) => sizePrograms(s)*digits.size
    case SCounter(length, start, index, count) => if(count == 0) 100 else sizePrograms(start)*sizePrograms(length)
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
    /*case SAny(_) => 1
    case SAnyInt(i) => 1*/
    case SIntSemiLinearSet(start, offset, max) => if(offset == 0) 1 else (max - start)/offset + 1
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
  
  /**
   * Replace routines (for intersection of loops)
   */
  
  /**
   * Replace routines
   */
  def replaceSTraceExpr(e: STraceExpr)(implicit w: Linear => Linear): STraceExpr = e match {
    case SDag(n, ns, nt, e, ww) =>  SDag(n, ns, nt, e, ww.mapValues(_.map(v => replaceSAtomicExpr(v)(w))))
    case e => e
  }
  def replaceSAtomicExpr(e: SAtomicExpr)(implicit w: Linear => Linear): SAtomicExpr = e match {
    case SSubStr(vi, p1, p2, m) => SSubStr(replaceStringVariable(vi)(w), p1.map(t=>replaceSPosition(t)(w)), p2.map(t=>replaceSPosition(t)(w)), m)
    case SConstStr(s) => e
    case SLoop(w2, _, separator) if w2 == w => e
    case SLoop(w2, e, separator) => SLoop(w2, replaceSTraceExpr(e)(w), separator)
    case SNumber(s, l, o) => SNumber(replaceSAtomicExpr(s)(w), l, o)
    case e => e
  }
  def replaceSPosition(e: SPosition)(implicit w: Linear => Linear): SPosition = e match {
    case SPos(p1, p2, t) => SPos(p1, p2, replaceSIntegerExpr(t)(w))
    case _ => e
  }
  def replaceStringVariable(e: StringVariable)(implicit w: Linear => Linear): StringVariable = e match {
    case InputString(i) => InputString(replaceIntegerExpr(i)(w))
    //case PrevStringNumber(i) => PrevStringNumber(replaceIntegerExpr(i)(w))
    case e => e
  }
  def replaceSIntegerExpr(e: SIntegerExpr)(implicit w: Linear => Linear): SIntegerExpr = e.map(t => replaceIntegerExpr(t)(w))
  def replaceIntegerExpr(e: IntegerExpr)(implicit w: Linear => Linear): IntegerExpr = e match {
    case e @ Linear(i, v, j) => w(e)
    case e => e
  }
 // addToEveryOccurence
  
}
