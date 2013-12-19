package ch.epfl.lara.synthesis.flashfill

import scala.collection.GenTraversableOnce
import scala.collection.mutable.PriorityQueue

object ProgramsSet {
  import Programs._
  import scala.language._

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
  sealed trait ProgramSet[+A <: Program] {
    def foreach[T](f: A => T): Unit
    def map[T](f: A => T): Stream[T]
    def flatMap[T](f: A => GenTraversableOnce[T]) = map(f).flatten
    def takeBest: A
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
      ???
      // TODO : Iterate.
    }
    def map[T](f: TraceExpr => T): Stream[T] = { // Sort programs according to their sizes.
      // Dynamic programming : Keep at each node all minimal programs.
      f(takeBest) #:: Stream.empty[T]
    }
    def neighbors(n: Node, n_weight: Int): Set[(Int, AtomicExpr, Node)] = {
      for(e <- ξ if e._1 == n; atomic <- W.getOrElse(e, Set.empty).map(_.takeBest).toList.sortBy(w => weight(w)).headOption) yield {
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
      
      val W2 = W.map { case ((e1, e2), v) => (nodeMapping(e1), nodeMapping(e2)) -> v }
      SDag(ñ2, ns2, nt2, ξ2, W2)
    }
  }
  
  type SAtomicExpr = ProgramSet[AtomicExpr]
  /**
   * Set of Loop expressions described in Programs.scala
   */
  case class SLoop(i: Identifier, e: STraceExpr) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(prog <- e) yield f(Loop(i, prog))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(prog <- e) f(Loop(i, prog))
    }
    def takeBest = Loop(i, e.takeBest)
  }
  /**
   * Set of SubStr expressions described in Programs.scala
   */
  case class SSubStr(vi: StringVariable, p1: Set[SPosition], p2: Set[SPosition]) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(pp1 <- p1.toStream; ppp1 <- pp1; pp2 <- p2; ppp2 <- pp2) yield f(SubStr(vi, ppp1, ppp2))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(pp1 <- p1; ppp1 <- pp1; pp2 <- p2; ppp2 <- pp2) f(SubStr(vi, ppp1, ppp2))
    }
    def takeBest = SubStr(vi, p1.toList.map(_.takeBest).sortBy(weight).head, p2.toList.map(_.takeBest).sortBy(weight).head)
  }
  
  /**
   * Returns the weight of a program.
   */
  def weight(p: Program): Int = p match {
    case CPos(k) => 10
    case Pos(r1, r2, c) => 1-weight(c)
    case Concatenate(l) => l.size
    case Loop(w, l) => weight(l) - 1
    case Number(s, l, os) => weight(s) - 1
    case ConstStr(s) => s.size*2
    case SubStr(vi, p, pos) => 1
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
  case class SAny(vi: StringVariable) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      Stream(f(ConstStr("")))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      f(ConstStr(""))
    }
    def takeBest = ConstStr("")
  }
  /**
   * Set of SubStr expressions described in Programs.scala
   */
  case class SNumber(a: SAtomicExpr, length: SIntegerExpr, offset: SInt, step: SInt) extends SAtomicExpr {
    def map[T](f: AtomicExpr => T): Stream[T] = {
      for(pp1 <- a; l <- length; o <- offset; s <- step) yield f(Number(pp1, l, (o.k, s.k)))
    }
    def foreach[T](f: AtomicExpr => T): Unit = {
      for(pp1 <- a; l <- length; o <- offset; s <- step) f(Number(pp1, l, (o.k, s.k)))
    }
    def takeBest = Number(a.takeBest, length.toList.sortBy(weight).head, (offset.takeBest.k, step.takeBest.k))
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
      for(rr1 <- r1; rr2 <- r2; cc <- c) yield f(Pos(rr1, rr2, cc))
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
  case object SEmpty extends ProgramSet[Nothing] {
    def map[T](f: Nothing => T): Stream[T] = ???
    def foreach[T](f: Nothing => T): Unit = ???
    def takeBest = ???
  }
  
  type SIntegerExpr = Set[IntegerExpr]
  /**
   * Set of tokens.
   */
  case class SToken(s: Set[Token]) extends ProgramSet[Token] {
    def intersect(other: SToken): SToken = {
      val intersection = s intersect other.s
      SToken(intersection)
    }
    def size = s.size
    def map[T](f: Token => T): Stream[T] = {
      for(t <- s.toStream) yield f(t)
    }
    def foreach[T](f: Token =>T): Unit = {
      for(t <- s.toStream) f(t)
    }
    def takeBest = s.toList.sortBy(weight).head
  }
  


  /**
   * Intersection function
   */
  def intersect(ss: Set[SAtomicExpr], tt: Set[SAtomicExpr]): Set[SAtomicExpr] = {
    for(s <- ss; t <- tt; r <- result(intersectAtomicExpr(s, t))) yield r
  }
  def result[T <: Program](a: ProgramSet[T]): Option[ProgramSet[T]] = if(size(a)==0) None else Some(a)
  
  def intersect(p1: STraceExpr, p2: STraceExpr)(implicit unify: Option[Identifier] = None): STraceExpr = (p1, p2) match {
    case (p1: SDag[_], p2: SDag[_]) => 
      intersectDag(p1, p2)
    case _ => SEmpty 
  }
  def intersectDag[Node1, Node2, Node3](p1: SDag[Node1], p2: SDag[Node2])(implicit unify: Option[Identifier] = None): STraceExpr = (p1, p2) match {
    case (s1@SDag(ñ1, n1s, n1t, ξ1, w1),
          s2@SDag(ñ2, n2s, n2t, ξ2, w2)) => 
          val ξ12 = for((n1, np1) <- ξ1; (n2, np2) <- ξ2) yield ((n1, n2), (np1, np2))
          val W12f = {  (arg : ((Node1, Node2), (Node1, Node2))) => arg match { case ((n1, n2), (np1, np2)) =>
              for(f1 <- w1(n1, np1); f2 <- w2(n2, np2)) yield intersectAtomicExpr(f1, f2)
            }}
          val W12 = (ξ12 ==> W12f)
          val ξ12final = ξ12.filterNot(e => W12.getOrElse(e, Set.empty).isEmpty)
          val ñ = ñ1 x ñ2
          if(ξ12final.size != 0) {
            val res = SDag[(Node1, Node2)](ñ, (n1s, n2s), (n1t, n2t), ξ12, W12).reduce
            if(sizeDag(res)(res.nt) == 0) SEmpty else res
          } else SEmpty
          // TODO : Optimize the structure of this DAG (remove unused nodes, unreachable nodes, edges with empty sets, etc.)
  }
  def notEmpty[T <: Program](a: ProgramSet[T]): Option[ProgramSet[T]] = if(a == SEmpty) None else Some(a)
  def intersectAtomicExpr(a: SAtomicExpr, b: SAtomicExpr)(implicit unify: Option[Identifier] = None): SAtomicExpr = (a, b) match {
    case (SLoop(i1, e1), SLoop(i2, e2)) if i1 == i2 => SLoop(i1, intersect(e1, e2)) 
    case (SConstStr(aa), SConstStr(bb)) if aa == bb => a
    case (SSubStr(vi@IntLiteral(i), pj, pk), SSubStr(vj@IntLiteral(j), pl, pm)) =>
      if(i == j || (unify.isDefined && ((i == j + 1) || (i == j - 1)))) {
        val pp1 = (for(p1 <- pj; p2 <- pl; res <- notEmpty(intersectPos(p1, p2))) yield res)
        val pp2 = (for(p1 <- pk; p2 <- pm; res <- notEmpty(intersectPos(p1, p2))) yield res)
        if(pp1.isEmpty || pp2.isEmpty) SEmpty else {
           if(i == j) SSubStr(vi, pp1, pp2)
           else if(i == j - 1 && unify.isDefined) SSubStr(Linear(1, unify.get, i), pp1, pp2)
           else if(i == j + 1 && unify.isDefined) SSubStr(Linear(1, unify.get, j), pp1, pp2)
           else SEmpty
        }
      } else SEmpty
    case (SAny(vi), SSubStr(vj, pj, pk)) if vi == vj => b
    case (SSubStr(vj, pj, pk), SAny(vi)) if vi == vj => b
    case (SNumber(ss1, l1, o1, s1), SNumber(ss2, l2, o2, s2)) =>
      val ss = intersectAtomicExpr(ss1, ss2)
      val l = l1 intersect l2
      val o = intersectIntSet(o1, o2)
      val s = intersectIntSet(s1, s2)
      if(size(ss)>0 && l.size > 0 && size(o) > 0 && size(s) > 0)
        SNumber(ss, l, o, s)
      else SEmpty
    case _ => SEmpty
  }
  def intersectPos(p1: SPosition, p2: SPosition)(implicit unify: Option[Identifier] = None): SPosition = (p1, p2) match {
    case (SCPos(k1), SCPos(k2)) if k1 == k2 => p1
    case (SPos(r11, r12, c1), SPos(r21, r22, c2)) =>
      val r1 = intersectRegex(r11,r12)
      if(r1 == SEmpty) return SEmpty
      val r2 = intersectRegex(r21,r22)
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
      val tokenSeq = s1 zip s2 map { case (t1, t2) => t1 intersect t2} filterNot (_.size == 0)
      if(tokenSeq.size == 0) SEmpty else STokenSeq(tokenSeq)
    case _ => SEmpty
  }
  def unify(s1: STraceExpr, s2: STraceExpr, w: Identifier) = intersect(s1, s2)(unify=Some(w))

  
  /**
   * Size function
   */
  def size(p: ProgramSet[T forSome { type T <: Program} ]): Int = p match {
    case SNumber(s, digits, offset, step) => size(s)*digits.size*size(offset)*size(step)
    case SSwitch(conds) => (1 /: (conds map _2 map size)) (_ * _)
    case dag@SDag(ñ1, n1s, n1t, ξ1, w1) => sizeDag(dag)()
    case SSubStr(vi, pj, pk) => (pj.toList map size).sum * (pk.toList map size).sum
    case SLoop(w, e) => size(e)
    case SConstStr(s) => 1
    case SCPos(k) => 1
    case SPos(r1, r2, c) => size(r1) * size(r2) * c.size
    case STokenSeq(tseq) => (1 /: (tseq map { (t:SToken) => t.size})) (_ * _)
    case SToken(s) => s.size
    case SEmpty => 0
    case SAny(_) => 1
    case SAnyInt(i) => 1
    case SIntSet(c) => c.size
  }
  def sizeDag[Node](p1: SDag[Node])(fromN: Node = p1.nt): Int = {
    if(fromN == p1.ns) 1 else {
      (for(np <- p1.ñ.toList) yield {
        val sum = (for(f <- p1.W.getOrElse((np, fromN),Set.empty)) yield size(f)).sum
        if(sum != 0) {
          sum * sizeDag(p1)(np)
        } else 0
      }).sum
    }
  }
  
}