package ch.epfl.lara.synthesis.flashfill

import scala.collection.GenTraversableOnce

object ProgramsSet {
  import Programs._
  
  def _2[A, B](t: Tuple2[A, B]) = t._2
  /**
   * Set of programs described in Programs.scala
   */
  sealed trait ProgramSet {
    def foreach[T](f: Program => T): Unit
    def map[T](f: Program => T): Set[T]
    def flatMap[T](f: Program => GenTraversableOnce[T]) = map(f).flatten
  }
  case class SSwitch(s: List[(Bool, STraceExpr)]) extends ProgramSet {
    def map[T](f: Program => T) = {
      def rec(l: List[(Bool, STraceExpr)], res: List[(Bool, TraceExpr)]): Set[T] = {
        case Nil => Set(f(Switch(res.reverse)))
        case (b, a)::q => a foreach { prog =>
          rec(q, ::res)
        }
          
      }
      
      yield e1
    }
  }
  sealed trait STraceExpr extends ProgramSet
  case class SDag[Node](ñ: Set[Node], ns: Node, nt: Node, ξ: Set[(Node, Node)], W: Map[(Node, Node), Set[SAtomicExpr]]) extends STraceExpr {
  }
  
  sealed trait SAtomicExpr extends ProgramSet
  case class SLoop(i: Identifier, e: STraceExpr) extends SAtomicExpr
  case class SSubStr(vi: StringVariable, p1: Set[SPosition], p2: Set[SPosition]) extends SAtomicExpr
  case class SConstStr(s: String) extends SAtomicExpr
  
  sealed trait SPosition extends ProgramSet
  case class SCPos(k: Int) extends SPosition
  case class SPos(r1: SRegExp, r2: SRegExp, c: SIntegerExpr) extends SPosition
  
  sealed trait SRegExp extends ProgramSet
  case class STokenSeq(s: List[SToken]) extends SRegExp
  
  case object SEmpty extends SRegExp with SPosition with SAtomicExpr with STraceExpr
  
  type SIntegerExpr = Set[IntegerExpr]
  type SToken = Set[Token]
  
  implicit class addCrossProduct[N](s: Set[N]) {
    def x[M](t: Set[M]): Set[(N, M)] = for { x <- s; y <- t } yield (x, y)
  }
  implicit class addMappingTo[T](t: Set[T]) {
    def ==>[A](w: T => A): Map[T, A] = (t.toList map { case el => el -> w(el)}).toMap
  }

  /**
   * Intersection function
   */
  def intersect(p1: STraceExpr, p2: STraceExpr)(implicit unify: Option[Identifier] = None): STraceExpr = (p1, p2) match {
    case (p1: SDag[_], p2: SDag[_]) => intersectDag(p1, p2)
    case _ => SEmpty 
  }
  def intersectDag[Node1, Node2](p1: SDag[Node1], p2: SDag[Node2])(implicit unify: Option[Identifier] = None): STraceExpr = (p1, p2) match {
    case (s1@SDag(ñ1, n1s, n1t, ξ1, w1),
          s2@SDag(ñ2, n2s, n2t, ξ2, w2)) => 
          val ξ12 = for((n1, np1) <- ξ1; (n2, np2) <- ξ2) yield ((n1, n2), (np1, np2))
          val W12f = {  (arg : ((Node1, Node2), (Node1, Node2))) => arg match { case ((n1, n2), (np1, np2)) =>
              for(f1 <- w1(n1, np1); f2 <- w2(n2, np2)) yield intersect(f1, f2)
            }}
          val W12 = ξ12 ==> W12f
          SDag[(Node1, Node2)](ñ1 x ñ2, (n1s, n2s), (n1t, n2t), ξ12, W12)
          // TODO : Optimize the structure of this DAG (remove unused nodes, etc.)
  }
  def intersect(a: SAtomicExpr, b: SAtomicExpr)(implicit unify: Option[Identifier] = None): SAtomicExpr = (a, b) match {
    case (SLoop(i1, e1), SLoop(i2, e2)) if i1 == i2 => SLoop(i1, intersect(e1, e2)) 
    case (SConstStr(aa), SConstStr(bb)) if aa == bb => a
    case (SSubStr(vi, pj, pk), SSubStr(vj, pl, pm)) if vi == vj =>
      val pp1 = (for(p1 <- pj; p2 <- pl) yield intersectPos(p1, p2)) filterNot (_ == SEmpty)
      val pp2 = (for(p1 <- pk; p2 <- pm) yield intersectPos(p1, p2)) filterNot (_ == SEmpty)
      if(pp1.isEmpty || pp2.isEmpty) SEmpty else
      SSubStr(vi, pp1, pp2)
    case _ => SEmpty
  }
  def intersectPos(p1: SPosition, p2: SPosition)(implicit unify: Option[Identifier] = None): SPosition = (p1, p2) match {
    case (SCPos(k1), SCPos(k2)) if k1 == k2 => p1
    case (SPos(r11, r12, c1), SPos(r21, r22, c2)) =>
      val r1 = intersectRegex(r11,r12)
      val r2 = intersectRegex(r21,r22)
      val c = if(unify.isEmpty) c1 intersect c2 else {
        val l1 = c1.collect{ case s:IntegerExpr => s}
        val l2 = c2.collect{ case s:IntegerExpr => s}
        val res = ((l1 x l2) flatMap { case (IntLiteral(k1), IntLiteral(k2)) => if(k1 != k2) List(Linear((k2-k1), unify.get, k1):IntegerExpr) else Nil }).toSet
        res: SIntegerExpr
      }
      if(r1 == SEmpty || r2 == SEmpty || c.isEmpty) SEmpty else {
        SPos(r1, r2, c)
      }
    case _ => SEmpty
  }
  def intersectRegex(r1: SRegExp, r2: SRegExp): SRegExp = (r1, r2) match {
    case (STokenSeq(s1), STokenSeq(s2)) if s1.length == s2.length =>
      val tokenSeq = s1 zip s2 map { case (t1, t2) => t1 intersect t2}
      if(tokenSeq contains SEmpty) SEmpty else STokenSeq(tokenSeq)
    case _ => SEmpty
  }
  def unify(s1: STraceExpr, s2: STraceExpr, w: Identifier) = intersect(s1, s2)(unify=Some(w))
  

  
  /**
   * Size function
   */
  def size(p: ProgramSet): Int = p match {
    case SSwitch(conds) => (1 /: (conds map _2 map size)) (_ * _)
    case dag@SDag(ñ1, n1s, n1t, ξ1, w1) => sizeDag(dag)()
    case SSubStr(vi, pj, pk) => (pj.toList map size).sum * (pk.toList map size).sum
    case SLoop(w, e) => size(e)
    case SConstStr(s) => 1
    case SCPos(k) => 1
    case SPos(r1, r2, c) => size(r1) * size(r2) * c.size
    case STokenSeq(tseq) => (1 /: (tseq map { (t:SToken) => t.size})) (_ * _)
    case SEmpty => 0
  }
  def sizeDag[Node](p1: SDag[Node])(fromN: Node = p1.nt): Int = {
    if(fromN == p1.ns) 1 else {
      (for(np <- p1.ñ.toList) yield {
        val sum = (for(f <- p1.W(np, fromN)) yield size(f)).sum
        if(sum != 0) {
          sum * sizeDag(p1)(np)
        } else 0
      }).sum
    }
  }
  
}