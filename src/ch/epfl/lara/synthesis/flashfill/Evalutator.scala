package ch.epfl.lara.synthesis.flashfill

import Programs._

trait ComputePositionsInString {
  /**
   * Computes the list of positions where there exists a word recognized by this regexp.
   */
  def computePositionsEndingWith(r: RegExp, s: String): List[Int]
}

object Evaluator {
  //import ctx.reporter._
  
  final val RegexpPositionsInString: ComputePositionsInString = Automata

  /**
   * Definitions of values
   */
  trait Value {
    def asString: String = throw new Exception(s"$this is not a string")
    def asBool: Boolean = throw new Exception(s"$this is not a Boolean")
    def asInt: Int = throw new Exception(s"$this is not an Int")
    def isBottom = false
    def asBoolFalseIfBottom = { if(isBottom) false else asBool }
  }
  case class StringValue(s: String) extends Value {
    override def asString = s
  }
  case class BoolValue(b: Boolean) extends Value {
    override def asBool = b
  }
  case class IntValue(n: Int) extends Value {
    override def asInt = n
  }
  case object BottomValue extends Value {
    override def isBottom = true
  }
  
  /**
   * Concatenates string values
   */
  def concatenate(ls: List[Value]): Value = {
    ((StringValue(""): Value) /: ls) {
      case (StringValue(a), StringValue(b)) => StringValue(a + b)
      case (_, _) => BottomValue
    }
  }
  
  /**
   * Replace routines
   */
  def replaceTraceExpr(e: TraceExpr)(implicit w: Identifier, k: Int): TraceExpr = e match {
    case Concatenate(fs) =>    Concatenate(fs.toList map replaceAtomicExpr)
  }
  def replaceAtomicExpr(e: AtomicExpr)(implicit w: Identifier, k: Int): AtomicExpr = e match {
    case SubStr(vi, p1, p2) => SubStr(vi, replacePosition(p1), replacePosition(p2))
    case ConstStr(s) => e
    case Loop(w2, _) if w2 == w => e
    case Loop(w2, e) => Loop(w2, replaceTraceExpr(e))
  }
  def replacePosition(e: Position)(implicit w: Identifier, k: Int): Position = e match {
    case Pos(p1, p2, Linear(k1, v, k2)) if v.value == w.value => Pos(p1, p2, IntLiteral(k * k1 + k2))
    case _ => e
  }
    
  /**
   * Loop routines
   */
  def loopR(w: Identifier, e: TraceExpr, k: Int)(implicit input: IndexedSeq[String]): Value = {
    val t = evalProg(replaceTraceExpr(e)(w, k))
    t match {
      case BottomValue => StringValue("")
      case StringValue(s) => concatenate(List(t, loopR(w, e, k+1)))
      case _ => BottomValue
    }
  }
  
  /**
   * Evaluates a program given an input.
   */
  def evalProg(p: Program)(implicit input: IndexedSeq[String]): Value = p match {
    case Switch(s) =>
      s.find{case (bool, expr) => evalProg(bool).asBoolFalseIfBottom} match {
        case Some((b, expr)) =>
          evalProg(expr)
        case None =>
          BottomValue
      }
    case Bool(ds) =>
      BoolValue((false /: ds) { case (res, cj) => res || evalProg(cj).asBoolFalseIfBottom })
    case Conjunct(pis) =>
      BoolValue((true /: pis) { case (res, cj) => res && evalProg(cj).asBoolFalseIfBottom })
    case Match(v, r, k) =>
      val s = input(v.index)
      ???
    case NotMatch(v, r, k) =>
      val s = input(v.index)
      ???
    case Concatenate(ls) =>
      concatenate(ls.toList map evalProg)
    case Loop(w, e) =>
      loopR(w, e, 1)
    case SubStr(v1, p1, p2) =>
      if(v1.index >= input.length) return BottomValue
      val s = input(v1.index)
      val i1 = evalProg(p1)(IndexedSeq(s))
      val i2 = evalProg(p2)(IndexedSeq(s))
      i1 match {
        case IntValue(n1) =>
          i2 match {
            case IntValue(n2) =>
              StringValue(s.substring(n1, n2))
            case _ => BottomValue
          }
        case _ => BottomValue
      }
      
    case ConstStr(s) => StringValue(s)
    case CPos(k) if k >= 0 => IntValue(k)
    case CPos(k) if k < 0 => IntValue(input(0).length + k + 1)
    case Pos(r1, r2, c) => val s = input(0)
    // TODO : the position for regular expressions
    // TODO : Precompute automatas.
    val res1 = RegexpPositionsInString.computePositionsEndingWith(r1, s).map(_ + 1)
    val res2 = RegexpPositionsInString.computePositionsEndingWith(r2.reverse, s.reverse).reverse.map(s.length-1 - _) // TODO : Check this
    val intersections = res1 intersect res2
    val IntValue(i) = evalProg(c)
    if(i >= 1 && intersections.length > i-1) {
      IntValue(intersections(i-1))
    } else if(i <= -1 && intersections.length > -i+1) {
      IntValue(intersections(-i+1))
    } else {
      BottomValue
    }
    case IntLiteral(i) => IntValue(i)
  }
}