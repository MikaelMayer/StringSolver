package ch.epfl.lara.synthesis.flashfill

import Programs._

trait ComputePositionsInString {
  /**
   * Computes the list of positions where there exists a word recognized by this regexp.
   */
  def computePositionsEndingWith(r: RegExp, s: String): List[Int]
  def computePositionsStartingWith(r: RegExp, s: String): List[Int]
  //def computePositionsOfToken(r: Token, s: String): List[(Int, Int)]
}

object Evaluator {
  //import ctx.reporter._
  import Implicits._
  
  final val RegexpPositionsInString: ComputePositionsInString = ScalaRegExp

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
  case object ⊥ extends Value {
    override def isBottom = true
  }
  
  /**
   * Concatenates string values
   */
  def concatenate(ls: List[Value]): Value = {
    ((StringValue(""): Value) /: ls) {
      case (StringValue(a), StringValue(b)) => StringValue(a + b)
      case (_, _) => ⊥
    }
  }
  def concatenate(s: Value*): Value = concatenate(s.toList)
  
  /**
   * Replace routines
   */
  def replaceTraceExpr(e: TraceExpr)(implicit w: Identifier, k: Int): TraceExpr = e match {
    case Concatenate(fs) =>    Concatenate(fs.toList map replaceAtomicExpr)
  }
  def replaceAtomicExpr(e: AtomicExpr)(implicit w: Identifier, k: Int): AtomicExpr = e match {
    case SubStr(vi, p1, p2) => SubStr(replaceIntegerExpr(vi), replacePosition(p1), replacePosition(p2))
    case ConstStr(s) => e
    case Loop(w2, _) if w2 == w => e
    case Loop(w2, e) => Loop(w2, replaceTraceExpr(e))
    case Number(s, l, ostep) => Number(replaceAtomicExpr(s), l, ostep)
  }
  def replacePosition(e: Position)(implicit w: Identifier, k: Int): Position = e match {
    case Pos(p1, p2, t) => Pos(p1, p2, replaceIntegerExpr(t))
    case _ => e
  }
  def replaceIntegerExpr(e: IntegerExpr)(implicit w: Identifier, k: Int): IntegerExpr = e match {
    case Linear(k1, v, k2) if v.value == w.value => IntLiteral(k * k1 + k2)
    case _ => e
  }
    
  /**
   * Loop routines
   */
  def loopR(w: Identifier, e: TraceExpr, k: Int)(implicit input: IndexedSeq[String]): Value = {
    val t = evalProg(replaceTraceExpr(e)(w, k))
    t match {
      case ⊥ => StringValue("")
      case StringValue(s) => concatenate(List(t, loopR(w, e, k+1))) // TODO : Correct the stack overflow error.
      case _ => ⊥
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
          ⊥
      }
    case Bool(ds) =>
      BoolValue((false /: ds) { case (res, cj) => res || evalProg(cj).asBoolFalseIfBottom })
    case Conjunct(pis) =>
      BoolValue((true /: pis) { case (res, cj) => res && evalProg(cj).asBoolFalseIfBottom })
    case Match(v, r, k) =>
      val s = input(evalProg(v).asInt)
      val res1 = RegexpPositionsInString.computePositionsEndingWith(r, s)
      BoolValue(res1.length >= k)
    case NotMatch(v, r, k) =>
      val s = input(evalProg(v).asInt)
      val res1 = RegexpPositionsInString.computePositionsEndingWith(r, s)
      BoolValue(res1.length < k)
    case Concatenate(ls) =>
      concatenate(ls.toList map evalProg)
    case Loop(w, e) =>
      loopR(w, e, 0)
    case SubStr(v1, p1, p2) =>
      val index = evalProg(v1).asInt
      if(index >= input.length) return ⊥
      val s = input(index)
      val i1 = evalProg(p1)(IndexedSeq(s))
      val i2 = evalProg(p2)(IndexedSeq(s))
      i1 match {
        case IntValue(n1) if n1 >= 0 =>
          i2 match {
            case IntValue(n2) if n2 <= s.length && n1 <= n2 =>
              StringValue(s.substring(n1, n2))
            case _ => ⊥
          }
        case _ => ⊥
      }
      
    case ConstStr(s) => StringValue(s)
    case CPos(k) if k >= 0 => IntValue(k)
    case CPos(k) if k < 0 => IntValue(input(0).length + k + 1)
    case Pos(r1, r2, c) => val s = input(0)
    val res1 = RegexpPositionsInString.computePositionsEndingWith(r1, s).map(_ + 1)
    val res2 = RegexpPositionsInString.computePositionsStartingWith(r2, s)
    val intersections = res1 intersect res2
    val IntValue(i) = evalProg(c)
    if(i >= 1 && intersections.length > i-1) {
      IntValue(intersections(i-1))
    } else if(i <= -1 && 0 <= intersections.length + i) {
      IntValue(intersections(intersections.length + i))
    } else {
       ⊥
    }
    case IntLiteral(i) => IntValue(i)
    case Number(a, size, (offset, step)) =>
    val i = evalProg(size)
    val o = IntValue(offset)
    val s = IntValue(step)
    (i, o, s) match { case (IntValue(ii), IntValue(oo), IntValue(ss)) =>
            evalProg(a) match {
              case StringValue(sv) =>
                if(sv.isNumber) {
                  val p = sv.toInt
                  val ps = (p+ss).toString
                  StringValue("0"*(ii - ps.size)+ps)
                } else ⊥
              case ⊥ => 
                val ps = oo.toString
                StringValue("0"*(ii - ps.size)+ps)
              case _ => ⊥
      }
    case _ =>  ⊥
    }
  }
}