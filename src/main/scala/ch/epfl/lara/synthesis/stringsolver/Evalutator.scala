package ch.epfl.lara.synthesis.stringsolver

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
  import StringSolver._
  
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
  def concatenate(s: Value*): Value = concatenate(s.toList)
  
  /**
   * Replace routines
   */
  /*def replaceTraceExpr(e: TraceExpr)(implicit w: Identifier, k: Int): TraceExpr = e match {
    case Concatenate(fs) =>    Concatenate(fs.toList map replaceAtomicExpr)
  }
  def replaceAtomicExpr(e: AtomicExpr)(implicit w: Identifier, k: Int): AtomicExpr = e match {
    case SubStr(vi, p1, p2, m) => SubStr(replaceStringVariable(vi), replacePosition(p1), replacePosition(p2), m)
    case ConstStr(s) => e
    case Loop(w2, _, separator) if w2 == w => e
    case Loop(w2, e, separator) => Loop(w2, replaceTraceExpr(e), separator)
    case Counter(digits, start, offset) => e
    case NumberMap(s, l, offset) => NumberMap(replaceAtomicExpr(s).asInstanceOf[SubStr], l, offset)
  }
  def replacePosition(e: Position)(implicit w: Identifier, k: Int): Position = e match {
    case Pos(p1, p2, t) => Pos(p1, p2, replaceIntegerExpr(t))
    case _ => e
  }
  def replaceStringVariable(e: StringVariable)(implicit w: Identifier, k: Int): StringVariable = e match {
    case InputString(i) => InputString(replaceIntegerExpr(i))
    //case PrevStringNumber(i) => PrevStringNumber(replaceIntegerExpr(i))
  }
  def replaceIntegerExpr(e: IntegerExpr)(implicit w: Identifier, k: Int): IntegerExpr = e match {
    case Linear(k1, v, k2) if v.value == w.value && (!(k2 >= 0) || k*k1+k2 >= 0) >= 0=> IntLiteral(k * k1 + k2)
    case _ => e
  }*/
    
  /**
   * Loop routines
   */
  def loopR(w: Identifier, e: TraceExpr, k: Int, separator: StringValue, first: Boolean = true)(implicit input: Input_state, context: Option[Map[String, Int]]): Value = {
    val t = evalProg(e)(input, Some(context.getOrElse(Map()) + (w.value -> k)))
    if(k > 50) {
      println("Bug in loopR?")
    }
    t match {
      case BottomValue => StringValue("")
      case StringValue(s) => 
        if(first) {
          concatenate(List(t, loopR(w, e, k+1, separator, false)))
        } else {
          concatenate(List(separator, t, loopR(w, e, k+1, separator, false)))
        }
      case _ => BottomValue
    }
  }
  
  /**
   * Evaluates a program given an input.
   */
  def evalProg(p: Program)(implicit input: Input_state, context: Option[Map[String, Int]] = Some(Map())): Value = p match {
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
    case Match(InputString(v), r, k) =>
      val s = input.inputs(evalProg(v).asInt)
      val res1 = RegexpPositionsInString.computePositionsEndingWith(r, s)
      BoolValue(res1.length >= k)
    case NotMatch(InputString(v), r, k) =>
      val s = input.inputs(evalProg(v).asInt)
      val res1 = RegexpPositionsInString.computePositionsEndingWith(r, s)
      BoolValue(res1.length < k)
    case c@Concatenate(ls) =>
      
      concatenate(ls.toList map evalProg) match {
        case BottomValue => // Reconsider other alternatives
          if(!c.getAlternatives.isEmpty) {
            var result: Value = BottomValue
            c.getAlternatives.find { alternative =>
              evalProg(alternative) match {
                case BottomValue => false
                case sv@StringValue(_) => result = sv; true
              }
            }
            result
          } else BottomValue
        case e => e
      }
    case l @ Loop(w, e, separator) =>
      separator map (e => evalProg(e)) match {
        case Some(s@StringValue(v)) =>
          loopR(w, e, l.startIndex, s)
        case None =>
          loopR(w, e, l.startIndex, StringValue(""))
        case _ =>
          BottomValue
      }
    case e @ SubStr(InputString(v1), p1, p2, m) =>
      evalProg(v1) match {
        case IntValue(index) =>
          if(index >= input.inputs.length) return BottomValue
          val s = input.inputs(index)
          val i1 = evalProg(p1)(Input_state(IndexedSeq(s), input.position), context)
          val i2 = evalProg(p2)(Input_state(IndexedSeq(s), input.position), context)
          val res = i1 match {
            case IntValue(n1) if n1 >= 0 =>
              i2 match {
                case IntValue(n2) if n2 <= s.length && n1 <= n2 =>
                  StringValue(m(s.substring(n1, n2)))
                case _ => BottomValue
              }
            case _ => BottomValue
          }
          if(res == BottomValue && !e.getAlternatives.isEmpty) {
            var result: Value = BottomValue
            e.getAlternatives.find { alternative =>
              evalProg(alternative) match {
                case BottomValue => false
                case sv@StringValue(_) => result = sv; true
              }
            }
            result
          } else res
        case _ => BottomValue
      }
      
    /*case SubStr(PrevStringNumber(v1), p1, p2, m) =>
      val index = evalProg(v1).asInt
      if(index >= input.prevNumberOutputs.length) return BottomValue
      val s = input.prevNumberOutputs(index)
      val i1 = evalProg(p1)(IndexedSeq(s)) // Handle s as a regular input for computing the position
      val i2 = evalProg(p2)(IndexedSeq(s))
      i1 match {
        case IntValue(n1) if n1 >= 0 =>
          i2 match {
            case IntValue(n2) if n2 <= s.length && n1 <= n2 =>
              StringValue(m(s.substring(n1, n2)))
            case _ => BottomValue
          }
        case _ => BottomValue
      }*/
    case ConstStr(s) => StringValue(s)
    case CPos(k) if k >= 0 => IntValue(k)
    case CPos(k) if k < 0 => IntValue(input.inputs(0).length + k + 1)
    case Pos(r1, r2, c) => val s = input.inputs(0)
    val res1 = RegexpPositionsInString.computePositionsEndingWith(r1, s).map(_ + 1)
    val res2 = RegexpPositionsInString.computePositionsStartingWith(r2, s)
    val intersections = res1 intersect res2
    evalProg(c) match {
      case IntValue(i) =>
        if(i >= 1 && intersections.length > i-1) {
          IntValue(intersections(i-1))
        } else if(i <= -1 && 0 <= intersections.length + i) {
          IntValue(intersections(intersections.length + i))
        } else {
           BottomValue
        }
      case e =>
        BottomValue
    }
    case IntLiteral(i) => IntValue(i)
    case Linear(k1, v, k2) =>
      //if(context contains v.value) {
        context.head.get(v.value) match {
          case Some(k) =>
             if(!(k2 >= 0) || k*k1+k2 >= 0) {
               IntValue(k * k1 + k2)
             } else BottomValue // Do not authorize to produce negative numbers if the base is greater than zero.            
          case None => BottomValue
        }
      //} else BottomValue
    
    case Counter(size, start, offset) =>
      val res = (input.position*offset + start).toString
      StringValue("0"*(size - res.length) + res)
    case NumberMap(a, size, offset) =>
    val i = IntValue(size)
    val o = IntValue(offset)
    (i, o) match { case (IntValue(ii), IntValue(oo)) =>
            evalProg(a) match {
              case StringValue(sv) if sv != "" =>
                if(sv.isNumber) {
                  val p = sv.toInt
                  val ps = (p+offset).toString
                  StringValue("0"*(ii - ps.size)+ps)
                } else BottomValue
              case BottomValue | StringValue("") => 
                BottomValue
                /*a match {
                  case SubStr(InputString(v), _, _, _) => BottomValue
                  case SubStr(PrevStringNumber(_), _, _, _) => 
                    val ps = oo.toString
                    StringValue("0"*(ii - ps.size)+ps)
                }*/
                
              case _ => BottomValue
      }
    case _ =>  BottomValue
    }
  }
}