/**
 *     _____ _       _         _____     _             
 *    |   __| |_ ___|_|___ ___|   __|___| |_ _ ___ ___ 
 *    |__   |  _|  _| |   | . |__   | . | | | | -_|  _|
 *    |_____|_| |_| |_|_|_|_  |_____|___|_|\_/|___|_|  
 *                        |___|      
 * 
 * File name: ImperativeProgram.scala
 * Author   : Mikaël Mayer
 * Date     : 14.02.2014
 * Function : Object to convert programs using a low level imperative structure
 *            using regular expressions
 *            which can be used later to produce other programming languages,
 *            such as bash, batch, vbscript or more.
 */
package ch.epfl.lara.synthesis.stringsolver
import  scala.language.implicitConversions

object ImperativeProgram {
  import Program.{Identifier => PIdentifier, _}
  sealed trait Tree {
    def toScala(): String = ???
    def toBash(): String = ???
    def toBatch(): String = ???
    def toPowershell(): String = ???
    def toVBScript(): String = ???
  }
  trait Stat extends Tree
  trait Expr extends Tree {
    def !=(other: Expr) = NotEq(this, other)
    def +(other: Expr) = Plus(this, other)
    def *(other: Expr) = Times(this, other)
    def ||(other: Expr) = Or(this, other)
  }
  
  case class Script(stats: Block) extends Tree
  object Block {
    def apply(stats: Stat*): Block = apply(stats.toList.flatMap({ case Block(l) => l case e => List(e) }))
  }
  case class Block(stats: List[Stat]) extends Stat {
    def apply(other: Block) = Block(stats ++ other.stats)
  }
  case class For(i: Identifier, low: Expr, up: Expr, body: Stat) extends Stat
  object While {
    def apply(cond: Expr)(body: Stat*):While = apply(cond, if(body.length == 1) body.head else Block(body.toList))
  }
  case class While(e: Expr, body: Stat) extends Stat
  case class If(cond: Expr, thn: Stat, els: Option[Stat]) extends Stat
  case class Assign(i: Identifier, e: Expr) extends Stat
  
  case class VarDecl(i: Identifier, e: Expr) extends Stat
  case class Identifier(a: String) extends Expr {
    def :=(other: Expr) = Assign(this, other)
    def ::=(other: Expr) = VarDecl(this, other)
  }
  case class RegularExpression(s: String) extends Expr
  case class StringLit(s: String) extends Expr ; implicit def toStringLit(i: String):StringLit = StringLit(i)
  case class IntLit(s: Int) extends Expr ; implicit def toIntLit(i: Int):IntLit = IntLit(i)
  case class NotEq(a: Expr, b: Expr) extends Expr
  case class BoolLit(b: Boolean) extends Expr ; implicit def toBoolLit(i: Boolean):BoolLit = BoolLit(i)
  case class Plus(a: Expr, b: Expr) extends Expr
  case class Times(a: Expr, b: Expr) extends Expr
  case class Or(a: Expr, b: Expr) extends Expr
  case class Concat(a: Expr, b: Expr) extends Expr // String Concatenation
  case class FormatNumber(a: Expr, nDigits: Int) extends Expr
  case class InputExpr(e: Expr) extends Expr
  case class SubString(e: Expr, start: Expr, end: Expr) extends Expr
  case class LowerCase(e: Expr) extends Expr
  case class UpperCase(e: Expr) extends Expr
  case class InitialCase(e: Expr) extends Expr
  case class ToInt(e: Expr) extends Expr
  case class PositionBetweenRegex(e1: String, e2: String, i: Expr) extends Expr
  
  var i: Int = 0
  def newVar(): Identifier = { i += 1; Identifier("res" + i) }
  val index = Identifier("index") // Special index which starts at 0.
  
  /**
   * Converts a program to an expression
   */
  def fromProgramExpr(p: Program): Expr = {
    p match {
      case NumberMap(s@SubStr(InputString(_), r1, r2, m), size, offset) =>
        FormatNumber(Plus(ToInt(fromProgramExpr(s)), offset), size)
      case SubStr(InputString(v1), p1, p2, mm) =>
        val res = SubString(InputExpr(fromProgramExpr(v1)), fromProgramExpr(p1), fromProgramExpr(p2))
        mm match {
          case NORMAL => res
          case CONVERT_LOWERCASE => LowerCase(res)
          case CONVERT_UPPERCASE => UpperCase(res)
          case UPPERCASE_INITIAL => InitialCase(res)
        }
      case Counter(digits, start, step) =>
        FormatNumber(index*step + start, digits)
      case Linear(i, w, j) =>
        fromProgramExpr(i) * fromProgramExpr(w) + fromProgramExpr(j)
      case Pos(r1, r2, i) =>
        PositionBetweenRegex(fromProgramExpr(r1).asInstanceOf[StringLit].s, fromProgramExpr(r2).asInstanceOf[StringLit].s, fromProgramExpr(i))
      case TokenSeq(l) =>
        StringLit(l map ScalaRegExp.convertToken mkString "")
      case t: Token => StringLit(ScalaRegExp.convertToken(t))
      case CPos(i) => i
      case IntLiteral(k) => k.toString
      case PIdentifier(v) =>
        Identifier(v)
      case ConstStr(s) =>
        StringLit(s)
      case e => throw new Exception(s"Conversion not implemented for $e")
    }
  }
  
  def fromProgram(p: Program, return_identifier: Option[Identifier] = None): Tree = {
    // TODO : Maybe consider direct concatenation instead of assigment
    def return_block(s: Expr): Block = Block(return_identifier match { case Some(r) => List(r := s) case _ => Nil })
    // Find variable declarations.
    p match {
      case Loop(w, c, separator) =>
        val i = Identifier(w.value)
        val s = Identifier(w.value + "_str")
        val r = Identifier(w.value + "_ret")
        val first = Identifier(w.value + "_first")
        Block(
        s ::= "",
        r ::= "",
        i ::= 0,
        first ::= true,
        While(first || r != "")(
          fromProgram(c, Some(r)).asInstanceOf[Stat],
          s := Concat(s, r),
          first := false,
          i := i + 1
        ))(return_block(s))
      case Concatenate(fs) =>
        val s = newVar()
        val ret = newVar()
        val lfs = fs flatMap { prog => fromProgram(prog, Some(ret)) match {
          case st: Stat => List(st, s := Concat(s, ret))
          case _ => Nil
        }}
        Block(lfs)(return_block(s))
      case SpecialConversion(s, p) =>
        throw new Exception("Special conversion not supported yet for conversion")
      case e => return_block(fromProgramExpr(p))
    }
  }
}

/**
 * Transforms an imperative-like program to a scala program.
 */
object Scalafication {
  import ImperativeProgram._
  def apply(t: Script): String = {
    fromScript(t)
  }
  def fromScript(t: Tree): String = t match {
    case t@Script(_) => "def main(args: Array[String]): String = " + fromScript(t.stats)
    case Block(s) => "{\n" + (s map fromScript mkString "\n") + "}"
    case While(cond, expr) => "while("+fromScript(cond)+") " + fromScript(expr)
    case If(cond, thn, els) => "if("+fromScript(cond)+") " + fromScript(thn) + (els match { case Some(a) => " else " + fromScript(a) case None => ""})
    case Assign(i, e) => fromScript(i) + " = " + fromScript(e)
    case VarDecl(i, e) => "var " + fromScript(i) + " = " + fromScript(e)
    case Identifier(i) => i
    case RegularExpression(s) => s
    case StringLit(s) => "\""+s+"\""
    case IntLit(i) => i.toString
    case BoolLit(i) => i.toString
    case NotEq(a, b) => "(" + fromScript(a) + " != " + fromScript(b) + ")"
    case Plus(a, b) => "(" + fromScript(a) + " + " + fromScript(b) + ")"
    case Times(a, b) => "(" + fromScript(a) + " * " + fromScript(b) + ")"
    case Or(a, b) => "(" + fromScript(a) + " || " + fromScript(b) + ")"
    case Concat(a, b) => "(" + fromScript(a) + "+" + fromScript(b) + ")"
    case FormatNumber(a, n) => "\"${"+fromScript(a)+"}%"+n+"d\""
    case InputExpr(a) => "args("+fromScript(a)+")"
    case SubString(e, start, end) => fromScript(e) + ".substring(" + fromScript(start) + "," + fromScript(end) + ")"
    case LowerCase(e) => fromScript(e) + ".toLowerCase()"
    case UpperCase(e) => fromScript(e) + ".toUpperCase()"
    case InitialCase(e) =>  fromScript(e) + ".map{ var first = true; (e: Char) => if(first) {first = false; e.toUpper} else e }"
    case ToInt(e) => "("+fromScript(e)+").toInt"
    case PositionBetweenRegex(r1, r2, i) => "{val i1 = }" // TODO
  }
}