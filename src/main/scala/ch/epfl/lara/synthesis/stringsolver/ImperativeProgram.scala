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

/**
 * ImperativeProgram(Program) produces an imperative version of the code.
 * Assigned expressions can be strings, numbers, booleans, operations on them.
 * 
 */
object ImperativeProgram {
  
  def main(args: Array[String]): Unit = {
    val c = StringSolver()
    c.setVerbose(true)
    c.setTimeout(6)
    c.add("let us reverse this sentence -> Sentence This...")
    c.add("ab bcd cd defg -> Defg Cd Bcd Ab")
    println(Scalafication(ImperativeProgram(c.solve().get)))
    //c.add("a b c d e f -> f e d c b a")
    //println("Adding the second example")
    //c.add("what should we do with this -> this with do we should what")
    println(Scalafication(ImperativeProgram(c.solve().get)))
  }
  
  import Program.{Identifier => PIdentifier, _}
  sealed trait Tree {
    def toBash(): String = ???
    def toBatch(): String = ???
    def toPowershell(): String = ???
    def toVBScript(): String = ???
    var comment: String = ""
    def withComment(s: String): this.type = { comment = s; this }
  }
  sealed trait Stat extends Tree
  sealed trait Expr extends Tree {
    def !=(other: Expr) = NotEq(this, other)
    def +(other: Expr) = Plus(this, other)
    def *(other: Expr) = Times(this, other)
    def ||(other: Expr) = Or(this, other)
    def &&(other: Expr) = And(this, other)
    def unary_! = Not(this)
  }
  
  case class Script(stats: Stat, return_ident: Identifier) extends Tree {
    def toScala() = Scalafication(this)
  }
  object Block {
    def apply(stats: Stat*): Stat = if(stats.length == 1) stats.head else apply(stats.toList.flatMap({ case Block(l) => l case e => List(e) }))
  }
  case class Block(stats: List[Stat]) extends Stat {
    def apply(other: Block): Stat = Block(stats ++ other.stats : _*)
  }
  //case class For(i: Identifier, low: Expr, up: Expr, body: Stat) extends Stat
  object While {
    def apply(cond: Expr)(body: Stat*):While = apply(cond, if(body.length == 1) Block(body.head) else Block(body.toList : _*))
  }
  case class While(e: Expr, body: Stat) extends Stat
  object If {
    def apply(cond: Expr)(body: Stat*): If = If(cond, Block(body.toList), None)
  }
  case class If(cond: Expr, thn: Stat, els: Option[Stat]) extends Stat {
    def Else(stats: Stat*): If = {
      If(cond, thn, Some(Block(stats.toList)))
    }
  }
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
  case class And(a: Expr, b: Expr) extends Expr
  case class Concat(a: Expr, b: Expr) extends Expr // String Concatenation
  case class FormatNumber(a: Expr, nDigits: Int, offset: Int) extends Expr
  case class InputExpr(e: Expr) extends Expr
  case class SubString(e: Expr, start: Expr, end: Expr, mode: SubStrFlag) extends Expr
  case class LowerCase(e: Expr) extends Expr
  case class UpperCase(e: Expr) extends Expr
  case class InitialCase(e: Expr) extends Expr
  case class ToInt(e: Expr) extends Expr
  case class PositionBetweenRegex(e1: String, e2: String, i: Expr) extends Expr
  case class Not(a: Expr) extends Expr
  
  var i: Int = 0
  def newVar(): Identifier = { i += 1; Identifier("res" + i) }
  val index = Identifier("index") // Special index which starts at 0.
  
  case class Options(starting: Boolean = true) {
    
  }
  
  def apply(p: Program) = {
    val ret = newVar()
    Script(fromProgram(p, ret, true), ret).withComment(Printer(p))
  }
  
  /**
   * Converts a program to an expression
   */
  private def fromProgramExpr(p: Program)(implicit opt: Options = Options()): Expr = {
    p match {
      case NumberMap(s@SubStr(InputString(_), r1, r2, m), size, offset) =>
        FormatNumber(fromProgramExpr(s), size, offset)
      case SubStr(InputString(v1), p1, p2, mm) =>
        SubString(InputExpr(fromProgramExpr(v1)), fromProgramExpr(p1), fromProgramExpr(p2), mm)
      case Counter(digits, start, step) =>
        FormatNumber(index*step, digits, start)
      case Linear(1, w, 0) =>
        fromProgramExpr(w)
      case Linear(i, w, 0) =>
        fromProgramExpr(i) * fromProgramExpr(w)
      case Linear(1, w, j) =>
        fromProgramExpr(w) + fromProgramExpr(j)
      case Linear(i, w, j) =>
        fromProgramExpr(i) * fromProgramExpr(w) + fromProgramExpr(j)
      case Pos(r1, r2, i) =>
        PositionBetweenRegex(fromProgramExpr(r1)(opt.copy(starting = false)).asInstanceOf[StringLit].s, fromProgramExpr(r2)(opt.copy(starting = true)).asInstanceOf[StringLit].s, fromProgramExpr(i))
      case t @ TokenSeq(l) =>
        ScalaRegExp.convertRegExp(t, starting = opt.starting).toString
        //StringLit(l map ScalaRegExp.convertToken mkString "")
      case t: Token => StringLit(ScalaRegExp.convertToken(t))
      case CPos(i) => i
      case IntLiteral(k) => k
      case PIdentifier(v) =>
        Identifier(v)
      case ConstStr(s) =>
        StringLit(s)
      case e => throw new Exception(s"Conversion not implemented for $e")
    }
  }
  
  private def fromProgram(p: Program, return_identifier: Identifier, initialize_return_identifier: Boolean): Stat = {
    p match {
      case Loop(w, c, separator) =>
        val i = Identifier(w.value)
        val s = return_identifier
        val r = Identifier(w.value + "_ret")
        val first = Identifier(w.value + "_first")
        Block(
        (if(initialize_return_identifier) s ::= "" else s := ""),
        r ::= "",
        i ::= 0,
        first ::= true,
        While(first || NotEq(r, ""))(
          fromProgram(c, r, false).asInstanceOf[Stat],
          if(separator != None) If(!first && NotEq(r, ""))(s := Concat(s, StringLit(separator.get.s))) else Block(),
          s := Concat(s, r),
          first := false,
          i := i + 1
        ))
      /*case Concatenate(List(s)) =>
        fromProgram(s, return_identifier, initialize_return_identifier)*/
      case Concatenate(fs) =>
        val s = return_identifier
        val ret = newVar()
        val lfs = fs flatMap { prog => fromProgram(prog, ret, false) match {
          case st: Stat => List(st, s := Concat(s, ret))
          case _ => Nil
        }}
        Block((if(initialize_return_identifier) s ::= "" else s := "")::(ret ::= "")::lfs : _*)
      case SpecialConversion(s, p) =>
        throw new Exception("Special conversion not supported yet for conversion")
      case e => Block(if(initialize_return_identifier) return_identifier ::= fromProgramExpr(e) else return_identifier := fromProgramExpr(e))
    }
  }
}

/**
 * Transforms an imperative-like program to a valid scala program.
 */
object Scalafication {
  import ImperativeProgram._
  def apply(t: Script): String = {
    fromScript(t)
  }
  case class Options(ret_ident: Option[Identifier] = None, declare_ret_ident: Boolean = false, input: Identifier = null)
  
  def fromScript(t: Tree, opt: Options = Options())(implicit indent: String = ""): String = t match {
    case t@Script(stat, expr) => 
      (if(t.comment != "") {
        "// "+t.comment + "\n"
      } else "") +
      "def script(args: Array[String], index: Int = 0): String = " + fromScript(t.stats, Options(ret_ident = Some(expr), declare_ret_ident=true))
    case Block(s) => var ss = indent + "{\n"
      ss +=  ((for(t <- s) yield {
        var res = t
        val tt = fromScript(t, Options())(indent = "  " + indent)
        tt
      }) mkString "\n")
    ss += "\n"
    ss += (opt.ret_ident match {
      case Some(r) => "  " + indent + fromScript(r) + "\n" + indent + "}"
      case None => indent + "}"
    })
    ss
    case While(cond, expr) => indent + "while("+fromScript(cond)+") " + fromScript(expr)
    case If(cond, thn, els) => indent + "if("+fromScript(cond)+") " + fromScript(thn) + (els match { case Some(a) => " else " + fromScript(a) case None => ""})
    
    case Assign(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i, e) => indent + fromScript(i) + " = " + fromScript(e)
    case VarDecl(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e) => indent + "var " + fromScript(i) + " = " + fromScript(e)
    case Identifier(i) => i
    case RegularExpression(s) => s
    case StringLit(s) => "\""+s+"\""
    case IntLit(i) => opt.ret_ident match {
      case Some(Identifier(p1)) =>
        if((opt.input ne null) && i < 0) {
          indent + s"val $p1 = ${opt.input.a}.length" + (if(i != -1) " + " + (i+1).toString else "")
        } else {
          indent + s"val $p1 = " + i.toString
        }
      case None => i.toString
    }
    case BoolLit(i) => i.toString
    case Not(a) => "!" + fromScript(a)
    case NotEq(a, b) => "(" + fromScript(a) + " != " + fromScript(b) + ")"
    case Plus(a, b) => "(" + fromScript(a) + " + " + fromScript(b) + ")"
    case Times(a, b) => "(" + fromScript(a) + " * " + fromScript(b) + ")"
    case Or(a, b) => "(" + fromScript(a) + " || " + fromScript(b) + ")"
    case And(a, b) => "(" + fromScript(a) + " && " + fromScript(b) + ")"
    case Concat(a, b) => "(" + fromScript(a) + "+" + fromScript(b) + ")"
    case FormatNumber(a, n, off) =>
      val ret_expr = if(opt.ret_ident != None) (if(opt.declare_ret_ident) "val " else "")+fromScript(opt.ret_ident.get)+" = " else ""
      val si = newVar()
      val s = si.a
      val offset = if(n > 0) ".toInt+"+off.toString else if(n < 0) ".toInt-" + n.toString else ""
      fromScript(a, opt.copy(ret_ident=Some(si),declare_ret_ident=true)) + "\n" +
      indent + s"${ret_expr}" + "f\"${" + s"$s" + offset + "}%0" + n + "d\""
    case InputExpr(a) => 
      val dropped = fromScript(a, Options())
      val dropped_string = if(dropped == "0") "" else s".drop($dropped)"
      val tq = "\""
      s"args$dropped_string.headOption.getOrElse($tq$tq)"
    case SubString(e, pbr1, pbr2, mode) => 
      val ret_expr = if(opt.ret_ident != None) (if(opt.declare_ret_ident) "val " else "")+fromScript(opt.ret_ident.get)+" = " else ""
      val si = newVar()
      val s = si.a
      val p1 = s+"Start"
      val p2 = s+"End"
      val i1 = s+"i"
      val i2 = s+"j"
      val tq = "\""
      import Program.NORMAL
      import Program.CONVERT_LOWERCASE
      import Program.CONVERT_UPPERCASE
      import Program.UPPERCASE_INITIAL
      val mm = mode match {
        case NORMAL => ""
        case CONVERT_LOWERCASE =>  ".toLowerCase()"
        case CONVERT_UPPERCASE =>  ".toUpperCase()"
        case UPPERCASE_INITIAL =>  ".map{ var first = true; (e: Char) => if(first) {first = false; e.toUpper} else e }"
      }
      indent + s"val $s = " + fromScript(e) + "\n" +
      fromScript(pbr1, opt.copy(ret_ident = Some(Identifier(p1)), input=si)) + "\n" +
      fromScript(pbr2, opt.copy(ret_ident = Some(Identifier(p2)), input=si)) + "\n" +
      indent + s"""${ret_expr}(if($p1 >= 0 && $p2 >= 0 && $p1 <= $s.length && $p2 <= $s.length) $s.substring($p1, $p2) else $tq$tq)$mm"""
    case LowerCase(e) => fromScript(e) + ".toLowerCase()"
    case UpperCase(e) => fromScript(e) + ".toUpperCase()"
    case InitialCase(e) =>  fromScript(e) + ".map{ var first = true; (e: Char) => if(first) {first = false; e.toUpper} else e }"
    case ToInt(e) => "("+fromScript(e)+").toInt"
    case PositionBetweenRegex(r1, r2, i) => 
       val i1 = newVar().a
       val p1 = opt.ret_ident.get.a
       val tq = "\""
       val s = opt.input.a
       indent + s"val $i1 = " + fromScript(i) + "\n" +
       indent + s"""val $p1 = $tq$tq$tq$r1$tq$tq$tq.r.findAllMatchIn($s).map(_.end(0)).toList.intersect($tq$tq$tq$r2$tq$tq$tq.r.findAllMatchIn($s).map(_.start(0)).toList) match { case l if l.length >= $i1 && $i1 >= 1 => l($i1-1) case l if l.length + $i1 >= 0 && $i1 <= -1 => l($i1 + l.length) case _ => -1 } \n"""
    case _ => throw new Exception(s"Impossible to parse expression $t")
      //i1 r1.r.findAllMatchIn(s).map(_.start(0)).toList
      //
  }
}