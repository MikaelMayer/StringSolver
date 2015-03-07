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

import scala.language.implicitConversions

/**
 * ImperativeProgram(Program) produces an imperative version of the code.
 * Assigned expressions can be strings, numbers, booleans, operations on them.
 * 
 */
object ImperativeProgram {
  import java.io._
   def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}
  
  /** Allows the user to do something like program.toBash to "script.sh"*/
  implicit class StringExt(s: String) {
    def to(filename: String) {
      printToFile(new java.io.File(filename))(p => p.println(s))
    }
  }
  
  def exportScala(c: Script, name: String): Unit = {
   
    printToFile(new File(name+".scala"))(p => {
      p.println("package test\nclass Test {")
      p.println(c.toScala)
      p.println("}")})
  }
  /**
  def runScalaProgram(name: String, input: Array[String], index: Int = 0): String = {
    import java.io.File
    import scala.reflect.runtime._
		val cm = universe.runtimeMirror(getClass.getClassLoader)
		import scala.tools.reflect.ToolBox
		val tb = cm.mkToolBox()
		val q = "\""
		var start = ("class Test { \n" /: scala.io.Source.fromFile(new File(name + ".scala")).getLines.toArray){ case (b, l) => b + "\n" + l }
		val c1= start+" }; new Test.script("
		val c2 = c1 + input.map(i => "\"\"\"" + i + "\"\"\"")
		val c3 = c2 + ", " + index + ")"
		tb.eval(tb.parse( c3 )) match {
		  case result: String => result
		  case _ => "<nothing>"
		}
  }
  
  val c = StringSolver() ; c.add("file499.pdf -> 01file.pdf") ; c.add("report_761.pdf -> 02report.pdf") ; c.add("credits##.pdf -> 03credits.pdf"); var k = c.solve().get ; var ki = ImperativeProgram(k);
  ki.toBash to "script.sh"
  **/
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
    def toBash() = Bashification(this)
    def toPowerShell() = Powershellification(this)
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
  case class CustomStat(s: String) extends Stat
  
  case class VarDecl(i: Identifier, e: Expr) extends Stat
  case class ArrayAdd(i: Expr, e: Expr) extends Stat
  case class Identifier(name: String) extends Expr {
    def :=(other: Expr) = Assign(this, other)
    def ::=(other: Expr) = VarDecl(this, other)
  }
  case class StringLit(s: String) extends Expr //implicit def toStringLit(i: String):StringLit = StringLit(i)
  case class EmptyArrayLit() extends Expr //implicit def toStringLit(i: String):StringLit = StringLit(i)
  case object InputsLength extends Expr
  case class IndexOf(array: Identifier, value: Identifier) extends Expr
  case class ArrayGet(array: Identifier, Index: Identifier) extends Expr
  case class IntLit(s: Int) extends Expr ; implicit def toIntLit(i: Int):IntLit = IntLit(i)
  case class NotEq(a: Expr, b: Expr) extends Expr
  case class Eq(a: Expr, b: Expr) extends Expr
  case class BoolLit(b: Boolean) extends Expr ; implicit def toBoolLit(i: Boolean):BoolLit = BoolLit(i)
  case class Plus(a: Expr, b: Expr) extends Expr
  case class Times(a: Expr, b: Expr) extends Expr
  case class Or(a: Expr, b: Expr) extends Expr
  case class And(a: Expr, b: Expr) extends Expr
  case class Concat(a: Expr, b: Expr) extends Expr // String Concatenation
  case class FormatNumber(a: Expr, nDigits: Int, offset: Int) extends Expr
  case class InputExpr(e: Expr) extends Expr
  case class SubString(e: Expr, start: Expr, end: Expr, mode: SubStrFlag) extends Expr
 // case class LowerCase(e: Expr) extends Expr
 // case class UpperCase(e: Expr) extends Expr
 // case class InitialCase(e: Expr) extends Expr
  case class ToInt(e: Expr) extends Expr
  case class PositionBetweenRegex(e1: String, e2: String, i: Expr) extends Expr
  case class Not(a: Expr) extends Expr
  
  var i: Int = 0
  def newVar(): Identifier = { i += 1; Identifier("res" + i) }
  val index = Identifier("index") // Special index which starts at 0.
  
  case class Options(starting: Boolean = true, replaceInputStringByIdentifier: Option[Identifier] = None) {
  }
  
  def apply(p: Program): Script = {
    val ret = newVar()
    Script(fromProgram(p, ret, true), ret).withComment(Printer(p))
  }
  
  def apply(p: SplitProgram): Script = {
    val ret = newVar()
    Script(fromProgram(p, ret, true), ret).withComment(p.toString)
  }
  
  def apply(p: PartitionProgram): Script = {
    val ret = newVar()
    Script(fromProgram(p, ret, true), ret).withComment(p.toString)
  }
  
  def apply(p: FilterProgram): Script = {
    val ret = newVar()
    Script(fromProgram(p, ret, true), ret).withComment(p.toString)
  }
  
  /**
   * Converts a program to an expression
   */
  private def fromProgramExpr(p: Program)(implicit opt: Options = Options()): Expr = {
    p match {
      case NumberMap(s@SubStr(InputString(_), r1, r2, m), size, offset) =>
        FormatNumber(fromProgramExpr(s), size, offset)
      case InputString(v1) =>
        opt.replaceInputStringByIdentifier match {
          case Some(id) if v1 == IntLiteral(0) =>
            id
          case _ => 
            InputExpr(fromProgramExpr(v1))
        }
      case SubStr(is@InputString(v1), CPos(0), CPos(-1), NORMAL) =>
        fromProgramExpr(is)
      case SubStr(is@InputString(v1), p1, p2, mm) =>
        SubString(fromProgramExpr(is), fromProgramExpr(p1), fromProgramExpr(p2), mm)
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
        StringLit(ScalaRegExp.convertRegExp(t, starting = opt.starting).toString)
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
  
  private val error = Identifier("error")
  
  private def fromProgram(p: Program, return_identifier: Identifier, initialize_return_identifier: Boolean)(implicit opt: Options = Options()): Stat = {
    p match {
      case Loop(w, c, separator) =>
        val i = Identifier(w.value)
        val s = return_identifier
        val r = Identifier(w.value + "_ret")
        val first = Identifier(w.value + "_first")
        Block(
        (if(initialize_return_identifier) s ::= StringLit("") else s := StringLit("")),
        r ::= StringLit(""),
        i ::= 0,
        first ::= true,
        error ::= false,
        While(first || Not(error))(
          fromProgram(c, r, false).asInstanceOf[Stat],
          if(separator != None) If(!first && NotEq(r, StringLit("")) && Not(error))(s := Concat(s, StringLit(separator.get.s))) else Block(),
          If(Not(error))(s := Concat(s, r)),
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
        Block((if(initialize_return_identifier) s ::= StringLit("") else s := StringLit(""))::(ret ::= StringLit(""))::lfs : _*)
      case SpecialConversion(s, p) =>
        throw new Exception("Special conversion not supported yet for conversion")
      case e => Block(if(initialize_return_identifier) return_identifier ::= fromProgramExpr(e) else return_identifier := fromProgramExpr(e))
    }
  }
  
  
  private def fromProgram(p: SplitProgram, return_identifier: Identifier, initialize_return_identifier: Boolean): Stat = {
    val s = return_identifier
    val i = Identifier("index")
    val r = Identifier(return_identifier.name + "_ret")
    val first = Identifier(return_identifier.name + "_first")
    Block(
        (if(initialize_return_identifier) s ::= EmptyArrayLit() else s := EmptyArrayLit()),
        r ::= StringLit(""),
        i ::= 1,
        first ::= true,
        While(first || NotEq(r, StringLit("")))(
          fromProgram(p.p, r, false).asInstanceOf[Stat],
          ArrayAdd(s, r),
          first := false,
          i := i + 1
        ))
  }
  
  
  private def fromProgram(p: FilterProgram, return_identifier: Identifier, initialize_return_identifier: Boolean): Stat = {
    val s = return_identifier
    val r = Identifier(return_identifier.name + "_ret")
    val i = Identifier("index")
    val tmp = newVar()
    Block(
      (if(initialize_return_identifier) s ::= EmptyArrayLit() else s := EmptyArrayLit()),
      i ::= 0,
      While(NotEq(i, InputsLength))(
        tmp := InputExpr(i),
        fromProgram(p.determiningSubstring, r, false)(Options(replaceInputStringByIdentifier = Some(tmp))).asInstanceOf[Stat],
        If(Eq(r, StringLit(p.shouldEqual)))(ArrayAdd(s, tmp)),
        i := (i + 1)
      )
    )
  }
  
  private def fromProgram(partitionProgram: PartitionProgram, return_identifier: Identifier, initialize_return_identifier: Boolean): Stat = {
    val partitions = return_identifier
    val equivalenceClasses = newVar()
    val i = Identifier("indexInput")
    val p = Identifier("partitionpos")
    val n = Identifier("nclasses")
    val r = Identifier(return_identifier.name + "_ret")
    val tmp = newVar()
    val res = Block(
      (if(initialize_return_identifier) partitions ::= EmptyArrayLit() else partitions := EmptyArrayLit()),
      equivalenceClasses ::= EmptyArrayLit(),
      n ::= 0,
      i ::= 0,
      tmp ::= StringLit(""),
      While(NotEq(i, InputsLength))(
        tmp := InputExpr(i),
        fromProgram(partitionProgram.determiningSubstring, r, false)(Options(replaceInputStringByIdentifier = Some(tmp))).asInstanceOf[Stat],
        p := IndexOf(equivalenceClasses, r),
        If(Eq(p, -1))(
            ArrayAdd(equivalenceClasses, r),
            ArrayAdd(partitions, EmptyArrayLit()),
            p := n,
            n := n + 1),
        ArrayAdd(ArrayGet(partitions, p), tmp),
        i ::= (i + 1)
      )
    )
    res
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
  def prefixReturnExpr(s: String, id: Option[Identifier]): String = {
    (if(id != None) (fromScript(id.get)("")+" = ") else "") + s
  }
  
  def fromScript(t: Tree, opt: Options = Options())(implicit indent: String = ""): String = t match {
    case t@Script(stats, expr) => 
      (if(t.comment != "") {
        "// "+t.comment + "\n"
      } else "") +
      "import collection.mutable.ListBuffer; def script(args: Array[String], index: Int = 0) = " + fromScript(t.stats, Options(ret_ident = Some(expr), declare_ret_ident=true))
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
    case While(cond, expr) => indent + "while("+fromScript(cond)+")" + fromScript(expr)
    case If(cond, thn, els) => indent + "if("+fromScript(cond)+") " + fromScript(thn) + (els match { case Some(a) => " else " + fromScript(a) case None => ""})
    case InputsLength => "args.length"
    case Assign(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i, e) => indent + fromScript(i) + " = " + fromScript(e)
    case VarDecl(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e) => indent + "var " + fromScript(i) + " = " + fromScript(e)
    case IndexOf(array: Identifier, value: Identifier) => indent + prefixReturnExpr(array.name + ".indexOf(" + value.name + ")", opt.ret_ident)
    case ArrayGet(array: Identifier, index: Identifier) => indent + prefixReturnExpr(array.name + "(" + index.name + ")", opt.ret_ident)
    case ArrayAdd(i:Identifier, e: Identifier) =>
      indent + s"${i.name} += ${e.name}"
    case ArrayAdd(i:Identifier, e) => var res = newVar()
      fromScript(e, opt.copy(ret_ident = Some(res))) + " \n"+
      fromScript(ArrayAdd(i, res))
    case ArrayAdd(expr, e) => var res = newVar()
      fromScript(expr, opt.copy(ret_ident = Some(res)))+ "\n"+
      fromScript(ArrayAdd(res, e))
    case EmptyArrayLit() => indent + prefixReturnExpr("ListBuffer[String]()", opt.ret_ident)
    case Identifier(i) => i
    case StringLit(s) => "\""+s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")+"\""
    case IntLit(i) => opt.ret_ident match {
      case Some(Identifier(p1)) =>
        if((opt.input ne null) && i < 0) {
          indent + s"val $p1 = ${opt.input.name}.length" + (if(i != -1) " + " + (i+1).toString else "")
        } else {
          indent + s"val $p1 = " + i.toString
        }
      case None => i.toString
    }
    case BoolLit(i) => i.toString
    case Not(a) => "!" + fromScript(a)
    case NotEq(a, b) => "(" + fromScript(a) + " != " + fromScript(b) + ")"
    case Eq(a, b) => "(" + fromScript(a) + " == " + fromScript(b) + ")"
    case Plus(a, b) => "(" + fromScript(a) + " + " + fromScript(b) + ")"
    case Times(a, b) => "(" + fromScript(a) + " * " + fromScript(b) + ")"
    case Or(a, b) => "(" + fromScript(a) + " || " + fromScript(b) + ")"
    case And(a, b) => "(" + fromScript(a) + " && " + fromScript(b) + ")"
    case Concat(a, b) => "(" + fromScript(a) + "+" + fromScript(b) + ")"
    case FormatNumber(a, n, off) =>
      val ret_expr = if(opt.ret_ident != None) (if(opt.declare_ret_ident) "val " else "")+fromScript(opt.ret_ident.get)+" = " else ""
      val si = newVar()
      val s = si.name
      val offset = if(n > 0) ".toInt+"+off.toString else if(n < 0) ".toInt-" + n.toString else ""
      fromScript(VarDecl(si, a)) + "\n" +
      indent + s"${ret_expr}" + "f\"${" + s"$s" + offset + "}%0" + n + "d\""
    case InputExpr(a) => 
      val dropped = fromScript(a, Options())
      val dropped_string = if(dropped == "0") "" else s".drop($dropped)"
      val tq = "\""
      s"args$dropped_string.headOption.getOrElse($tq$tq)"
    case SubString(e, pbr1, pbr2, mode) => 
      val ret_expr = if(opt.ret_ident != None) (if(opt.declare_ret_ident) "val " else "")+fromScript(opt.ret_ident.get)+" = " else ""
      val si = newVar()
      val s = si.name
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
  //  case LowerCase(e) => fromScript(e) + ".toLowerCase()"
  //  case UpperCase(e) => fromScript(e) + ".toUpperCase()"
  //  case InitialCase(e) =>  fromScript(e) + ".map{ var first = true; (e: Char) => if(first) {first = false; e.toUpper} else e }"
    case ToInt(e) => "("+fromScript(e)+").toInt"
    case PositionBetweenRegex(r1, r2, i) => 
       val i1 = newVar().name
       val p1 = opt.ret_ident.get.name
       val tq = "\""
       val s = opt.input.name
       indent + s"val $i1 = " + fromScript(i) + "\n" +
       indent + s"""val $p1 = $tq$tq$tq$r1$tq$tq$tq.r.findAllMatchIn($s).map(_.end(0)).toList.intersect($tq$tq$tq$r2$tq$tq$tq.r.findAllMatchIn($s).map(_.start(0)).toList) match { case l if (l.length >= $i1 && $i1 >= 1) => l($i1-1) case l if (l.length + $i1 >= 0 && $i1 <= -1) => l($i1 + l.length) case _ => -1 } \n"""
    case _ => throw new Exception(s"Impossible to parse expression $t")
      //i1 r1.r.findAllMatchIn(s).map(_.start(0)).toList
      //
  }
}

/** Transforms an imperative program into a valid bash program */
object Bashification {
  import ImperativeProgram._
  def apply(t: Script): String = {
    fromScript(t)
  }
  case class Options(ret_ident: Option[Identifier] = None, declare_ret_ident: Boolean = false, input: Identifier = null) //TODO
  final val stopBashAndExplainIfNecessary = """
if [[ $# -eq 0 ]];
  then
    echo "Please provide a string (possibly in quotes) and an optional integer as last argument for the index."
    exit
fi
# Checks if the last element (index) is a number, else set the index to 0 and keep the whole array.
args=( "$@" )
index=${args[ ${#args[@]} - 1 ] - 1}
re='^[0-9]+$'
if test `echo $index | grep -E -c $re` -ne 0; then
  args=(${args[@]:0:$$(${#args[@]}-1)})
else
  index=0 
fi
"""
  def fromScript(t: Tree, opt: Options = Options())(implicit indent: String = ""): String = t match {
    case t@Script(stats, expr) => 
      "#!/bin/sh\n" + (if(t.comment != "") ("# "+t.comment + "\n") else "") + "\n" +
      stopBashAndExplainIfNecessary +
      "function script " + fromScript(stats match { case Block(_) => stats case e => Block(e::Nil) }, Options(ret_ident = Some(expr), declare_ret_ident=true)) + "\n\nscript $0 $1"
    case Block(s) => var ss = indent + "{\n"
      ss +=  ((for(t <- s) yield {
        var res = t
        val tt = fromScript(t, opt)(indent = "  " + indent)
        tt
      }) mkString "\n")
	    ss += "\n"
	    ss += (opt.ret_ident match {
	      case Some(r) => "  " + indent + "echo " + fromScript(r) + ";\n" + indent + " }"
	      case None => indent + " }"
	    })
	    ss
	  case InputsLength => "${#args[@]}"
    case While(cond, expr) => indent + "while [["+fromScript(cond)("")+"]]\n"+indent+"do\n" + fromScript(expr) + "\n"+indent+"done"
    case If(cond, thn, els) => indent + "if [["+fromScript(cond)("")+"]]; then\n" + fromScript(thn, opt)(indent + "  ") + (els match { case Some(a) => "\n"+indent+"else\n" + fromScript(a, opt)(indent + "  ") case None => ""}) + "\n" + indent + "fi";
    case Assign(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(Identifier(i), e) => indent + i + "=(" + fromScript(e) + ")"
    case VarDecl(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(Identifier(i), StringLit(s)) => indent + "local " + i + "=\"" + s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"") + "\"";
    case VarDecl(Identifier(i), e) => indent + "local " + i + "=$((" + fromScript(e) + ")) # "+e;
    case Identifier(i) => "$"+i
    case StringLit(s) => "\""+s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")+"\""
    case IntLit(i) => opt.ret_ident match {
      case Some(Identifier(p1)) =>
        if((opt.input ne null) && i < 0) {
          indent + s"local $p1=" + "$(${"+s"#${opt.input.name}[@]}" + (if(i != -1) " + " + (i+1).toString else "")+")"
        } else {
          indent + s"local $p1=" + i.toString
        }
      case None => i.toString
    }
    case BoolLit(i) => i.toString
    case Not(a) => "! " + fromScript(a)
    case NotEq(a, b) => "(" + fromScript(a) + " != " + fromScript(b) + ")"
    case Eq(a, b) => "(" + fromScript(a) + " == " + fromScript(b) + ")"
    case Plus(a, b) => "(" + fromScript(a) + " + " + fromScript(b) + ")"
    case Times(a, b) => "(" + fromScript(a) + " * " + fromScript(b) + ")"
    case Or(a, b) => "(" + fromScript(a) + " || " + fromScript(b) + ")"
    case And(a, b) => "(" + fromScript(a) + " && " + fromScript(b) + ")"
    case Concat(a, b) => 
      val s1 = newVar()
      val s2 = newVar()
      indent + "local " + s1.name + "=" + fromScript(a)("") + "\n" + 
      indent + "local " + s2.name + "=" + fromScript(b)("") + "\n" +
      indent + (opt.ret_ident match {
        case Some(ident) => ident.name + "=\"${"+s1.name+"}${"+s2.name+"}\"";
        case None => "echo \"${"+s1.name+"}${"+s2.name+"}\"";
      })
    case FormatNumber(a, n, off) =>
      val ret_expr = if(opt.ret_ident != None) (if(opt.declare_ret_ident) "local " else "")+opt.ret_ident.get.name+"=" else "echo "
      val si = newVar()
      val s = si.name
      val offset = if(n > 0) "+"+off.toString else if(n < 0) "-" + n.toString else ""
      fromScript(VarDecl(si, ToInt(a))) + "\n" +
      indent + s"${ret_expr}" + "(printf \"%0" + n + "d\" ${"+s"$s" + offset+"})"
    case InputExpr(a) => 
      val i = fromScript(a, Options())
      val tq = "\""
      "args[$(( "+i+" ))]"
    case SubString(e, pbr1, pbr2, mode) => 
      val ret_expr = if(opt.ret_ident != None) (if(opt.declare_ret_ident) "local " else "")+opt.ret_ident.get.name+"=" else "echo "
      val si = newVar()
      val s = si.name
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
        case NORMAL => "{:0}"
        case CONVERT_LOWERCASE =>  "echo {:0} | tr '[:upper:]' '[:lower:]'"
        case CONVERT_UPPERCASE =>  "echo {:0} | tr '[:lower:]' '[:upper:]'"
        case UPPERCASE_INITIAL =>  "echo \"$(echo \"{:0}\" | sed 's/.*/\\u&/')\""
      }
      indent + s"local $s=" + fromScript(e) + "\n" +
      fromScript(pbr1, opt.copy(ret_ident = Some(Identifier(p1)), input=si)) + " #Substring position left ("+pbr1.toString()+")\n" +
      fromScript(pbr2, opt.copy(ret_ident = Some(Identifier(p2)), input=si)) + " #substring position right ("+pbr2.toString()+")\n" +
      indent + s"""$si=$${if [[ $$$p1 -ge 0 && $$$p2 -ge 0 && $$$p1 -le $${#${s}[@]} && $$$p2 -le $${#${s}[@]} ]]; then echo $$s|tail -c +$$(($$$p1 + 1))|head -c $$(($$$p2 - $$$p1)); else echo $tq$tq; fi}""" +
      indent + ret_expr + mm.replace("{:0}", "$"+s)
    case ToInt(e) => fromScript(e)
    case PositionBetweenRegex(r1, r2, i) => //TODO: The most important part in bash.
       val i1 = newVar().name
       val p1 = opt.ret_ident.get.name
       val tq = "\""
       val s = opt.input.name
       val matchings = newVar().name
       indent + s"local $i1=" + fromScript(i) + " #"+i+"\n" +
       indent + s"""local $matchings=$$(echo $$$s | awk  '{s1=$$0; i1=1; idx1=0; s2=s1; i2=1; idx2=0; newidx1=0; newidx2=0;
       while(i1>0 && i2>0){
           if(idx1 <= idx2) {
             i1=match(s1, /${r1.replace("/", "\\/").replace("'", "\\'")}/); 
             if( i1 > 0) {
               newidx1 = idx1 + i1;
               s1=substr(s1, i1+1);
             }
           }
           if(idx2 <= idx1) {
             i2=match(s2, /${r2.replace("/", "\\/").replace("'", "\\'")}/); 
             if( i2 > 0) {
               newidx2 = idx2 + i2;
               s2=substr(s2, i2+1);
             }
           }
           if(i1>0 && i2>0) {
             if(newidx1 == newidx2) {
               print newidx1;
             }
             idx1 = newidx1;
             idx2 = newidx2;
           }
       }
}' ORS=' ')""" +
       indent + s"""
       local $p1=$$(if [[ $$(#$matchings[@]) -ge $$$i1 && $$$i1 -ge 1 ]]; then
          echo $${$matchings[ $$$i1 - 1 ]}
       else
          if [[ $$(#$matchings[@]) + $$$i1 -ge 0 && $$$i1 -le -1 ]]; then
            echo $${$matchings[ $${#$matchings[@]} + $$$i1 ]}
          else 
            echo -1
          fi
       fi
       );"""
//      """ local $p1 = $$$mappings match { case l if (l.length >= $i1 && $i1 >= 1) => l($i1-1) case l if (l.length + $i1 >= 0 && $i1 <= -1) => l($i1 + l.length) case _ => -1 } \n"""
    case _ => throw new Exception(s"Impossible to parse expression $t")
      //i1 r1.r.findAllMatchIn(s).map(_.start(0)).toList
      //
  }
  
}

object Powershellification {
  import ImperativeProgram._
  import ImperativeProgram._
  def apply(t: Script): String = {
    fromScript(t)
  }
  
  var DEBUG = false
  def debug(s: =>String) = if(DEBUG) s else ""
    
  case class Options(ret_ident: Option[Identifier] = None, declare_ret_ident: Boolean = false, input: Identifier = null)
  
  def prefixParams(b: Block) = b match {
    case Block(l) =>
      Block(CustomStat("param([string[]] $strings = @(\"\"), [int] $index = 0)") :: l)
  }
  def makeBlock(t: Stat): Block = t match {
    case b: Block => b
    case n => Block(n::Nil)
  }
  def prefixReturnExpr(s: String, id: Option[Identifier]): String = {
    (if(id != None) (fromScript(id.get)("")+" = ") else "") + s
  }
  def fromScript(t: Tree, opt: Options = Options())(implicit indent: String = ""): String = t match {
    case t@Script(stats, expr) => 
      "#####################\n" + (if(t.comment != "") ("# "+t.comment + "\n") else "") + "#####################\n" +
      """
if($args[0] -is [system.array]) {
  $args = $args[0]
}
$index = 0
if($args[-1] -match "^[0-9]+$") {
  $index = [convert]::ToInt32($args[-1]) - 1
  $args = $args[0..($args.length-1)]
}
      
filter Get-Starts($Pattern) {
$_ | Select-String -AllMatches $pattern | Select-Object -ExpandProperty Matches | Select-Object -ExpandProperty Index 
}
filter Get-Ends($Pattern) {
$_ | Select-String -AllMatches $pattern | Select-Object -ExpandProperty Matches | Select-Object @{Name="End";Expression={$_.Index + $_.Length}} | Select-Object -ExpandProperty End
}
""" +
      fromScript(prefixParams(makeBlock(stats)), Options(ret_ident = Some(expr), declare_ret_ident=true))(indent) + " -strings $args -index $index\n"
    case Block(s) => var ss = indent + (if(opt.ret_ident.nonEmpty) "& " else "")+"{\n"
      ss +=  ((for(t <- s) yield {
        var res = t
        val tt = fromScript(t, opt)(indent = "  " + indent)
        tt
      }) mkString "\n")
	    ss += "\n"
	    ss += (opt.ret_ident match {
	      case Some(r) => "  " + indent + fromScript(r)("") + ";\n" + indent + " }"
	      case None => indent + " }"
	    })
	    ss
    case While(cond, expr) => indent + "while ("+fromScript(cond)("")+")"+debug("<#"+expr+"#>")+"\n"+indent + fromScript(makeBlock(expr))
    case If(cond, thn, els) => indent + "if ("+fromScript(cond)("")+") " + fromScript(makeBlock(thn), opt) + (els match { case Some(a) => "\n"+indent+"else\n" + fromScript(makeBlock(a), opt) case None => ""});
    
    case Assign(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case Assign(i@Identifier(_), e) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e:FormatNumber) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i, e:SubString) => fromScript(e, opt.copy(ret_ident = Some(i)))
    case VarDecl(i@Identifier(_), StringLit(s)) => indent + fromScript(i)("") + debug("<# "+t+" #>") + " = \"" + s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"") + "\"";
    case VarDecl(i@Identifier(_), e) => indent + fromScript(i)("") + debug("<# "+t+" #>") + " = " + fromScript(e)("") + "  # "+e
    case IndexOf(Identifier(array), Identifier(value)) => indent + prefixReturnExpr(s"[array]::indexof($$$array,$$$value)", opt.ret_ident)
    case ArrayGet(Identifier(array), Identifier(index)) => indent + prefixReturnExpr(s"$$$array[$$$index] "+ debug("<# "+t+" #>") , opt.ret_ident)
    case ArrayAdd(i:Identifier, e:Identifier) =>
      indent + s"$$${i.name} += , $$${e.name}"
    case ArrayAdd(i:Identifier, e) => var res = newVar()
      fromScript(e, opt.copy(ret_ident = Some(res))) + debug("<# "+t+" #>")+"\n" +
      fromScript(ArrayAdd(i, res))
    case ArrayAdd(expr, e: Identifier) => 
      indent + fromScript(expr)("") + s" += , $$${e.name}"
    case ArrayAdd(expr, e) => var res = newVar()
      fromScript(expr, opt.copy(ret_ident = Some(res)))("")+ debug("<# "+t+" #>") + "\n"
      fromScript(ArrayAdd(res, e))
    case EmptyArrayLit() => indent + prefixReturnExpr("@()", opt.ret_ident)
    case Identifier(i) => prefixReturnExpr("$"+i, opt.ret_ident)
    case StringLit(s) => indent + prefixReturnExpr("\""+s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")+"\"", opt.ret_ident)
    case IntLit(i) => opt.ret_ident match {
      case Some(Identifier(p1)) =>
        if((opt.input ne null) && i < 0) {
          indent + s"$$$p1 = " + s"$$${opt.input.name}.length" + (if(i != -1) " + " + (i+1).toString else "")
        } else {
          indent + s"$$$p1 = " + i.toString
        }
      case None => indent + i.toString
    }
    case BoolLit(i) => indent + prefixReturnExpr(if(i) "$true" else "$false", opt.ret_ident)
    case Not(a) => indent + prefixReturnExpr("! (" + fromScript(a)+")", opt.ret_ident)
    case NotEq(a, b) => "(" + fromScript(a) + " -cne " + fromScript(b) + ")"
    case Eq(a, b) => "(" + fromScript(a) + " -ceq " + fromScript(b) + ")"
    case Plus(a, b) => indent + prefixReturnExpr("(" + fromScript(a) + " + " + fromScript(b) + ")", opt.ret_ident)
    case Times(a, b) => indent + prefixReturnExpr("(" + fromScript(a) + " * " + fromScript(b) + ")", opt.ret_ident)
    case Or(a, b) => "(" + fromScript(a) + " -or " + fromScript(b) + ")"
    case And(a, b) => "(" + fromScript(a) + " -and " + fromScript(b) + ")"
    case Concat(Identifier(s1), Identifier(s2)) => 
      indent + (opt.ret_ident match {
        case Some(ident) => fromScript(ident)("") + " = \"$"+s1+"$"+s2+"\"";
        case None => "\"$"+s1+"$"+s2+"\"";
      })
    case Concat(i1@Identifier(s1), b) => 
      val s2 = newVar()
      fromScript(b, opt.copy(ret_ident=Some(s2))) + s" # Concat $b with return $s2\n" +
      fromScript(Concat(i1, s2), opt)
    case Concat(a, b) => 
      val s1 = newVar()
      fromScript(a, opt.copy(ret_ident=Some(s1))) + s" # Concat $a with return $s1\n" + 
      fromScript(Concat(s1, b), opt)
    case FormatNumber(a, n, off) =>
      val si = newVar()
      val s = si.name
      val offset = if(n > 0) "+"+off.toString else if(n < 0) "-" + n.toString else ""
      fromScript(VarDecl(si, a)) + "\n" +
      indent + prefixReturnExpr("(\"{0:D", opt.ret_ident) + n + "}\" -f ( "+ fromScript(ToInt(si))("") + offset +"))"
    case InputExpr(a) => 
      val i = fromScript(a, Options())("")
      indent + prefixReturnExpr("$strings["+i+"]", opt.ret_ident)
    case InputsLength => "$strings.length"
    case SubString(e, pbr1, pbr2, mode) => 
      val si = newVar()
      val s = si.name
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
        case NORMAL => "{:0}"
        case CONVERT_LOWERCASE =>  "{:0}.ToLower()"
        case CONVERT_UPPERCASE =>  "{:0}.ToUpper()"
        case UPPERCASE_INITIAL =>  "{:0} | % { Try { $_.substring(0,1).ToUpper()+$_.substring(1) } catch { $error = $true; \"\" } }"
      }
      fromScript(e,    opt.copy(ret_ident = Some(si))) + s" # Defining input string $e\n" +
      fromScript(pbr1, opt.copy(ret_ident = Some(Identifier(p1)), input=si)) + " #Substring position left ("+pbr1.toString()+")\n" +
      fromScript(pbr2, opt.copy(ret_ident = Some(Identifier(p2)), input=si)) + " #substring position right ("+pbr2.toString()+")\n" +
      indent + s"""$$$s = & { if ($$$s -and $$$p1 -ge 0 -and $$$p2 -ge 0 -and $$$p1 -le $$${s}.length -and $$$p2 -le $$${s}.length) { $$$s.substring($$$p1, $$$p2 - $$$p1) } else {$$error = $$true; $tq$tq} }\n""" +
      indent + prefixReturnExpr(mm.replace("{:0}", s"$$$s"), opt.ret_ident)
    case ToInt(e) => "[convert]::ToInt32(" + fromScript(e)("") + ")"
    case CustomStat(s) => indent + s
    case PositionBetweenRegex(r1, r2, i) =>
       val i1 = newVar().name
       val p1 = opt.ret_ident.get.name
       val tq = "\""
       val s = opt.input.name
       val matchingsStart = newVar()
       val matchingsEnd = newVar()
       val matchings = newVar()
       indent + s"$$$i1=" + fromScript(i) + " #"+i+"\n" +
       indent + s"$$${matchingsStart.name} = $$$s | Get-Starts $tq$r2$tq\n" +
       indent + s"$$${matchingsEnd.name} = $$$s | Get-Ends $tq$r1$tq\n" +
       indent + s"$$${matchings.name} = $$${matchingsStart.name} | ?{$$${matchingsEnd.name} -contains $$_}\n" +
       indent + prefixReturnExpr(s"& { if ($$${matchings.name}.length -ge $$$i1 -and $$$i1 -ge 1) { $$${matchings.name}[$$$i1-1] } else { if ($$${matchings.name}.length + $$$i1 -ge 0 -and $$$i1 -le -1) { $$${matchings.name}[$$$i1 + $$${matchings.name}.length] } else { -1 } } }", opt.ret_ident)
//      """ local $p1 = $$$mappings match { case l if (l.length >= $i1 && $i1 >= 1) => l($i1-1) case l if (l.length + $i1 >= 0 && $i1 <= -1) => l($i1 + l.length) case _ => -1 } \n"""
    case _ => throw new Exception(s"Impossible to parse expression $t")
      //i1 r1.r.findAllMatchIn(s).map(_.start(0)).toList
      //
  }
}