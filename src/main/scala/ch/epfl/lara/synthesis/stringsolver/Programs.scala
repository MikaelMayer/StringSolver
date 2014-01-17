/**
 *    ____        _      __  ____          __  __ 
 *   / __/_______(_)__  / /_/ __/_ _____  / /_/ / 
 *  _\ \/ __/ __/ / _ \/ __/\ \/ // / _ \/ __/ _ \
 * /___/\__/_/ /_/ .__/\__/___/\_, /_//_/\__/_//_/
 *              /_/           /___/               
 *              
 *  File:   Programs.scala
 *  Author: Mikaël Mayer
 *  Date:   27.11.2013
 *  Purpose:Declaration of the programs to manipulate strings.
 */


package ch.epfl.lara.synthesis.stringsolver

/**
 * Attribution to Sumit Gulwani for this particular piece of code.
 */

object Programs {
  import Evaluator._
  import ProgramsSet._
  import StringSolver._
  
  sealed trait Program {

    /**
     * Returns true if the identifier is used in this program.
     */
    def uses(w: Identifier): Boolean = this match {
      case i@Identifier(_) => i == w
      case p: Product => p.productIterator.toList exists {
        case arg: Program => arg uses w
        case arg: List[_] => arg exists { case p: Program => p uses w}
        case arg: Set[_] => arg exists { case p: Program => p uses w}
        case _ => false
      }
      case _ => false
    }
    def get(args: IndexedSeq[String]): Value = {
      evalProg(this)(args)
    }
    def apply(args: String*): String = {
      get(args.toIndexedSeq).asString
    }
    def apply(args: Input_state): String = {
      evalProg(this)(args).asString
    }
  }

  
  object Switch { def apply(s: (Bool, TraceExpr)*): Switch = apply(s.toList) }
  case class Switch(s: List[(Bool, TraceExpr)]) extends Program
  case class Bool(ds: Seq[Conjunct]) extends Program         // Disjunction of these
  case class Conjunct(pis: Seq[Predicate]) extends Program   // Conjunction of these
  sealed trait Predicate extends Program
  case class Match(v: InputString, r: RegExp, k: Int) extends Predicate
  object Match { def apply(v: InputString, r: RegExp): Match = Match(v, r, 1) }
  case class NotMatch(v: InputString, r: RegExp, k: Int) extends Predicate
  object NotMatch { def apply(v: InputString, r: RegExp): NotMatch = NotMatch(v, r, 1) }
  
  
  sealed trait RegExp extends Program {
    def reverse: RegExp
  }
  /**
   * A regular expression r = TokenSeq(T1; ...; Tn) is a sequence
    of tokens T1; ...; Tn. We often refer to singleton token sequences
    TokenSeq(T1) simply as T1. We use the notation e to denote an
    empty sequence of tokens. e matches an empty string.
   */
  //case object Epsilon extends RegExp
  object TokenSeq {
    def apply(s: Token*): TokenSeq = apply(s.toList)
  }
  case class TokenSeq(t: List[Token]) extends RegExp {
    def reverse = TokenSeq(t.reverse)
  }
  val Epsilon = TokenSeq()
  /**
   * A token is either some special
     token or is constructed from some character class C in two ways:
     C+ denotes a token that matches a sequence of one or more characters
     from C. :C+ denotes a token that matches a sequence of one
     or more characters that do not belong to C. We use the following
     collection of character classes C: Numeric Digits (0-9), Alphabets
     (a-zA-Z), Lowercase alphabets (a-z), Uppercase alphabets (A-Z),
     Accented alphabets, Alphanumeric characters, Whitespace characters,
     All characters.
   */
  sealed trait Token extends Program
  class CharClass(val f: List[(Char, Char)]) extends Program { def reverse = this
    def unapply(c: Char): Option[Unit] = f find { case tuple => tuple._1 <= c && c <= tuple._2 } map {_ => ()}
    override def toString = {
      val res = this.getClass().getName().replace("ch.epfl.lara.synthesis.stringsolver.Programs$","").replace("$", "")
      if(res == "CharClass") {
        "CharClass("+f.toString+")"
      } else res
    }
    def unary_! = RepeatedNotToken(this)
  }
  case class RepeatedToken(c: CharClass) extends Token {
    override def toString = c.toString.replaceAll("class","Tok")
  }
  case class RepeatedNotToken(c: CharClass) extends Token {
    override def toString = "Non" + c.toString.replaceAll("class","Tok")
  }
  
  
  case object UpperClass extends CharClass(List(('A', 'Z')))
  val UpperTok = RepeatedToken(UpperClass)
  val NonUpperTok = RepeatedNotToken(UpperClass)
  case object NumClass extends CharClass(List(('0', '9')))
  val NumTok = RepeatedToken(NumClass)
  val NonNumTok = RepeatedNotToken(NumClass)
  case object LowerClass extends CharClass(List(('a', 'z')))
  val LowerTok = RepeatedToken(LowerClass)
  val NonLowerTok = RepeatedNotToken(LowerClass)
  case object AlphaClass extends CharClass(List(('A', 'Z'), ('a', 'z')))
  val AlphaTok = RepeatedToken(AlphaClass)
  val AlphTok = AlphaTok
  val NonAlphaTok = RepeatedNotToken(AlphaClass)
  case object AlphaNumClass extends CharClass(List(('0', '9'), ('A', 'Z'), ('a', 'z')))
  val AlphaNumTok = RepeatedToken(AlphaNumClass)
  val NonAlphaNumTok = RepeatedNotToken(AlphaNumClass)
  
  val NonSpaceTok = RepeatedNotToken(new CharClass(List((' ', ' '))))
  val SpaceTok = RepeatedToken(new CharClass(List((' ', ' '))))
  case object StartTok extends Token // Start of the string
  case object EndTok extends Token // End of the string
  
  val NonDotTok = RepeatedNotToken(new CharClass(List(('.','.'))))
  
  var listNonEmptyTokens: List[Token] = List(UpperTok,
      NonUpperTok,
      NumTok,
      NonNumTok,
      LowerTok,
      NonLowerTok,
      AlphaTok,
      NonAlphaTok,
      AlphaNumTok,
      NonAlphaNumTok,
      NonSpaceTok,
      SpaceTok,
      NonDotTok
      )
  
  object SpecialChar { def unapply(s: SpecialChar): Option[Char] = Option(s.c)
    def apply(s: Char): SpecialChar = {
      val res =  new SpecialChar(s)
      listNonEmptyTokens = res :: listNonEmptyTokens
      res
    }
  }
  class SpecialChar(val c: Char) extends Token { override def toString = s"SpecialChar('$c')"}
  /*object NonSpecialChar { def unapply(s: NonSpecialChar): Option[Char] = Option(s.c)
    def apply(s: Char): NonSpecialChar = {
      val res =  new NonSpecialChar(s)
      listTokens = res :: listTokens
      res
    }
  }
  class NonSpecialChar(val c: Char) extends Token { override def toString = s"NonSpecialChar('$c')"}
  */
  val HyphenTok = SpecialChar('-')
  val DotTok = SpecialChar('.')
  val SemiColonTok = SpecialChar(';')
  val ColonTok = SpecialChar(':')
  val CommaTok = SpecialChar(',')
  val Backslash = SpecialChar('\\')
  val SlashTok = SpecialChar('/')
  val LeftParenTok = SpecialChar('(')
  val RightParenTok = SpecialChar(')')
  val LeftBracketTok  = SpecialChar('[')
  val RightBracketTok = SpecialChar(']')
  val LeftBraceTok  = SpecialChar('{')
  val RightBraceTok = SpecialChar('}')
  val PercentageTok = SpecialChar('%')
  val HatTok        = SpecialChar('^')
  val UnderscoreTok = SpecialChar('_')
  val EqSignTok     = SpecialChar('=')
  val PlusTok       = SpecialChar('+')
  val StarTok       = SpecialChar('*')
  val AndTok        = SpecialChar('&')
  val AtTok         = SpecialChar('@')
  val DollarTok     = SpecialChar('$')
  val QuestionTok   = SpecialChar('?')
  val QuoteTok      = SpecialChar('"')
  val PoundTok      = SpecialChar('#')
  val ExclamationTok= SpecialChar('!')
  val SingleQuoteTok= SpecialChar(''')
  val LessTok       = SpecialChar('<')
  val RightTok      = SpecialChar('>')
  val TildeTok      = SpecialChar('~')
  val BackTickTok   = SpecialChar('`')
  
  val listTokens = StartTok::EndTok::listNonEmptyTokens
  
  //assert(listTokens.length < 64)
  // TODO : Missing UTF8 chars
  
  
  sealed trait StringVariable extends Program
  case class InputString(i: IntegerExpr) extends StringVariable
  case class PrevStringNumber(i: IntegerExpr) extends StringVariable
  object InputString { 
    def apply(index: Int): InputString = InputString(IntLiteral(index))
  }
  
  trait Alternative[A <: Program] {
    private var alternatives: Iterable[A] = SEmpty
    def withAlternative(a: Iterable[A]): this.type = {alternatives = a; this}
    def getAlternatives = alternatives
  }
  /**
   * A trace expression refers to the Concatenate(f1; ; fn) constructor,
      which denotes the string obtained by concatenating the
      strings represented by f1; f2; ; fn in that order.
   */
  sealed trait TraceExpr extends Program with Alternative[TraceExpr]
  object Concatenate {
    def apply(s : AtomicExpr*): Concatenate = Concatenate(s.toList)
  }
  case class Concatenate(fs: List[AtomicExpr]) extends TraceExpr
  
  /**
   *  An atomic expression
      refers to ConstStr (denoting a constant string), SubStr or
      Loop constructors, which are explained below
   */
  sealed trait AtomicExpr extends Program with Alternative[AtomicExpr]
  
  object SubStrFlag {
    var registered = List[SubStrFlag](NORMAL, CONVERT_LOWERCASE, CONVERT_UPPERCASE, UPPERCASE_INITIAL)
    def apply(i: Int) = {
      registered(i)
    }
  }
  sealed trait SubStrFlag extends Program { def id: Int ; def apply(s: String): String }
  case object NORMAL extends SubStrFlag { def id = 0 ; def apply(s: String) = s}
  case object CONVERT_LOWERCASE extends SubStrFlag { def id = 1 ; def apply(s: String) = s.toLowerCase() }
  case object CONVERT_UPPERCASE extends SubStrFlag { def id = 2 ; def apply(s: String) = s.toUpperCase() }
  case object UPPERCASE_INITIAL extends SubStrFlag { def id = 3 ; def apply(s: String) = if(s.length == 0) "" else s(0).toUpper.toString + s.substring(1, s.length)}
  
   /**
      The SubStr(vi; p1; p2) constructor makes use of two position expressions
      p1 and p2, each of which evaluates to an index within the
      string vi. SubStr(vi; p1; p2) denotes the substring of string vi that
      starts at index specified by p1 and ends at index specified by p2-1.
      If either of p1 or p2 refer to an index that is outside the range of
      string vi, then the SubStr constructor returns BOTTOM.
      
      We use notation SubStr2(vi; r; c) to denote the cth occurrence
      of regular expression r in vi, i.e., SubStr(vi; Pos(; r; c); Pos(r; ; c)).
      We often denote SubStr(vi; CPos(0); CPos(-1)) by simply vi.
   */
  case class SubStr(vi: StringVariable, p1: Position, p2: Position, flag: SubStrFlag = NORMAL) extends AtomicExpr
  def SubStr2(vi: StringVariable, r: RegExp, c: IntegerExpr): SubStr = {
    SubStr(vi, Pos(Epsilon, r, c), Pos(r, Epsilon, c), NORMAL)
  }
  case class ConstStr(s: String) extends AtomicExpr
  case class Loop(w: Identifier, content: TraceExpr, separator: Option[ConstStr] = None) extends AtomicExpr
  // Offset is an integer which helps to compute the initial number if a evaluates to nothing
  // Number("0001", 3, Linear(6, "w", 5)) == "007"
  // Number("0001", 3, IntLiteral(6)) = "006"
  
  case class Number(a: AtomicExpr, length: IntegerExpr, offsetstep: (Int, Int)) extends AtomicExpr
  
  sealed trait Position extends Program
  /**
   * The position expression CPos(k) refers to the kth index in
     a given string from the left side (or right side), if the integer
     constant k is non-negative (or negative).
   */
  case class CPos(k: Int) extends Position
  /**
   * Pos(r1; r2; c) is another
   * position constructor, where r1 and r2 are some regular expressions
   * and integer expression c evaluates to a non-zero integer. The Pos
   * constructor evaluates to an index t in a given string s such that r1
   * matches some suffix of s[0 : t-1] and r2 matches some prefix of
   * s[t : `-1], where ` = Length(s). Furthermore, t is the cth such
   * match starting from the left side (or the right side) if c is positive
   * (or negative). If not enough matches exist, then Bottom is returned.
   */
  case class Pos(r1: RegExp, r2: RegExp, c: IntegerExpr) extends Position
  
  sealed trait IntegerExpr extends Program
  case class IntLiteral(k: Int) extends IntegerExpr
  case class Linear(k1: Int, w: Identifier, k2: Int) extends IntegerExpr // k1*w + k2
  
  case class Identifier(value: String) extends Program { self =>
    def *(k1: Int) = new { def +(k2: Int) = Linear(k1, self, k2) }
    def +(k2: Int) = Linear(1, self, k2)
  }
  import scala.language._

  implicit def CharClassToRegExp(c: CharClass): RegExp = TokenSeq(RepeatedToken(c))
  implicit def CharClassToToken(c: CharClass): Token = RepeatedToken(c)
  implicit def SpecialCharToRegExp(s: SpecialChar): RegExp = TokenSeq(s)
  implicit def TokenToRegExp(s: Token): RegExp = TokenSeq(s)
  implicit def IntToIntegerExpr(i: Int): IntLiteral = IntLiteral(i)
  implicit def IntToStringVariable(i: Int): StringVariable = InputString(i)
  implicit def IdentifiertoIntegerExpr(i: Identifier): IntegerExpr = Linear(1, i, 0)
  implicit def MatchToBool(i: Predicate): Bool = Bool(Seq(Conjunct(Seq(i))))
}


