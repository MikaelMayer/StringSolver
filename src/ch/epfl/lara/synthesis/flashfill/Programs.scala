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


package ch.epfl.lara.synthesis.flashfill

/**
 * Attribution to Sumit Gulwani for this particular piece of code.
 */

object Programs {
  sealed trait Program {}

  
  object Switch { def apply(s: (Bool, TraceExpr)*): Switch = apply(s.toList) }
  case class Switch(s: List[(Bool, TraceExpr)]) extends Program
  case class Bool(ds: Seq[Conjunct]) extends Program         // Disjunction of these
  case class Conjunct(pis: Seq[Predicate]) extends Program   // Conjunction of these
  sealed trait Predicate extends Program
  case class Match(v: StringVariable, r: RegExp, k: Int) extends Predicate
  object Match { def apply(v: StringVariable, r: RegExp): Match = Match(v, r, 1) }
  case class NotMatch(v: StringVariable, r: RegExp, k: Int) extends Predicate
  object NotMatch { def apply(v: StringVariable, r: RegExp): NotMatch = NotMatch(v, r, 1) }
  
  
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
  }
  case class RepeatedToken(c: CharClass) extends Token
  case class RepeatedNotToken(c: CharClass) extends Token
  
  
  case object UpperTok extends CharClass(List(('A', 'Z')))
  case object NumTok extends CharClass(List(('0', '9')))
  case object LowerTok extends CharClass(List(('a', 'z')))
  case object AlphaTok extends CharClass(List(('A', 'Z'), ('a', 'z')))
  val AlphTok = AlphaTok
  case object AlphaNumTok extends CharClass(List(('0', '9'), ('A', 'Z'), ('a', 'z')))
  val NonDotTok = RepeatedNotToken(new CharClass(List(('.', '.'))))
  val NonSpaceTok = RepeatedNotToken(new CharClass(List((' ', ' '))))
  val SpaceTok = RepeatedToken(new CharClass(List((' ', ' '))))
  
  object SpecialChar { def unapply(s: SpecialChar): Option[Char] = Option(s.c)}
  abstract class SpecialChar(val c: Char) extends Token
  case object StartTok extends Token // Start of the string
  case object EndTok extends Token // End of the string
  case object HyphenTok extends SpecialChar('-')
  case object DotTok extends SpecialChar('.')
  case object SemiColonTok extends SpecialChar(';')
  case object ColoTokn extends SpecialChar(':')
  case object CommaTok extends SpecialChar(',')
  case object Backslash extends SpecialChar('\\')
  case object SlashTok extends SpecialChar('/')
  case object LeftParenTok extends SpecialChar('(')
  case object RightParenTok extends SpecialChar(')')
  case object LeftBracketTok  extends SpecialChar('[')
  case object RightBracketTok extends SpecialChar(']')
  case object LeftBraceTok  extends SpecialChar('{')
  case object RightBraceTok extends SpecialChar('}')
  case object PercentageTok extends SpecialChar('%')
  case object HatTok        extends SpecialChar('^')
  case object UnderscoreTok extends SpecialChar('_')
  case object EqSignTok     extends SpecialChar('=')
  case object PlusTok       extends SpecialChar('+')
  case object StarTok       extends SpecialChar('*')
  case object AndTok        extends SpecialChar('&')
  case object AtTok         extends SpecialChar('@')
  case object DollarTok     extends SpecialChar('$')
  case object QuestionTok   extends SpecialChar('?')
  // TODO Missing #!"'<>~`
  
  
  sealed trait StringVariable extends Program { def index: Int }
  case class InputString(index: Int) extends StringVariable
  
  /**
   * A trace expression refers to the Concatenate(f1; ; fn) constructor,
      which denotes the string obtained by concatenating the
      strings represented by f1; f2; ; fn in that order.
   */
  sealed trait TraceExpr extends Program
  object Concatenate {
    def apply(s : AtomicExpr*): Concatenate = Concatenate(s.toList)
  }
  case class Concatenate(fs: List[AtomicExpr]) extends TraceExpr
  
  /**
   *  An atomic expression
      refers to ConstStr (denoting a constant string), SubStr or
      Loop constructors, which are explained below
   */
  sealed trait AtomicExpr extends Program
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
  case class SubStr(vi: StringVariable, p1: Position, p2: Position) extends AtomicExpr
  def SubStr2(vi: StringVariable, r: RegExp, c: IntegerExpr): SubStr = {
    SubStr(vi, Pos(Epsilon, r, c), Pos(r, Epsilon, c))
  }
  
  
  case class ConstStr(s: String) extends AtomicExpr
  case class Loop(w: Identifier, c: TraceExpr) extends AtomicExpr
  
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
  }
  
  implicit def CharClassToRegExp(c: CharClass): RegExp = TokenSeq(RepeatedToken(c))
  implicit def CharClassToToken(c: CharClass): Token = RepeatedToken(c)
  implicit def SpecialCharToRegExp(s: SpecialChar): RegExp = TokenSeq(s)
  implicit def TokenToRegExp(s: Token): RegExp = TokenSeq(s)
  implicit def IntToIntegerExpr(i: Int): IntegerExpr = IntLiteral(i)
  implicit def IdentifiertoIntegerExpr(i: Identifier): IntegerExpr = Linear(1, i, 0)
  implicit def MatchToBool(i: Predicate): Bool = Bool(Seq(Conjunct(Seq(i))))
}


