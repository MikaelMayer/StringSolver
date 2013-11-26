package ch.epfl.lara.synthesis.flashfill

/**
 * Attribution to Sumit Gulwani for this particular piece of code.
 */

object Programs {
  trait RegExp extends Program {
    def reverse: RegExp
  }
  /**
   * A regular expression r = TokenSeq(T1; ...; Tn) is a sequence
    of tokens T1; ...; Tn. We often refer to singleton token sequences
    TokenSeq(T1) simply as T1. We use the notation e to denote an
    empty sequence of tokens. e matches an empty string.
   */
  //case object Epsilon extends RegExp
  case class TokenSeq(t: Seq[Token]) extends RegExp {
    def reverse = TokenSeq(t.reverse)
  }
  val Epsilon = TokenSeq(Seq())
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
  trait Token extends Program
  abstract class CharClass(val f: List[(Char, Char)]) extends RegExp { def reverse = this }
  case class RepeatedToken(c: CharClass) extends Token
  case class RepeatedNotToken(c: CharClass) extends Token
  
  case object UpperTok extends CharClass(List(('A', 'Z')))
  case object NumTok extends CharClass(List(('0', '9')))
  case object LowerTok extends CharClass(List(('a', 'z')))
  case object AlphaTok extends CharClass(List(('A', 'Z'), ('a', 'z')))
  case object AlphaNumTok extends CharClass(List(('0', '9'), ('A', 'Z'), ('a', 'z')))
  // TODO : Add missing tokens.
  
  object SpecialChar { def unapply(s: SpecialChar): Option[Char] = Option(s.c)}
  abstract class SpecialChar(val c: Char) extends Token
  case object StartTok extends Token // Start of the string
  case object EndTok extends Token // End of the string
  case object Hyphen extends SpecialChar('-')
  case object Dot extends SpecialChar('.')
  case object SemiColon extends SpecialChar(';')
  case object Colon extends SpecialChar(':')
  case object Comma extends SpecialChar(',')
  case object Backslash extends SpecialChar('\\')
  case object ForwardSlash extends SpecialChar('/')
  case object LeftParenthesis extends SpecialChar('(')
  case object RightParenthesis extends SpecialChar(')')
  case object LeftBracket  extends SpecialChar('[')
  case object RightBracket extends SpecialChar(']')
  case object LeftBrace  extends SpecialChar('{')
  case object RightBrace extends SpecialChar('}')
  case object Percentage extends SpecialChar('%')
  case object Hat        extends SpecialChar('^')
  case object Underscore extends SpecialChar('_')
  case object EqSign     extends SpecialChar('=')
  case object Plus       extends SpecialChar('+')
  case object Star       extends SpecialChar('*')
  case object And        extends SpecialChar('&')
  case object At         extends SpecialChar('@')

  // TODO Missing #!$"'<>?~`
  
  
  sealed trait StringVariable extends Program { def index: Int }
  case class InputString(index: Int) extends StringVariable
  
  sealed trait Program {}
  case class Switch(s: Seq[(Bool, TraceExpr)]) extends Program
  case class Bool(ds: Seq[Conjunct]) extends Program         // Disjunction of these
  case class Conjunct(pis: Seq[Predicate]) extends Program   // Conjunction of these
  sealed trait Predicate extends Program
  case class Match(v: StringVariable, r: RegExp, k: Int) extends Predicate
  case class NotMatch(v: StringVariable, r: RegExp, k: Int) extends Predicate
  
  /**
   * A trace expression refers to the Concatenate(f1; ; fn) constructor,
      which denotes the string obtained by concatenating the
      strings represented by f1; f2; ; fn in that order.
   */
  sealed trait TraceExpr extends Program
  case class Concatenate(fs: Seq[AtomicExpr]) extends TraceExpr
  
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
   * (or negative). If not enough matches exist, then ? is returned.
   */
  case class Pos(r1: RegExp, r2: RegExp, c: IntegerExpr) extends Position
  
  trait IntegerExpr extends Program
  case class IntLiteral(k: Int) extends IntegerExpr
  case class Linear(k1: Int, w: Identifier, k2: Int) extends IntegerExpr // k1*w + k2
  
  case class Identifier(value: String) extends Program
}


