package ch.epfl.lara.synthesis.flashfill

import Programs._
import scala.util.matching.Regex
import java.util.regex.Pattern

object ScalaRegExp extends ComputePositionsInString {
  type Start = Int
  type End = Int
  final val plus = "{1," + (Integer.MAX_VALUE / 100) + "}"
  
  /**
   * Computes the list of positions where there exists a word recognized by this regexp.
   */
  implicit class AugmentedRegExp(dfa: Regex) {
    def recordFinalStates(s: String): List[Int] = {
      val res = dfa.findAllMatchIn(s).map(_.end(0)).toList.map(_ - 1)
      res
    }
    def recordStartingStates(s: String): List[Int] = {
      val res = dfa.findAllMatchIn(s).map(_.start(0)).toList
      res
    }
  }
  
  def computePositionsStartingWith(r: RegExp, s: String): List[Start] = {
    val dfa = convertRegExp(r, starting = true)
    dfa.recordStartingStates(s)
  }
  
  def computePositionsEndingWith(r: RegExp, s: String): List[End] = {
    val dfa = convertRegExp(r, starting = false)
    dfa.recordStartingStates(s) map (_ - 1)
  }
  def computePositionsOfToken(r: Token, s: String): (List[Start],List[End]) = {
    val starts = ("(?="+convertToken(r) + ")").r
    var endings = ("(?<="+convertToken(r) + ")").r
    (starts.findAllMatchIn(s).map(m => (m.start(0))).toList,
     endings.findAllMatchIn(s).map(m => (m.start(0))).toList
    )
  }
  def computePositionsOfTokenSimple(r: Token, s: String): List[(Start, End)] = {
    val dfa = convertToken(r).r
    (dfa.recordStartingStates(s) zip dfa.recordFinalStates(s))
  }
  def computePositionsOfRegExp(r: RegExp, s: String): (List[Start],List[End]) = {
    (computePositionsStartingWith(r, s), computePositionsEndingWith(r, s))
  }
  
  def convertRegExp(r: RegExp, starting: Boolean): Regex = {
    r match {
      case TokenSeq(l) =>
        if(starting) {
          ("(?=" + (l map convertToken mkString "") + ")").r
        } else {
          ("(?<=" + (l map convertToken mkString "") + ")").r
        }
    }
  }
  
  def convertList(l: List[(Char, Char)]) = (l map { case tuple => if(tuple._1 == tuple._2) tuple._1 else (tuple._1 + "-" + tuple._2) } mkString "")
  
  def convertToken(t: Token): String = {
    t match {
      case DotTok => """\."""
      case Backslash => """\\"""
      case RightParenTok => """\)"""
      case LeftParenTok => """\("""
      case RightBracketTok => """\]"""
      case LeftBracketTok => """\["""
      case HatTok => """\^"""
      case DollarTok => """\$"""
      case PlusTok => """\+"""
      case StarTok => """\*"""
      case QuestionTok => """\?"""
      case LeftBraceTok => """\{"""
      case RightBraceTok => """\}"""
      case s: SpecialChar =>
        s.c.toString
      case RepeatedToken(c) =>
        val cl = convertList(c.f)
        cl match {
          case _ =>
            s"""(?<=[^$cl]|^)[$cl]$plus(?=[^$cl]|$$)"""
        }
      case RepeatedNotToken(c) =>
        val cl = convertList(c.f)
        cl match {
          case "." =>
            s"[^\\.]$plus"
          case _ =>
            s"""(?<=[$cl]|^)[^$cl]$plus(?=[$cl]|$$)"""
        }
      case EndTok => "\\Z"
      case StartTok => "\\A"
    }
  }
}