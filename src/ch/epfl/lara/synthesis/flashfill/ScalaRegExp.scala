package ch.epfl.lara.synthesis.flashfill

import Programs._

import scala.util.matching.Regex

object ScalaRegExp extends ComputePositionsInString {
  /**
   * Computes the list of positions where there exists a word recognized by this regexp.
   */
  implicit class AugmentedRegExp(dfa: Regex) {
    def recordFinalStates(s: String): List[Int] = {
      dfa.findAllMatchIn(s).map(_.end(0)).toList.map(_ - 1)
    }
  }
  
  def computePositionsEndingWith(r: RegExp, s: String): List[Int] = {
    val dfa = convertRegExp(r)
    dfa.recordFinalStates(s)
  }
  
  def convertRegExp(r: RegExp): Regex = {
    r match {
      case TokenSeq(l) =>
        (l map convertToken mkString "").r
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
      case s: SpecialChar =>
        s.c.toString
      case RepeatedToken(c) =>
        val cl = convertList(c.f)
        cl match {
          case " " =>
            "[ ]+"
          case "." =>
            "[.]+"
          case _ =>
            s"""(?<=[^$cl]|^)[$cl]+(?=[^$cl]|$$)"""
        }
      case RepeatedNotToken(c) =>
        val cl = convertList(c.f)
        cl match {
          case " " =>
            "[^ ]+"
          case "." =>
            "[^.]+"
          case _ =>
            s"""(?<=[^$cl]|^)[^$cl]+(?=[^$cl]|$$)"""
        }
      case EndTok => "\\Z"
      case StartTok => "\\A"
    }
  }
}