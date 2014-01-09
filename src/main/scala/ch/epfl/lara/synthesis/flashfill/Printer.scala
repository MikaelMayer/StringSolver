/**
 *    ____        _      __  ____          __  __ 
 *   / __/_______(_)__  / /_/ __/_ _____  / /_/ / 
 *  _\ \/ __/ __/ / _ \/ __/\ \/ // / _ \/ __/ _ \
 * /___/\__/_/ /_/ .__/\__/___/\_, /_//_/\__/_//_/
 *              /_/           /___/               
 *              
 *  File:   Printer.scala
 *  Author: Mikaël Mayer
 *  Date:   27.11.2013
 *  Purpose:Extracts a program to transform it to a readable string.
 */


package ch.epfl.lara.synthesis.flashfill

import scala.collection.mutable.ArrayBuffer

object Printer {
  import Programs._
  
  implicit class AugmentedString(sc: StringContext) {
    def t(args: Any*): String = {
      val args2 = args map {
        case p: Program => apply(p)
        case s => s.toString
      }
      sc.s(args2: _*)
    }
  }
  
  implicit class Rule(r: String) {
    def ==>(f: Seq[String] => String)(implicit ruleRegister: ArrayBuffer[String => String]): String => String = {
      val res = {(s: String) =>
        val c = r.r.unapplySeq(s)
        if(c == None) { s } else { f(c.get) }
      }
      ruleRegister += res
      res
    }
  }
  
  def numeral(i: Int): String = i match {
    case -3 => "antepenultimate"
    case -2 => "penultimate"
    case -1 => "last"
    case 0 => "zero-th"
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case i if i % 10 == 1 => i.toString+"st"
    case i if i % 10 == 2 => i.toString+"nd"
    case i if i % 10 == 3 => i.toString+"rd"
    case i => i.toString+"th"
  }
  def numeral(i: IntegerExpr): String = i match {
    case IntLiteral(j) => numeral(j)
    case Linear(i, w, j) => 
      (i, j) match {
      case (1, 0) => t"$w-th"
      case (1, j) => t"$w+$j-th"
      case (i, 0) => t"$i*$w-th"
      case (i, j) => t"$i*$w+$j-th"
    }
  }
  
  def makeEnumeration(l: List[String], sep: String = ", ", lastSep: String = " and "): String = {
    val init = l.view.init
    if (init.nonEmpty) {
      init.mkString(sep) + lastSep + l.last
    } else l.headOption.getOrElse("")
    }
  
  def apply(p: Program): String = { val res = p match {
      case Loop(w, c, separator) =>
        val separated = separator.map((tt: ConstStr) => s" separated by '${tt.s}'").getOrElse("")
        c match {
          case Concatenate(t) if t.length == 1 =>
            t"concatenates for all $w>=0 $c$separated"
          case _ => t"($c)"
            t"concatenates$separated for all $w>=0 ($c)"
        }
      case Concatenate(fs) =>
        if(fs.size == 1)
          apply(fs.head)
        else {
          makeEnumeration(fs.map(apply), " + ", " + ")
        }
      case Linear(i, w, j) =>
        (i, j) match {
          case (1, 0) => t"$w"
          case (1, j) => t"$w+$j"
          case (i, 0) => t"$i*$w"
          case (i, j) => t"$i*$w+$j"
        }
      case SubStr(v1, p1, p2, mm) =>
        val m = mm match {
          case NORMAL => ""
          case CONVERT_UPPERCASE => " uppercase"
          case CONVERT_LOWERCASE => " lowercase"
          case _ => mm.toString
        }
        val sv1 = v1 match {
          case InputString(IntLiteral(i)) => s"${numeral(i+1)} input"
          case InputString(Linear(i, w, j)) => s"input ${apply(Linear(i, w, j+1))}"
          case PrevStringNumber(IntLiteral(i)) => s"numbers of previous ${numeral(i+1)} output"
          case PrevStringNumber(o) => s"numbers of previous output $o"
        }
        (p1, p2) match {
          case (Pos(TokenSeq(List(StartTok)), Epsilon, _) | CPos(0), Pos(Epsilon, TokenSeq(List(EndTok)), _) | CPos(-1)) =>
            t"the $sv1$m"
          case (Pos(_, TokenSeq(List(NumTok)), IntLiteral(c1)), Pos(TokenSeq(List(NumTok)), _, IntLiteral(c2))) if c1 == c2 && v1.isInstanceOf[PrevStringNumber]=>
            t"the ${numeral(c1)} number in previous output"
          case (Pos(Epsilon, r1, c1), Pos(r2, Epsilon, c2)) if r1 == r2 && c1 == c2 =>
            val cs = numeral(c1)
            t"the $cs$m occurence of $r1 in $sv1"
          case (CPos(0), CPos(d)) if d >= 1 =>
            val chars = if(d == 1) "char" else s"$d chars"
            t"the first$m $chars in $sv1"
          case (CPos(i), CPos(-1)) if i <= -2 =>
            val chars = if(i == -2) "char" else s"$i chars"
            t"the last$m $chars in $sv1"
          case (c, d) =>
            val ending = t"$d"
            val starting = t"$c"
            if(ending == "the end") {
              t"the$m $sv1 starting at [$starting]"
            } else if(starting == "the beginning") {
              t"the$m $sv1 until [$ending]"
            } else {
              t"the$m substring starting at [$starting] ending at [$ending] in $sv1"
            }
        }
      case Pos(r1, Epsilon, i) =>
        t"the end of the ${numeral(i)} $r1"
      case Pos(Epsilon, r2, i) =>
        t"the ${numeral(i)} $r2"
      case Pos(r1, r2, i) =>
        t"the ${numeral(i)} $r2 after $r1"
      case NonUpperTok =>
        "token not containing A-Z"
      case UpperTok =>
        "uppercase word"
      case LowerTok =>
        "lowercase word"
      case NonLowerTok =>
        "token not containing a-z"
      case AlphaTok =>
        "word"
      case NonAlphaTok =>
        "token not containing a-zA-Z"
      case AlphaNumTok =>
        "AlphaNumeric token"
      case NonAlphaNumTok =>
        "token not containing 0-9a-zA-Z"
      case StartTok =>
        "the beginning"
      case EndTok =>
        "the end"
      case NumTok =>
        "number"
      case NonNumTok =>
        "non-number"
      case ConstStr(s) => s"the constant string '$s'"
      case SpecialChar(a) => s"'$a'"
      case t: CharClass if t.f.head._1 == t.f.head._2 => "'"+t.f.head._1+"'"
      case RepeatedToken(l) =>
        t"${l}s"
      case RepeatedNotToken(l) =>
        t"not ${l}s"
      case TokenSeq(l) =>
        l match {
          case Nil => "nothing"
          case a::Nil => t"$a"
          case _ => val ls = (l map apply)
            def rec(l: Seq[String], res: String): String = l match { case a::b::Nil => res + a + " and " + b case a::l => rec(l, res + a + ", ")}
            "(" + rec(ls, "") + ")"
        }
      case CPos(0) => "the beginning"
      case CPos(-1) => "the end"
      case CPos(i) => if(i >= 0) s"the ${numeral(i+1)} char" else s"the ${numeral(-(i+1))} char from the end"
      case IntLiteral(k) => k.toString
      case Identifier(v) =>
        v
      case Number(s@SubStr(InputString(_), r1, r2, m), size, (offset, step)) =>
        val incrementedby = if(step == 0) "" else if(step > 0) s" incremented by $step" else s" decremented by ${-step}"
        val default = if(offset == 0) "" else s" (default: $offset)"
        t"a $size-digit number from $s$incrementedby$default"
      case Number(s, size, (offset, step)) =>
        val by = if(step == 1) "" else s" by $step"
        val from = if(false && offset == 1) "" else s" starting at $offset"
        t"a $size-digit number incrementing$by$from continuing $s"
      case _ =>
        s"UNKNOWN : $p"
    }
    // Post-processing
    
    implicit val rules = ArrayBuffer[String => String]()
    "(.*)for all (\\w+)>=0 the (\\w+)\\+1-th occurence(.*)" ==>
    { case Seq(prefix, w1, w2, suffix) if w1 == w2 =>
        prefix + "every occurence" + suffix
    }
    "(.*)for all (\\w+)>=0 the input (\\w+)\\+1(.*)" ==>
    { case Seq(prefix, w1, w2, suffix) if w1 == w2 =>
        prefix + "all inputs" + suffix
    }
    
    (res /: rules) { case (r, ruletoApply) => ruletoApply(r) }
  }
}