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
  
  def apply(p: Program): String = { val res = p match {
      case Loop(w, c) => t"concatenates for all $w $c"
      case Concatenate(fs) =>
        if(fs.size == 1)
          apply(fs.head)
        else {
          fs.map(apply) mkString " and "
        }
      case SubStr(v1, Pos(Epsilon, r1, c1), Pos(r2, Epsilon, c2)) if r1 == r2 && c1 == c2 =>
        t"the ${c1}th occurence of $r1 in $v1"
      case UpperTok =>
        "uppercase letters"
      case IntLiteral(k) => k.toString
      case Linear(k1, w, k2) =>
        val prefix = k1 match { case 1 => "" case -1 => "-" case k => k.toString + "*" }
        val suffix = k2 match { case 0 => "" case k if k<0=> k.toString case k => "+" + k.toString}
        prefix + w.value + suffix
      case InputString(v) =>
        s"input $v"
      case Identifier(v) =>
        v
      case _ =>
        s"UNKNOWN : $p"
    }
    // Post-processing
    
    implicit val rules = ArrayBuffer[String => String]()
    "(.*)for all (\\w+) the (\\w+)th occurence(.*)" ==>
    { case Seq(prefix, w1, w2, suffix) if w1 == w2 =>
        prefix + "every occurence" + suffix
    }
    
    (res /: rules) { case (r, ruletoApply) => ruletoApply(r) }
  }
}