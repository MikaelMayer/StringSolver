package ch.epfl.lara.synthesis.stringsolver
import Programs._
import ProgramsSet._

object Weights {
  /**
   * Returns the weight of a program.
   * 
   * Heuristic rules
   * - A number is preferred to a substring extracted
   * - A substring is preferred to constant strings
   * - A number is better if it a mapping from another number present in an input rather than in the output.
   *     in the case of extracting from input, the step can be anything.
   * - A number is preferred if its length is smaller.
   * - A concatenation is better if the sum of its children weights is better.
   */
  def weight(p: Program)(implicit starting_pos: Boolean = true): Int = p match {
    case CPos(0) => 10
    case CPos(-1) => 10
    case CPos(k) => Math.abs(k)*20+80
    case Pos(r1, r2, c) => 
      val const_weight = c match {
        case IntLiteral(i) if i >= 1 => i*10 - 10
        case IntLiteral(i) if i <= -1 => -i*10 + 10
        case _ => weight(c)
      }
      if(starting_pos) {
        3*weight(r1)+2*weight(r2)+const_weight
      } else {
        2*weight(r1)+3*weight(r2)+const_weight
      }
    case Concatenate(l) => l.map(weight).sum
    
    case Loop(w, l, separator) => 
      val separatorweight = separator match {
        case None => 0
        case Some(ConstStr(s)) if isCommonSeparator(s) => 20
        case Some(_) => 1000 // Non-standard separators are penalized.
      }
      100 + weight(l) - 10 + separatorweight
    case NumberMap(s@ SubStr(InputString(vi), p1, p2, m), l, offset) =>
      weight(vi) +
      weight(s) + (if(offset < 0) ((-offset).toString.length * 70 + 50) else if(offset == 0) 0 else offset.toString.length * 70)
    /*case Number(s@ SubStr(PrevStringNumber(_), p1, p2, m), l, (o, step)) =>
      100 + weight(s) - 10 + 100*(Math.abs(step) - 1) + (o - 1)*/
    /*case NumberMap(s, l, (o, step)) =>
      100 + weight(s) - 10 + (step-1)*10*/ // if s is smaller, the better.
    case Counter(length, start, step) =>
      50 + length * 100 + Math.abs(step)*10
    case ConstStr(s) => 50 + s.size*100
    case s@SubStr(InputString(vi), Pos(r1, r2, i), Pos(p1, p2, j), method) if i == j && r1 == Epsilon && p2 == Epsilon && r2 == p1 =>
      100 + weight(vi) + weight(r2) + method.id*10
    case s@SubStr(InputString(vi), CPos(0), CPos(-1), method) => 130 + weight(vi)
    case s@SubStr(InputString(vi), p, pos, method) => 100 + weight(vi) + weight(p)(true) + weight(pos)(false) + method.id*10
    case TokenSeq(t) => t.map(token => weight(token)(true)).sum
    case IntLiteral(i) => Math.abs(i)*3
    case Linear(i,w,j) => (i-1)*10+Math.max(j-1, 0)
    case SpecialConversion(s, p) => weight(s)/10
    case NumTok => 8
    case AlphaNumTok => 8
    case AlphaTok => 9
    case LowerTok => 10
    case UpperTok => 10
    case _ => 10
  }
  def weightWithOffset(offset: Int)(p: Program)(implicit starting_pos: Boolean = true): Int = weight(p) + offset
}