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
    case CPos(0) => 1
    case CPos(-1) => 1
    case CPos(k) => Math.abs(k)*2+8
    case Pos(r1, r2, c) => 
      val const_weight = c match {
        case IntLiteral(i) if i >= 1 => i - 1
        case IntLiteral(i) if i <= -1 => -i + 1
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
        case Some(ConstStr(s)) if isCommonSeparator(s) => 2
        case Some(_) => 100 // Non-standard separators are penalized.
      }
      10 + weight(l) - 1 + separatorweight
    case Number(s@ SubStr(InputString(_), p1, p2, m), l, (o, step)) =>
      0 + weight(s) + (if(step < 0) ((-step).toString.length * 7 + 5) else if(step == 0) 0 else step.toString.length * 7)
    case Number(s@ SubStr(PrevStringNumber(_), p1, p2, m), l, (o, step)) =>
      10 + weight(s) - 1 + 10*(Math.abs(step) - 1) + (o - 1)
    case Number(s, l, (o, step)) =>
      10 + weight(s) - 1 + (step-1) // if s is smaller, the better.
    case ConstStr(s) => 5 + s.size*10
    case SubStr(vi, Pos(r1, r2, i), Pos(p1, p2, j), method) if i == j && r1 == Epsilon && p2 == Epsilon && r2 == p1 =>
      10 + weight(r2) + method.id
    case SubStr(vi, CPos(0), CPos(-1), method) => 13
    case SubStr(vi, p, pos, method) => 10 + weight(p)(true) + weight(pos)(false) + method.id
    case TokenSeq(t) => t.length // Best for empty sequences.
    case IntLiteral(i) => Math.abs(i)
    case Linear(i,w,j) => i
    case _ => 1
  }
}