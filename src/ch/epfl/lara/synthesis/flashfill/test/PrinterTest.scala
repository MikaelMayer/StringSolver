package ch.epfl.lara.synthesis.flashfill.test

import org.scalatest._
import org.scalatest.matchers._

class PrinterTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  /*def contain[A](f: Function[A] => Boolean) = Matcher { (left: Iterator[Function[A]]) =>
    MatchResult(
        left.find(f) != None,
        left + " did not contain the required function",
        left + " contained the required function"   
    )
  }
  def containFNMapping[A](input: List[List[Int]], output: List[A]) = Matcher { (left: Iterator[Function[A]]) =>
    MatchResult(
        left.find({ f => 
          println(s"testing ${f.toReadableString}")
          input.map{elem => f(elem)} == output }) != None,
        left + s" did not contain the mapping from ${input} to ${output}",
        left + s" contained the required function"   
    )
  }*/

  "Printer" should "output correct program signification" in {
    
    val p = Loop(Identifier("w"), Concatenate(List(SubStr2(InputString(1), UpperTok, Linear(1, Identifier("w"), 0)))))
    Printer(p) should equal ("concatenates every occurence of uppercase letters in input 1")
  }
}

class EvaluatorTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  /*def contain[A](f: Function[A] => Boolean) = Matcher { (left: Iterator[Function[A]]) =>
    MatchResult(
        left.find(f) != None,
        left + " did not contain the required function",
        left + " contained the required function"   
    )
  }
  def containFNMapping[A](input: List[List[Int]], output: List[A]) = Matcher { (left: Iterator[Function[A]]) =>
    MatchResult(
        left.find({ f => 
          println(s"testing ${f.toReadableString}")
          input.map{elem => f(elem)} == output }) != None,
        left + s" did not contain the mapping from ${input} to ${output}",
        left + s" contained the required function"   
    )
  }*/
  import ch.epfl.lara.synthesis.flashfill.Evaluator._
  
  "Evaluator" should "correctly concatenate strings" in {
    concatenate(List(StringValue("a"), StringValue("b"), StringValue("c"))) should equal (StringValue("abc"))
    concatenate(List(StringValue("a"), BottomValue, StringValue("c"))) should equal (BottomValue)
  }
  
  "Evaluator" should "correctly replace TraceExprs" in {
    val e = replaceTraceExpr(Concatenate(List(
        SubStr(InputString(1), Pos(TokenSeq(List(RepeatedToken(NumTok))), TokenSeq(List(RepeatedToken(AlphaTok))), Linear(3, Identifier("w"), 1)), CPos(-1)),
        SubStr(InputString(1), Pos(TokenSeq(List(RepeatedToken(NumTok))), TokenSeq(List(RepeatedToken(AlphaTok))), IntLiteral(2)), CPos(-1)))))(Identifier("w"), 3)
    
    e should equal (Concatenate(List(
        SubStr(InputString(1), Pos(TokenSeq(List(RepeatedToken(NumTok))), TokenSeq(List(RepeatedToken(AlphaTok))), IntLiteral(10)), CPos(-1)),
        SubStr(InputString(1), Pos(TokenSeq(List(RepeatedToken(NumTok))), TokenSeq(List(RepeatedToken(AlphaTok))), IntLiteral(2)), CPos(-1))))
    )
  }

  "Evaluator" should "correctly execute programs" in {
    val p = Concatenate(List(ConstStr(" + "), SubStr(InputString(0), Pos(TokenSeq(List(RepeatedToken(NumTok))), TokenSeq(List(RepeatedToken(AlphaTok))), IntLiteral(2)), CPos(-1))))

    evalProg(p)(Array("qsdf1234amlkj12345mlkj432  fkj ")) should equal (StringValue(" + mlkj432  fkj "))
  }
}

class AutomataTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  import ch.epfl.lara.synthesis.flashfill.Automata.{not => nt, _}
  import ch.epfl.lara.synthesis.flashfill.Automata
  /*def contain[A](f: Function[A] => Boolean) = Matcher { (left: Iterator[Function[A]]) =>
    MatchResult(
        left.find(f) != None,
        left + " did not contain the required function",
        left + " contained the required function"   
    )
  }
  def containFNMapping[A](input: List[List[Int]], output: List[A]) = Matcher { (left: Iterator[Function[A]]) =>
    MatchResult(
        left.find({ f => 
          println(s"testing ${f.toReadableString}")
          input.map{elem => f(elem)} == output }) != None,
        left + s" did not contain the mapping from ${input} to ${output}",
        left + s" contained the required function"   
    )
  }*/
  
  
  "Automata" should "correctly compute negations" in {
    val digit = '0' -> '9'
    val up = 'A' -> 'Z'
    val low = 'a' -> 'z'
    
    nt(List(up)) should equal (List((0: Char, ('A'-1).toChar), (('Z' + 1).toChar, Char.MaxValue)))
    
    nt(List()) should equal (List((0: Char, Char.MaxValue)))
    nt(List((0: Char, Char.MaxValue))) should equal (List())
    
    val cl = List(digit, up, low)
    nt(nt(cl)) should equal (cl)
  }
  
  "Automata" should "correctly compute intersections" in {
    val digit = '0' -> '9'
    val up = 'A' -> 'Z'
    val low = 'a' -> 'z'
    
    inter(List(digit, up, low), List(up)) should equal(List(up))
    inter(List(digit, up, low), List(low)) should equal(List(low))
    inter(List(digit, up, low), List(digit)) should equal(List(digit))
    
    inter(List(digit, up, low), List(up, low)) should equal(List(up, low))
    inter(List(digit, up, low), List(digit, low)) should equal(List(digit, low))
    inter(List(digit, up, low), List(digit, up)) should equal(List(digit, up))
    
    // Compute union
    nt(inter(nt(List(digit)), nt(List(up)))) should equal(List(digit, up))
    
  }
  
  "Automata" should "correctly compute sub-labels" in {
    val digit = List('0' -> '9')
    val up = List('A' -> 'Z')
    val low = List('a' -> 'z')
    val b = List('B' -> 'B')
    
    val res = createLabelSubsets(List(digit, up,  allChars && !digit && !up), List(b, !b))
    //println(res)
    res(digit) should equal (List(CharLabel(digit)))
    res(up) should equal (List(CharLabel(List(('B','B'))), CharLabel(List(('A','A'), ('C','Z')))))
    res(b) should equal (List(CharLabel(b)))
    
    val res2 = createLabelSubsets(List(digit, allChars && !digit), List(allChars))
    
    res2 should contain key (allChars)
    //res(allChars) should equal (List(CharLabel(digit), up))
  }
  
  "Automata" should "correctly compute createDisjointSets" in {
    val digit = List('0' -> '9')
    val up = List('A' -> 'Z')
    val low = List('a' -> 'z')
    val b = List('B' -> 'B')
    
    val res = createDisjointSets(List(CharLabel(digit), CharLabel(nt(digit)), CharLabel(up), CharLabel(nt(up))))
    res(digit) should equal (List(CharLabel(digit)))
    res(up) should equal  (List(CharLabel(up)))
    res(nt(digit)).toSet should equal (Set(CharLabel(inter(nt(digit), nt(up))), CharLabel(up)))
  }
  
  
  "Automata" should "correctly convert simple tokens" in {
    val d = convertToken(RepeatedToken(UpperTok))
    d.recordFinalStates("UZEabOPQ") should equal (List(0,1,2))

    convertToken(StartTok).recordFinalStates("UZEabOPQ") should equal (List(0))
    convertToken(RepeatedToken(LowerTok)).recordFinalStates("abcdEFG") should equal (List(0, 1, 2, 3))
  }

  "Automata" should "correctly convert regexps" in {
    val dfa = convertRegExp(TokenSeq(List(RepeatedToken(UpperTok))))
    dfa.recordFinalStates("UZEabOPQ") should equal (List(0,1,2, 5, 6, 7))
    
    val dfa2 = convertRegExp(TokenSeq(List(StartTok, RepeatedToken(UpperTok))))
    dfa2.recordFinalStates("UZEabOPQ") should equal (List(0,1,2))
    
    val dfa3 = convertRegExp(TokenSeq(List(RepeatedToken(LowerTok), ForwardSlash)))
    dfa3.recordFinalStates("125abc/1/aa1/ab/") should equal (List(6,15))
  }
  
}