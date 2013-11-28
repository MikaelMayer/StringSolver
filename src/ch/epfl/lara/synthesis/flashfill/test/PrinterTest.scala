package ch.epfl.lara.synthesis.flashfill.test

import org.scalatest._
import org.scalatest.matchers._

class PrinterTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer

  val v1 = InputString(0)
  val w = Identifier("w")
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
    
    val p = Loop(Identifier("w"), Concatenate(SubStr2(v1, UpperTok, w*1+0)))
    Printer(p) should equal ("concatenates every occurence of uppercase letters in first input")
  }
}

class EvaluatorTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  
  
  val v1 = InputString(0)
  val w = Identifier("w")

  import ch.epfl.lara.synthesis.flashfill.Evaluator._
  
  "Evaluator" should "correctly concatenate strings" in {
    concatenate(StringValue("a"), StringValue("b"), StringValue("c")) should equal (StringValue("abc"))
    concatenate(StringValue("a"), BottomValue, StringValue("c")) should equal (BottomValue)
  }
  
  "Evaluator" should "correctly replace TraceExprs" in {
    val e = replaceTraceExpr(Concatenate(List(
        SubStr(InputString(1), Pos(NumTok, AlphaTok, Linear(3, Identifier("w"), 1)), CPos(-1)),
        SubStr(InputString(1), Pos(NumTok, AlphaTok, IntLiteral(2)), CPos(-1)))))(Identifier("w"), 3)
    
    e should equal (Concatenate(List(
        SubStr(InputString(1), Pos(NumTok, AlphaTok, IntLiteral(10)), CPos(-1)),
        SubStr(InputString(1), Pos(NumTok, AlphaTok, IntLiteral(2)), CPos(-1))))
    )
  }
  
  implicit class RichEval(i: String) {
    def ==>(p: Program) = new { def ==>(o: String) = evalProg(p)(Array(i)) should equal (StringValue(o)) }
  }

  "Evaluator" should "correctly execute programs" in {
    val p =Concatenate(ConstStr(" + "), SubStr(v1, Pos(NumTok, AlphaTok, IntLiteral(2)), CPos(-1)))
    "qsdf1234amlkj12345mlkj432  fkj " ==> p ==> " + mlkj432  fkj "
    
    val p2 = Loop(Identifier("w"),Concatenate(SubStr2(v1, UpperTok, w*1+0)))
    "My Taylor is Rich" ==> p2 ==> "MTR"
    
    val p3 = SubStr(v1, Pos(Epsilon, NumTok, 1), CPos(-1))
    "CAMP DRY DBL NDL 3.6 OZ" ==> p3 ==> "3.6 OZ"
    
    val pp1 = Pos(LeftParenTok, TokenSeq(NumTok, SlashTok), w)
    val pp2 = Pos(TokenSeq(SlashTok, NumTok), RightParenTok,w)
    val p4 = Loop(w, Concatenate(SubStr(v1, pp1, pp2), ConstStr(" # ")))
    "(6/7)(4/5)(14/1)" ==> p4 ==> "6/7 # 4/5 # 14/1 # "

    val firstname = SubStr(v1, Pos(Epsilon, TokenSeq(AlphTok, NonDotTok), 1), Pos(Epsilon, TokenSeq(LowerTok, NonDotTok), 1))
    val e1 = Concatenate(SubStr(v1, Pos(Epsilon, TokenSeq(AlphTok, CommaTok), 1), Pos(AlphTok, CommaTok, 1)), ConstStr(", "), firstname, ConstStr("."))
    val e2 =  Concatenate(SubStr2(v1, AlphTok, -1), ConstStr(", "), firstname, ConstStr("."))
    val p5 = Switch((Match(v1, CommaTok, 1), e1), (NotMatch(v1, CommaTok, 1), e2))
      
    "Dr. Eran Yahav" ==> p5 ==> "Yahav, E."
    "Prof. Kathleen S. Fisher" ==> p5 ==> "Fisher, K."
    "Bill Gates, Sr." ==> p5 ==> "Gates, B."
    "George Ciprian Necula" ==> p5 ==> "Necula, G."
    "Ken McMillan, II" ==> p5 ==> "McMillan, K."
      
    val p61 = Pos(Epsilon, NonSpaceTok, w)
    val p62 = Pos(NonSpaceTok, TokenSeq(SpaceTok, NonSpaceTok), w)
    val p6 = Concatenate(Loop(w, Concatenate(SubStr(v1, p61, p62), ConstStr(" "))), SubStr2(v1, NonSpaceTok, -1))
      
    "Oege      de        Moor" ==> p6 ==> "Oege de Moor"
    "Kathleen         Fisher    AT&T Labs" ==> p6 ==> "Kathleen Fisher AT&T Labs"
  }
}

class AutomataTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  import ch.epfl.lara.synthesis.flashfill.AutomataChar.{not => nt, _}
  import ch.epfl.lara.synthesis.flashfill.AutomataChar
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
    
    val res = Automata.createDisjointSets(List(CharLabel(digit), CharLabel(nt(digit)), CharLabel(up), CharLabel(nt(up))))
    res(digit) should equal (List(CharLabel(digit)))
    res(up) should equal  (List(CharLabel(up)))
    res(nt(digit)).toSet should equal (Set(CharLabel(inter(nt(digit), nt(up))), CharLabel(up)))
  }
  
  
  "Automata" should "correctly convert simple tokens" in {
    val d = convertToken(RepeatedToken(UpperTok))
    d.recordFinalStates("UZEabOPQ") should equal (List(0,1,2))

    convertToken(StartTok).recordFinalStates("UZEabOPQ") should equal (List(-1))
    convertToken(RepeatedToken(LowerTok)).recordFinalStates("abcdEFG") should equal (List(0, 1, 2, 3))
  }

  "Automata" should "correctly convert regexps" in {
    val dfa = convertRegExp(UpperTok)
    dfa.recordFinalStates("UZEabOPQ") should equal (List(0,1,2, 5, 6, 7))
    
    val dfa2 = convertRegExp(TokenSeq(StartTok, UpperTok))
    dfa2.recordFinalStates("UZEabOPQ") should equal (List(0,1,2))
    
    val dfa3 = convertRegExp(TokenSeq(LowerTok, SlashTok))
    dfa3.recordFinalStates("125abc/1/aa1/ab/") should equal (List(6,15))
    
    val dfa4 = convertRegExp(Epsilon)
    dfa4.recordFinalStates("0/2/1") should equal (List(-1, 0, 1, 2, 3, 4))
  }
}



class ScalaRegExpTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  import ch.epfl.lara.synthesis.flashfill.ScalaRegExp._
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
  
  "ScalaRegExp" should "correctly convert regexps" in {
    val dfa = convertRegExp(UpperTok)
    dfa.recordFinalStates("UZEabOPQ") should equal (List(2, 7))
    
    val dfa2 = convertRegExp(TokenSeq(StartTok, UpperTok))
    dfa2.recordFinalStates("UZEabOPQ") should equal (List(2))
    
    val dfa3 = convertRegExp(TokenSeq(LowerTok, SlashTok))
    dfa3.recordFinalStates("125abc/1/aa1/ab/") should equal (List(6,15))
    
    val dfa4 = convertRegExp(Epsilon)
    dfa4.recordFinalStates("0/2/1") should equal (List(-1, 0, 1, 2, 3, 4))
    
    val dfa5 = convertRegExp(TokenSeq(AlphTok))
    dfa5.recordFinalStates("some words sentence") should equal (List(3, 9, 18))
    
    val dfa6 = convertRegExp(TokenSeq(LeftParenTok))
    dfa6.recordFinalStates("(6/7)(4/5)(14/1)") should equal (List(0, 5, 10))
  }
}

