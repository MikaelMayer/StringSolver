package ch.epfl.lara.synthesis.flashfill.test

import org.scalatest._
import org.scalatest.matchers._

class PrinterTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  import scala.language._
    
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
    
    val p = Loop(Identifier("w"), Concatenate(SubStr2(v1, UpperTok, w+1)))
    Printer(p) should equal ("concatenates every occurence of uppercase word in first input")
  }
}

class EvaluatorTest extends FlatSpec with ShouldMatchers  {
  import ch.epfl.lara.synthesis.flashfill.Programs._
  import ch.epfl.lara.synthesis.flashfill.Printer
  import scala.language._

  
  val v1 = InputString(0)
  val w = Identifier("w")

  import ch.epfl.lara.synthesis.flashfill.Evaluator._
  
  "Evaluator" should "correctly concatenate strings" in {
    concatenate(StringValue("a"), StringValue("b"), StringValue("c")) should equal (StringValue("abc"))
    concatenate(StringValue("a"), ⊥, StringValue("c")) should equal (⊥)
  }
  
  it should "correctly replace TraceExprs" in {
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

  it should "correctly execute programs" in {
    val p =Concatenate(ConstStr(" + "), SubStr(v1, Pos(NumTok, AlphaTok, IntLiteral(2)), CPos(-1)))
    "qsdf1234amlkj12345mlkj432  fkj " ==> p ==> " + mlkj432  fkj "
    
    val p2 = Loop(Identifier("w"),Concatenate(SubStr2(v1, UpperTok, w+1)))
    "My Taylor is Rich" ==> p2 ==> "MTR"
    
    val p3 = SubStr(v1, Pos(Epsilon, NumTok, 1), CPos(-1))
    "CAMP DRY DBL NDL 3.6 OZ" ==> p3 ==> "3.6 OZ"
    
    val pp1 = Pos(LeftParenTok, TokenSeq(NumTok, SlashTok), w+1)
    val pp2 = Pos(TokenSeq(SlashTok, NumTok), RightParenTok,w+1)
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
      
    val p61 = Pos(Epsilon, NonSpaceTok, w+1)
    val p62 = Pos(NonSpaceTok, TokenSeq(SpaceTok, NonSpaceTok), w+1)
    val p6 = Concatenate(Loop(w, Concatenate(SubStr(v1, p61, p62), ConstStr(" "))), SubStr2(v1, NonSpaceTok, -1))
      
    "Oege      de        Moor" ==> p6 ==> "Oege de Moor"
    "Kathleen         Fisher    AT&T Labs" ==> p6 ==> "Kathleen Fisher AT&T Labs"
      
    val pNumber = Concatenate(ConstStr("myFile"), Number(SubStr2(v1, NumTok, 1), 3, (5, 2)))
    "My number 1 should increase" ==> pNumber ==> "myFile003"
    "No number should start at five" ==> pNumber ==> "myFile005"
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
  
  it should "correctly compute intersections" in {
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
  
  it should "correctly compute sub-labels" in {
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
  
  it should "correctly compute createDisjointSets" in {
    val digit = List('0' -> '9')
    val up = List('A' -> 'Z')
    val low = List('a' -> 'z')
    val b = List('B' -> 'B')
    
    val res = Automata.createDisjointSets(List(CharLabel(digit), CharLabel(nt(digit)), CharLabel(up), CharLabel(nt(up))))
    res(digit) should equal (List(CharLabel(digit)))
    res(up) should equal  (List(CharLabel(up)))
    res(nt(digit)).toSet should equal (Set(CharLabel(inter(nt(digit), nt(up))), CharLabel(up)))
  }
  
  
  it should "correctly convert simple tokens" in {
    val d = convertToken(UpperTok)
    d.recordFinalStates("UZEabOPQ") should equal (List(0,1,2))

    convertToken(StartTok).recordFinalStates("UZEabOPQ") should equal (List(-1))
    convertToken(LowerTok).recordFinalStates("abcdEFG") should equal (List(0, 1, 2, 3))
  }

  it should "correctly convert regexps" in {
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

class ProgramSetTest extends FlatSpec with ShouldMatchers {
  import ch.epfl.lara.synthesis.flashfill._
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import FlashFill._
  import FlashFill._
  import Implicits._
  import Evaluator._
  val c = SDag[Int](Set(0, 1, 2, 3), 0, 3,
        Set((0, 1), (1, 2), (2, 3), (0, 2), (1, 3), (0, 3)),
        Map[(Int, Int), Set[SAtomicExpr]]()
        + ((0, 1) -> Set(SConstStr("a"):SAtomicExpr)) 
        + ((1, 2) -> Set(SConstStr("b"):SAtomicExpr))  
        + ((2, 3) -> Set(SConstStr("c"):SAtomicExpr,SSubStr(0, Set(SCPos(1)), Set(SCPos(2))):SAtomicExpr)) 
        + ((0, 2) -> Set(SConstStr("ab"):SAtomicExpr)) 
        + ((1, 3) -> Set(SConstStr("bc"):SAtomicExpr)) 
        + ((0, 3) -> Set(SConstStr("abc"):SAtomicExpr))
    )
    
  "DAG" should "compute neighbors correctly" in {
    
    c.neighbors(0, 0).map(_._3) should equal (Set(1, 2, 3))
  }
  
  it should "compute best paths" in {
    c.takeBest should equal (Concatenate(ConstStr("ab"), SubStr(0, CPos(1), CPos(2))))
  }
}
/*
class BenchMarkTest extends FlatSpec with ShouldMatchers {
  import ch.epfl.lara.synthesis.flashfill._
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import FlashFill._
  import FlashFill._
  import Implicits._
  import Evaluator._
  
  "Problem: Renaming files" should "be solved" in {
    val c = FlashFill()
    c.setUseNumbers(true)
    c.setUseDots(true)
    c.add(List("AB1234.gif"), "AB-0111-1.gif")
    val prog0 = c.solve()
    println(Printer(prog0.get))
    evalProg(prog0.get)(Array("AB1234.gif", "")).asString should equal ("AB-0111-1.gif")
    
    val solutions = c.add(List("B3245.gif"), "B-0111-2.gif")
    println(Printer(solutions.takeBest))
    //c.add(List("AB2541.gif"), "AB-0111-3.gif")
    //c.add(List("AB11422.jpg"), "AB-0111-4.jpg")
    val prog = c.solve()
    prog should not be 'empty
    println(Printer(prog.get))
    evalProg(prog.get)(Array("AB1234.gif", "")).asString should equal ("AB-0111-1.gif")
    evalProg(prog.get)(Array("B3245.gif", "   0111 1   ")).asString should equal ("B-0111-2.gif")
    
    evalProg(prog.get)(Array("B5678.gif", "   0111 4   ")) should equal (StringValue("B-0111-5.gif"))
  }
}*/

class FlashFillTest extends FlatSpec with ShouldMatchers {
  import ch.epfl.lara.synthesis.flashfill._
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import FlashFill._
  import FlashFill._
  import Implicits._
  import Evaluator._
  
  val f = new FlashFill()
  import f._
  initStats()
  
  "flashfill" should "correcly compute positions" in {
    val c = "AB12a45? #AB 18A. abc"
    for(i <- 0 until c.length; spos <- f.generatePosition(c, i); pos <- spos) {
      evalProg(pos)(Array(c)).asInt should equal(i)
    }
  }
  
  it should "correcly compute partitions of tokens" in {
    Reps("a     b") should equal(List(NonLowerTok, LowerTok, NonUpperTok, QuestionTok))
  }
  
  it should "correctly compute token seqs" in {
    val sa = computetokenSeq("a", listTokens)(0, 0) map (_._1)
    sa should contain(TokenSeq(AlphaTok))
    sa should not contain(TokenSeq(StartTok, StartTok, AlphaTok))
    sa should not contain(TokenSeq(AlphaTok, EndTok, EndTok))
    sa should contain(TokenSeq(StartTok, AlphaTok))
    sa should contain(TokenSeq(AlphaTok, EndTok))
    sa should contain(TokenSeq(NonNumTok, EndTok))
  }
  
  it should "correctly compute token seqs for longer strings" in {
    val sa = computetokenSeq("a     b", listTokens)(0, 6) map (_._1)
    sa should contain(TokenSeq(AlphaTok, SpaceTok, AlphaTok))
  }
  
  it should "correctly compute number positions." in {
     val res = ("0011" subnumberOf "1 2 10 15").toList
     res should contain ((0, 0, 10))
     res should contain ((2, 2, 9))
     res should contain ((4, 5, 1))
  }
  
  it should "correctly find numbers" in {
     "I god 0011,2 on my 31".numbers should equal("      0011 2       31")
  }
  
  it should "compute atomic subexpressions" in {
    val res = generateSubString(Array("5555", "0abb"), "abb")
    res.size should equal(1)
    res.headOption match {
      case Some(SSubStr(vi, spos, epos)) => 
        vi should equal(IntLiteral(1))
        spos should contain (SCPos(1):SPosition)
        spos should contain (SCPos(-4):SPosition)
        val notconst = spos.toTraversable.filterNot { case SCPos(_) => true case _ => false}
        notconst should not be 'empty
        notconst.headOption match {
          case Some(SPos(STokenSeq(List(SToken(beforeTokens))), STokenSeq(List(SToken(afterTokens))), nth)) =>
            beforeTokens should contain (NumTok: Token)
            beforeTokens should contain (NonLowerTok: Token)
            beforeTokens should contain (NonAlphaTok: Token)
            afterTokens should contain (NonNumTok: Token)
            afterTokens should contain (LowerTok: Token)
            afterTokens should contain (AlphaTok: Token)
            nth should contain (IntLiteral(1): IntegerExpr)
            nth should contain (IntLiteral(-1): IntegerExpr)
          case None =>
          case _ => throw new Exception(s"$notconst does not contain a SPos.")
        }
      case None => 
      case _ => throw new Exception(s"$res does not contain a SSubStr.") 
    }
  }
  
  it should "compute loops easily" in {
    val inputs = List("a", "b", "c", "d")
    val s = FlashFill()
    s.add(inputs, "ab...")
    val prog = s.solve()
    prog should not be 'empty
    evalProg(prog.get)(Array("a", "b","c", "d")) should equal (StringValue("abcd"))
  }
  
  it should "compute loops medium" in {
    val inputs = List("a", "b", "c", "d")
    val s = FlashFill()
    s.add(inputs, "a,a,b,b,...")
    val prog = s.solve()
    prog should not be 'empty
    evalProg(prog.get)(Array("a", "b","c", "d")) should equal (StringValue("a,a,b,b,c,c,d,d,"))
  }
  /*
  it should "compute loops hard" in {
    val inputs = List("a", "b", "c", "d")
    val prog = FlashFill(List(inputs), List("cp a a1;rm a;cp b b1;rm b;..."))
    evalProg(prog.head)(Array("a", "b", "c", "d")) should equal (StringValue("cp a a1;rm a;cp b b1;rm b;cp c c1;rm c;cp d d1;rm d;"))
  }
  */
  it should "compute dag intersections with correct numbering" in {
     val f = new FlashFill()
     val res1 = f.generateStr(Array(""), "001", 0)
     val res2 = f.generateStr(Array("001"), "002", 0)
     val res3 = intersect(res1, res2)
     val program = res3.takeBest
     println(Printer(program))
     evalProg(program)(Array("002")) should equal (StringValue("003"))
  }
  
  it should "compute dag intersections with correct numbering with constants" in {
     val f = new FlashFill()
     val res1 = f.generateStr(Array(""), "001,100", 0)
     val res2 = f.generateStr(Array("001,100"), "002,102", 0)
     val res3 = intersect(res1, res2)
     val program = res3.takeBest
     println(Printer(program))
     evalProg(program)(Array("002,102")) should equal (StringValue("003,104"))
  }
  
  it should "Find words after the first dot" in {
     val c = FlashFill()
    c.setUseNumbers(true)
    c.setUseDots(true)
    c.setLoopLevel(0)
    c.add(Array("Dr. Best is"), "Best")
    c.add(Array("Th t. Amazonia is nt"), "Amazonia")
     val program = c.solve()
     evalProg(program.get)(Array("Dr. Best is")) should equal (StringValue("Best"))
  }
}
