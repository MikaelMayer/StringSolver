package ch.epfl.lara.synthesis.flashfill

import org.scalatest._
import org.scalatest.matchers._

class PrinterTest extends FlatSpec with ShouldMatchers with PrivateMethodTester {
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
    
    val p = Loop(Identifier("w"), Concatenate(SubStr2(v1, UpperTok, w+1)), None)
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
    concatenate(StringValue("a"), BottomValue, StringValue("c")) should equal (BottomValue)
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
  
  it should "correctly execute programs" in {
    val p =Concatenate(ConstStr(" + "), SubStr(v1, Pos(NumTok, AlphaTok, IntLiteral(2)), CPos(-1)))
    p("qsdf1234amlkj12345mlkj432  fkj ") should equal (" + mlkj432  fkj ")
    
    val p2 = Loop(Identifier("w"),Concatenate(SubStr2(v1, UpperTok, w+1)), None)
    p2("My Taylor is Rich") should equal ("MTR")
    
    val p3 = SubStr(v1, Pos(Epsilon, NumTok, 1), CPos(-1))
    p3("CAMP DRY DBL NDL 3.6 OZ") should equal ("3.6 OZ")
    
    val pp1 = Pos(LeftParenTok, TokenSeq(NumTok, SlashTok), w+1)
    val pp2 = Pos(TokenSeq(SlashTok, NumTok), RightParenTok,w+1)
    val p4 = Loop(w, Concatenate(SubStr(v1, pp1, pp2), ConstStr(" # ")), None)
    p4("(6/7)(4/5)(14/1)") should equal ("6/7 # 4/5 # 14/1 # ")

    val firstname = SubStr(v1, Pos(Epsilon, TokenSeq(AlphTok, NonDotTok), 1), Pos(Epsilon, TokenSeq(LowerTok, NonDotTok), 1))
    val e1 = Concatenate(SubStr(v1, Pos(Epsilon, TokenSeq(AlphTok, CommaTok), 1), Pos(AlphTok, CommaTok, 1)), ConstStr(", "), firstname, ConstStr("."))
    val e2 =  Concatenate(SubStr2(v1, AlphTok, -1), ConstStr(", "), firstname, ConstStr("."))
    val p5 = Switch((Match(v1, CommaTok, 1), e1), (NotMatch(v1, CommaTok, 1), e2))
      
    p5("Dr. Eran Yahav") should equal ("Yahav, E.")
    p5("Prof. Kathleen S. Fisher") should equal ("Fisher, K.")
    p5("Bill Gates, Sr.") should equal ("Gates, B.")
    p5("George Ciprian Necula") should equal ("Necula, G.")
    p5("Ken McMillan, II") should equal ("McMillan, K.")
      
    val p61 = Pos(Epsilon, NonSpaceTok, w+1)
    val p62 = Pos(NonSpaceTok, TokenSeq(SpaceTok, NonSpaceTok), w+1)
    val p6 = Concatenate(Loop(w, Concatenate(SubStr(v1, p61, p62), ConstStr(" ")), None), SubStr2(v1, NonSpaceTok, -1))
      
    p6("Oege      de        Moor")  should equal ("Oege de Moor")
    p6("Kathleen         Fisher    AT&T Labs") should equal ("Kathleen Fisher AT&T Labs")
      
    val pNumber = Concatenate(ConstStr("myFile"), Number(SubStr2(v1, NumTok, 1), 3, (5, 2)))
    pNumber("My number 1 should increase") should equal ("myFile003")
    pNumber("My number 4 should increase") should equal ("myFile006")
      
    val pos = Pos(TokenSeq(StartTok, UpperTok, LowerTok), TokenSeq(NonLowerTok, NonDotTok), 1)
    evalProg(pos)(IndexedSeq("Algorithm1.pdf")).asInt should equal(9)
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
    computePositionsEndingWith(UpperTok, "UZEabOPQ") should equal (List(2, 7))
    
    computePositionsEndingWith(TokenSeq(StartTok, UpperTok), "UZEabOPQ") should equal (List(2))
    
    computePositionsEndingWith(TokenSeq(LowerTok, SlashTok), "125abc/1/aa1/ab/") should equal (List(6,15))

    computePositionsEndingWith(Epsilon, "0/2/1") should equal (List(-1, 0, 1, 2, 3, 4))
    
    computePositionsEndingWith(TokenSeq(AlphTok), "some words sentence") should equal (List(3, 9, 18))
    
    computePositionsEndingWith(TokenSeq(LeftParenTok), "(6/7)(4/5)(14/1)") should equal (List(0, 5, 10))
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
        + ((2, 3) -> Set(SConstStr("c"):SAtomicExpr,SSubStr(0, Set(SCPos(0)), Set(SCPos(-1)), SSubStrFlag(List(NORMAL))): SAtomicExpr)) 
        + ((0, 2) -> Set(SConstStr("ab"):SAtomicExpr)) 
        + ((1, 3) -> Set(SConstStr("bc"):SAtomicExpr))
    )
    
  "DAG" should "compute neighbors correctly" in {
    
    c.neighbors(0, 0).map(_._3) should equal (Set(1, 2))
  }
  
  it should "compute best paths" in {
    c.takeBest should equal (Concatenate(ConstStr("ab"), SubStr(0, CPos(0), CPos(-1))))
  }
  
  "SToken" should "correctly represent tokens" in {
    SToken(Set(NumTok))(Programs.listTokens).toSet should equal (Set(NumTok))
    SToken(Set(NumTok, HyphenTok))(Programs.listTokens).toSet should equal (Set(NumTok, HyphenTok))
    val input = Set(NumTok, AlphaTok, DotTok, LeftParenTok)
    val input2 = Set(NumTok, AlphaTok, RightParenTok, LeftParenTok)
    SToken(input)(Programs.listTokens).map((i: Token) =>i).toSet should equal (input)
    
    (input intersect input2).map((i: Token) =>i).toSet should equal (Set(NumTok, AlphaTok, LeftParenTok))
  }
}

class FlashFillTest extends FlatSpec with ShouldMatchers with PrivateMethodTester {
  import ch.epfl.lara.synthesis.flashfill._
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import FlashFill._
  import Implicits._
  import Evaluator._
  
  val generateStr = PrivateMethod[STraceExpr]('generateStr)
  
  val f = new FlashFillSolver()
  import f._
  initStats()
  
  "flashfill" should "correcly compute positions" in {
    val c = "AB12a.  .45? #AB"
    for(i <- 0 to c.length; spos <- f.generatePosition(c, i); pos <- spos) {
      evalProg(pos)(IndexedSeq(c)).asInt should equal(i)
    }
  }
  
  it should "correcly compute partitions of tokens" in {
    Reps("a     b")._1.toSet should equal(Set(NonDotTok, NonUpperTok, BackTickTok, LowerTok, NonLowerTok))
  }
  
  it should "correctly compute token seqs" in {
    val sa = computetokenSeq("a", listNonEmptyTokens)(0, 0) map (_._1)
    sa should contain(TokenSeq(AlphaTok))
    sa should not contain(TokenSeq(StartTok, StartTok, AlphaTok))
    sa should not contain(TokenSeq(AlphaTok, EndTok, EndTok))
    sa should contain(TokenSeq(StartTok, AlphaTok))
    sa should contain(TokenSeq(AlphaTok, EndTok))
    sa should contain(TokenSeq(NonNumTok, EndTok))
  }
  
  it should "correctly compute token seqs for longer strings" in {
    val sa = computetokenSeq("a     b", listNonEmptyTokens)(0, 5) map (_._1)
    sa should contain(TokenSeq(AlphaTok, SpaceTok))
    val sb = computetokenSeq("a     b", listNonEmptyTokens)(1, 6) map (_._1)
    sb should contain(TokenSeq(SpaceTok, AlphaTok))
  }
  
  it should "correctly compute number positions." in {
     val res = ("0011" addedNumberFrom "1 2 10 15").toList
     res should contain ((0, 0, 10))
     res should contain ((2, 2, 9))
     res should contain ((4, 5, 1))
  }
  
  it should "correctly find numbers" in {
     "I god 0011,2 on my 31".numbers should equal("      0011 2       31")
  }
  
  it should "compute atomic subexpressions" in {
    val res = generateSubString(IndexedSeq("5555", "0abb"), "abb")
    res.size should equal(1)
    res.headOption match {
      case Some(SSubStr(vi, spos, epos, method)) => 
        method should contain (NORMAL: SubStrFlag)
        method should contain (CONVERT_LOWERCASE: SubStrFlag)
        vi should equal(InputString(IntLiteral(1)))
        spos should contain (SCPos(1):SPosition)
        spos should contain (SCPos(-4):SPosition)
        val notconst = spos.toTraversable.filterNot { case SCPos(_) => true case _ => false}
        notconst should not be 'empty
        notconst find {
          case SPos(STokenSeq(List(beforeTokens@SToken(_))), STokenSeq(List(afterTokens@SToken(_))), nth) if !(afterTokens contains NonDotTok) =>
            if(beforeTokens contains NumTok) {
              beforeTokens should contain (NumTok: Token)
              beforeTokens should contain (NonLowerTok: Token)
              beforeTokens should contain (NonAlphaTok: Token)
              afterTokens should contain (NonNumTok: Token)
              afterTokens should contain (LowerTok: Token)
              afterTokens should contain (AlphaTok: Token)
              nth should contain (IntLiteral(1): IntegerExpr)
              nth should contain (IntLiteral(-1): IntegerExpr)
              true
            } else false
          case _ => false
        } should not equal (None)
      case None => 
      case _ => throw new Exception(s"$res does not contain a SSubStr.") 
    }
  }
  
  it should "compute find first and second numbers" in {
    val res = generateSubString(IndexedSeq("[Various-PC]_Some_Name_178_HD_[e2813be1].mp4"), "178")
    val expected = SSubStr(InputString(0), Set(SPos(STokenSeq(Nil),STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),Set(1))),Set(SPos(STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),STokenSeq(Nil),Set(1))), SSubStrFlag(List(NORMAL)))
    val c = res.map(intersectAtomicExpr(_, expected))
    c.filter(_ != SEmpty).flatten should not be 'empty
    
    val res2 = generateSubString(IndexedSeq("[Various-PC]_Some_Name_178_HD_[e2813be1].mp4"), "2813")
    val expected2 = SSubStr(InputString(0), Set(SPos(STokenSeq(Nil),STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),Set(2))),Set(SPos(STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),STokenSeq(Nil),Set(2))), SSubStrFlag(List(NORMAL)))
    res2.map(intersectAtomicExpr(_, expected2)).filter(_ != SEmpty).flatten should not be 'empty
  }
  
  it should "compute loops easily" in {
    val inputs = List("a", "b", "c", "d")
    val s = FlashFill()
    s.add(inputs, "ab...")
    val prog = s.solve()
    prog should not be 'empty
    evalProg(prog.get)(IndexedSeq("a", "b", "c", "d")) should equal (StringValue("abcd"))
  }
  
  it should "compute loops medium" in {
    val inputs = List("a", "b", "c", "d")
    val s = FlashFill()
    s.setMaxSeparatorLength(0)
    s.add(inputs, "a,a,b,b,...")
    val prog = s.solve()
    prog should not be 'empty
    evalProg(prog.get)(IndexedSeq("a", "b", "c", "d")) should equal (StringValue("a,a,b,b,c,c,d,d,"))
  }
  it should "compute loops medium with separator" in {
    val inputs = List("a", "b", "c", "d")
    val s = FlashFill()
    s.add(inputs, "a,a,b,b...")
    val prog = s.solve()
    prog should not be 'empty
    evalProg(prog.get)(IndexedSeq("a", "b", "c", "d")) should equal (StringValue("a,a,b,b,c,c,d,d"))
  }
  
  it should "compute loops hard" in {
    val inputs = List("a", "b", "c", "d")
    val c = FlashFill()
    //c.setVerbose(true)
    //c.setTimeout(5000)
    c.setMaxSeparatorLength(0)
    c.add(inputs, "cp a a1;rm a;cp b b1;rm b;...")
    val prog = c.solve().get
    evalProg(prog)(IndexedSeq("a", "b", "c", "d")) should equal (StringValue("cp a a1;rm a;cp b b1;rm b;cp c c1;rm c;cp d d1;rm d;"))
  }
  
  
  it should "compute splits" in {
    val inputs = List("a,b,c,d")
    val outputs = List("a","b","...")
    val s = FlashFill()
    s.add(inputs, outputs)
    val prog = s.solve()
    //s.solve("d,e,f,g") should equal ("d | e | f | g")
    //TODO
  }
  
  it should "compute dag intersections with correct numbering" in {
     val f = new FlashFillSolver()
     val res1 = (f.generateStr)(IndexedSeq("000"), "001", 0)
     val res2 = (f.generateStr)(IndexedSeq("001"), "002", 0)
     val res3 = intersect(res1, res2)
     val program = res3.takeBest
     println(Printer(program))
     program("009") should equal ("010")
  }
  
  it should "compute dag intersections with correct numbering with constants" in {
     val f = new FlashFillSolver()
     val res1 = f.generateStr(IndexedSeq("000,99"), "100,001", 0)
     val res2 = f.generateStr(IndexedSeq("100,001"), "002,101", 0)
     val res3 = intersect(res1, res2)
     val program = res3.takeBest
     println(Printer(program))
     program("005,106") should equal ("107,006")
  }
  
  it should "compute simple number sequences" in {
    val f = FlashFill()
    f.add(List("AB00"), "AB-00-1")
    f.add(List("A652"), "A-652-2")
    f.add(List("C14"), "C-14-3")
    f.add(List("E56"), "E-56-4")
    f.solve(List("D3"))(0) should equal ("D-3-5")
  }
}

