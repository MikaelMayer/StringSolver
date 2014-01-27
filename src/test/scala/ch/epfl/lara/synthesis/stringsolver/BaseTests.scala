package ch.epfl.lara.synthesis.stringsolver

import org.scalatest._
import org.scalatest.matchers._

class PrinterTest extends FlatSpec with ShouldMatchers with PrivateMethodTester {
  import Programs._
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
  import Programs._
  import scala.language._

  
  val v1 = InputString(0)
  val w = Identifier("w")

  import Evaluator._
  import StringSolver._
  
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
      
    val pNumber = Concatenate(ConstStr("myFile"), NumberMap(SubStr2(v1, NumTok, 1), 3, 2))
    pNumber("My number 1 should increase") should equal ("myFile003")
    pNumber("My number 4 should increase") should equal ("myFile006")
      
    val pos = Pos(TokenSeq(StartTok, UpperTok, LowerTok), TokenSeq(NonLowerTok, NonDotTok), 1)
    evalProg(pos)(Input_state(IndexedSeq("Algorithm1.pdf"), 1)).asInt should equal(9)
  }
}

class ScalaRegExpTest extends FlatSpec with ShouldMatchers  {
  import Programs._
  import ScalaRegExp._
  
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
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import StringSolver._
  import StringSolver._
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

class StringSolverTest extends FlatSpec with ShouldMatchers with PrivateMethodTester {
  import Programs._
  import ProgramsSet._
  import ScalaRegExp._
  import StringSolver._
  import Implicits._
  import Evaluator._
  
  val generateStr = PrivateMethod[STraceExpr]('generateStr)
  
  val f = new StringSolverAlgorithms()
import f._
  initStats()
  
  "StringSolver" should "correcly compute positions" in {
    val c = "AB12a.  .45? #AB"
    for(i <- 0 to c.length; spos <- f.generatePosition(c, i); pos <- spos) {
      evalProg(pos)(Input_state(IndexedSeq(c), 0)).asInt should equal(i)
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
    val res = generateSubString(Input_state(IndexedSeq("5555", "0abb"), 0), "abb")
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
    val res = generateSubString(Input_state(IndexedSeq("[Various-PC]_Some_Name_178_HD_[e2813be1].mp4"), 0), "178")
    val expected = SSubStr(InputString(0), Set(SPos(STokenSeq(Nil),STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),Set(1))),Set(SPos(STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),STokenSeq(Nil),Set(1))), SSubStrFlag(List(NORMAL)))
    val c = res.map(intersectAtomicExpr(_, expected))
    c.filter(_ != SEmpty).flatten should not be 'empty
    
    val res2 = generateSubString(Input_state(IndexedSeq("[Various-PC]_Some_Name_178_HD_[e2813be1].mp4"), 0), "2813")
    val expected2 = SSubStr(InputString(0), Set(SPos(STokenSeq(Nil),STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),Set(2))),Set(SPos(STokenSeq(List(SToken(List(NumTok))(Programs.listTokens))),STokenSeq(Nil),Set(2))), SSubStrFlag(List(NORMAL)))
    res2.map(intersectAtomicExpr(_, expected2)).filter(_ != SEmpty).flatten should not be 'empty
  }
  
  it should "compute compute intersection of semi-linear sets" in {
    val s1 = SIntSemiLinearSet(1, 3, 11)
    val s2 = SIntSemiLinearSet(0, 2, 13)
    val s3 = SIntSemiLinearSet(0, 2, 3)
    val s4 = SIntSemiLinearSet(-1, 3, 8)
    intersectIntSet(s1, s2) should equal (SIntSemiLinearSet(4,6,11))
    intersectIntSet(s1, s3) should equal (SEmpty)
    intersectIntSet(s2, s3) should equal (s3)
    intersectIntSet(s1, s4) should equal (SEmpty)
    intersectIntSet(s2, s4) should equal (SIntSemiLinearSet(2,6,8))
    val c2 = SCounter(SIntSemiLinearSet(3,1,3),SIntSemiLinearSet(1,0,1),1,0)
    val c1 = SCounter(SIntSemiLinearSet(3,1,3),SIntSemiLinearSet(0,1,7),7,1)
    intersectAtomicExpr(c1, c2) should equal(SCounter(SIntSemiLinearSet(3,1,3),SIntSemiLinearSet(1,0,1),7,1))
  }
  
  it should "compute loops easily" in {
    val inputs = List("a", "b", "c", "d")
    val s = StringSolver()
    s.add(inputs, "ab...")
    val prog = s.solve()
    prog should not be 'empty
    prog.get.apply(IndexedSeq("a", "b", "c", "d"), 0) should equal (StringValue("abcd"))
  }
  
  it should "compute loops medium" in {
    val inputs = List("a", "b", "c", "d")
    val s = StringSolver()
    s.setMaxSeparatorLength(0)
    s.add(inputs, "a,a,b,b,...")
    val prog = s.solve()
    prog should not be 'empty
    prog.get.apply(IndexedSeq("a", "b", "c", "d"), 0) should equal (StringValue("a,a,b,b,c,c,d,d,"))
  }
  it should "compute loops medium with separator" in {
    val inputs = List("a", "b", "c", "d")
    val s = StringSolver()
    s.add(inputs, "a,a,b,b...")
    val prog = s.solve()
    prog should not be 'empty
    prog.get.apply(IndexedSeq("a", "b", "c", "d"), 0) should equal (StringValue("a,a,b,b,c,c,d,d"))
  }
  
  it should "compute loops hard" in {
    val inputs = List("a", "b", "c", "d")
    val c = StringSolver()
    //c.setVerbose(true)
    //c.setTimeout(5000)
    c.setMaxSeparatorLength(0)
    c.add(inputs, "cp a a1;rm a;cp b b1;rm b;...")
    val prog = c.solve().get
    prog(IndexedSeq("a", "b", "c", "d"), 0) should equal (StringValue("cp a a1;rm a;cp b b1;rm b;cp c c1;rm c;cp d d1;rm d;"))
  }
  
  
  it should "compute splits" in {
    val inputs = List("a,b,c,d")
    val outputs = List("a","b","...")
    val s = StringSolver()
    s.add(inputs, outputs)
    val prog = s.solve()
    //s.solve("d,e,f,g") should equal ("d | e | f | g")
    //TODO
  }
  
  it should "compute dag intersections with correct numbering" in {
     val f = new StringSolverAlgorithms()
     val res1 = f.generateStr(Input_state(IndexedSeq("000"), 0), "001", 0)
     val res2 = f.generateStr(Input_state(IndexedSeq("006"), 1), "007", 0)
     val res3 = intersect(res1, res2)
     val program = res3.takeBest
     println(Printer(program))
     program("009") should equal ("010")
  }
  
  it should "compute dag intersections with correct numbering with constants" in {
     val f = new StringSolverAlgorithms()
     val res1 = f.generateStr(Input_state(IndexedSeq("000,99"), 0), "100,001", 0)
     val res2 = f.generateStr(Input_state(IndexedSeq("100,001"), 0), "002,101", 0)
     val res3 = intersect(res1, res2)
     val program = res3.takeBest
     println(Printer(program))
     program("005,106") should equal ("107,006")
  }
  
  it should "compute simple number sequences" in {
    val f = StringSolver()
    f.setTimeout(10000)
    f.add(List("AB00"), "AB-00-1")
    f.add(List("A652"), "A-652-2")
    f.add(List("C14"), "C-14-3")
    f.add(List("E56"), "E-56-4")
    println(Printer(f.solve().get))
    f.solve(List("D3"))(0) should equal ("D-3-5")
  }
  it should "prefer numbers over strings" in {
    val f = StringSolver()
    val c = f.add("Algorithm1.txt -> Algorithm01.txt")
    println(Printer(c.takeBest))
    f.solve("Algorithm10.txt") should equal ("Algorithm10.txt")
  }
}

