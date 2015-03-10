package ch.epfl.lara.synthesis.stringsolver

import scala.collection.mutable.ListBuffer

import Program.{Identifier => PIdentifier, _}
import ImperativeProgram.{Identifier, newVar, Stat, Script, Block}
import StringSolver.{Input_state}

sealed trait TypeExpr
case class TFunc(in: TypeExpr, out: TypeExpr) { override def toString = s"$in => $out" }
case class TList(tpe: TypeExpr) extends TypeExpr  { override def toString = s"List[$tpe]" }
case object TString extends TypeExpr  { override def toString = s"String" }

trait ExportableProgram[-In, +Out] {
  
  
  def toStat(return_identifier: Identifier): Stat
  
  def apply(in: In, index: Option[Int] = None): Out
}
sealed trait HasTypeFunc[-In, +Out] { self: ExportableProgram[In, Out] =>
  def name: String
  def tpe: TFunc
  type Comment = String
  def andThenPossible[In2, Out2](h: HasTypeFunc[In2, Out2]): (Boolean, Comment) = 
  (tpe, h.tpe) match {
    case (TFunc(i, o), TFunc(i2, o2)) if o == i2 => (true, "")
    case (TFunc(i, o@TList(lo)), TFunc(i2, o2)) if lo == i2 => (false, s"$name (out: ${tpe.out}) cannot be composed with ${h.name} (in: ${h.tpe.in}), but you can perform it using MAP, e.g. $name andThen (${h.name} as map)")
    case (TFunc(i, o), TFunc(i2@List(li2), o2)) if o == li2 => (false, s"$name (out: ${tpe.out}) cannot be composed with ${h.name} (in: ${h.tpe.in}). but it can with implicit list wrapping around $o")
    case _ => (false, s"$name (out: ${tpe.out}) cannot be composed with ${h.name} (in: ${h.tpe.in})")
  }
}

trait ExportableWithType[-In, +Out] extends ExportableProgram[In, Out] with HasTypeFunc[In, Out] {
  def andThen[In2, Out2](h: ExportableWithType[In2, Out2]): ExportableWithType[In, Out2] = (tpe, h.tpe) match {
    case (TFunc(i, o), TFunc(i2, o2)) if o == i2 => AndThen(this, h)
   // case (TFunc(i, o), TFunc(i2@List(li2), o2)) if o == li2 => AndThen(ListWrapper(this), h)
    case _ => AndThen(this, h)// But should have thrown an error before.
  }
  def | [In2, Out2](h: ExportableWithType[In2, Out2]): ExportableWithType[In, Out2] = andThen(h)
  
  def toScript: Script = {
    if(tpe.in != TString && tpe.in != TList(TString)) {
      println("/!\\ Not possible to convert a script which takes a "+tpe.in+" as argument. Should be a List[String].")
      Script(Block(), newVar())
    } else {
      ImperativeProgram(this)
    }
  }
  def toScala = toScript.toScala
  def toBash = toScript.toBash
  def toPowerShell = toScript.toPowerShell
  def toPowershell = this.toPowerShell
  import ProgramTypes._
  def in(tpe: ProgramType) = tpe match {
    case Scala => toScala
    case Bash => println("/!\\ Bash is not fully supported yet.");toBash
    case PowerShell => toPowerShell
  }
}

//Useless?
/*case class ListWrapper[In, Out](p: ExportableWithType[In, Out]) extends ExportableWithType[In, List[Out]] {
  def name = " as list"
  val tpe = TFunc(p.tpe.in, TList(p.tpe.out))
  def apply(arg: In): List[Out]= List(p(arg))
  def toScript = ImperativeProgram(p)
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}*/

case class TransformProgram(p: Program) extends ExportableWithType[String, String] {
  def name = "transform"
  import Printer._
  override def toString = t"'$p'. Vary the index from 1 to extract all sub-strings.".replaceAll("first input", "string")
  val tpe = TFunc(TString, TString)
  def apply(arg: String, index: Option[Int] = None): String = index match { case Some(i) => p(arg, i) case _ => p(arg) }
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}

case class ReduceProgram(p: Program) extends ExportableWithType[List[String], String] {
  def name = "reduce"
  import Printer._
  override def toString = t"'$p'.".replaceAll(" input", " string")
  val tpe = TFunc( TList(TString), TString)
  def apply(arg: List[String], index: Option[Int] = None): String = index match {
    case Some(i) => p(Input_state(arg.toIndexedSeq, i))
    case None =>  p(arg: _*)
  }
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}

object MapProgram {
  def apply[In, Out](p: ExportableWithType[In, Out]) = Mapper(p)
  def apply(p: Program) = Mapper[List[String], String](ReduceProgram(p))
}

case class SplitProgram(p: Program) extends ExportableWithType[String, List[String]] {
  def name = "split"
  val tpe = TFunc(TString, TList(TString))
  import Printer._
  override def toString = t"'$p'. Vary the index from 1 to extract all sub-strings.".replaceAll("first input", "string")
  def apply(arg: String, index: Option[Int] = None): List[String] = {
    var res = ListBuffer[String]()
    var i = 2
    var tmp = ""
    do {
      try { tmp = p(arg, i) } catch { case e: Exception => tmp = "" }
      if(tmp != "") res += tmp
      i += 1
    } while(tmp != "")
    res.toList
  }
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}

case class PartitionProgram(determiningSubstring: Program) extends ExportableWithType[List[String], List[List[String]]]  {
  def name = "partition"
  val tpe = TFunc(TList(TString), TList(TList(TString)))
  import Printer._
  override def toString = t"Groups strings where '$determiningSubstring' is the same.".replaceAll("first input", "string")
  
  import collection.mutable.{LinkedHashMap, LinkedHashSet}

	implicit class GroupByOrderedImplicit[A](t: Traversable[A]) {
	  def groupByOrdered[K](f: A => K): LinkedHashMap[K, LinkedHashSet[A]] = {
	    val map = LinkedHashMap[K,LinkedHashSet[A]]()
	    for (i <- t) {
	      val key = f(i)
	      map(key) = map.lift(key).getOrElse(LinkedHashSet[A]()) + i
	    }
	    map
	  }
	}

  
  def apply(args: String*): List[List[String]] = apply(args.toList, None)
  def apply(arg: List[String], index: Option[Int] = None): List[List[String]] = {
    val run = arg.map(elem => (elem, try  { determiningSubstring(elem)} catch { case e: Exception => elem } ))
    run.groupByOrdered(_._2).map(_._2.map(_._1).toList).toList
  }
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}

case class FilterProgram(determiningSubstring: Program, shouldEqual: String) extends ExportableWithType[List[String], List[String]] {
  import Printer._
  def name = "filter"
  val tpe = TFunc(TList(TString), TList(TString))
  override def toString = t"Filter strings where '$determiningSubstring' is '$shouldEqual'.".replaceAll("first input", "string")
  def apply(args: String*): List[String] = apply(args.toList, None)
  def apply(arg: List[String], index: Option[Int] = None): List[String] = {
    val run = arg.filter(elem => try  { determiningSubstring(elem) == shouldEqual } catch { case e: Exception => false } )
    run
  }
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}

case class Mapper[-In, +Out](e: ExportableWithType[In, Out] ) extends ExportableWithType[List[In], List[Out]]  {
  def name = s"${e.name} as map"
  override def toString = s"Mapped version of $e"
  val tpe: TFunc = TFunc(TList(e.tpe.in), TList(e.tpe.out))
  
  def apply(arg: List[In], index: Option[Int] = None): List[Out] = {
    arg.zipWithIndex.map{ case (a, i) => e(a, Some(i)) }
  }
  
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}

case class AndThen[-In1, Out1, In2, +Out2](p1: ExportableWithType[In1, Out1] , p2: ExportableWithType[In2, Out2] ) extends ExportableWithType[In1, Out2]  {
  val (res, c) = p1 andThenPossible p2
  if(!res) println(c)
  def tpe = TFunc(p1.tpe.in, p2.tpe.out)
  def name = s"${p1.name} andThen ${p2.name}"
  override def toString = s"${p1} \nOnce done, take the output as the input for the following:\n ${p2}"
  def apply(arg: In1, index: Option[Int] = None): Out2 = {
    p2(p1(arg, index).asInstanceOf[In2], index)
  }
  def toStat(return_identifier: Identifier): Stat = ImperativeProgram.fromProgram(this, return_identifier, true)
}


