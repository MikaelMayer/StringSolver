/**
 *     _____ _       _         _____     _             
 *    |   __| |_ ___|_|___ ___|   __|___| |_ _ ___ ___ 
 *    |__   |  _|  _| |   | . |__   | . | | | | -_|  _|
 *    |_____|_| |_| |_|_|_|_  |_____|___|_|\_/|___|_|  
 *                        |___|      
 * File name: Implicits.scala
 * Author   : Mikaël Mayer
 * Date     : 14.02.2014
 * Function : Provides implicit string conversions and extractions.
 */

package ch.epfl.lara.synthesis.stringsolver

import scala.Option.option2Iterable
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.regex.Pattern

object Implicits {
  import Program._
  import ProgramSet._
  
  implicit class AugBoolean(s: Boolean) {
    def implies(other: Boolean): Boolean = !s || other
  }
  
  implicit class AugString(s: String) {
    def e(i: Int, j: Int) = s.substring(i, j + 1)
    /**
     * Returns the positions of s in source.
     */
    def substringOf(source: String): Seq[Int] = {
      Pattern.quote(s).r.findAllMatchIn(source).map(_.start(0)).toSeq
    }
    
    /**
     * Returns the positions of s in source with case modificators
     */
    def substringWithCaseOf(source: String): Seq[(Int, SSubStrFlag)] = {
      (0 to (source.length - s.length)).toList.flatMap{
        case pos => 
          val in = source.substring(pos, pos + s.length)
          var res= Set[SubStrFlag](); implicit class Adder(b: Boolean) { def ==>(t: SubStrFlag) = if(b) res += t }
          in == s ==> NORMAL
          CONVERT_LOWERCASE(in) == s ==> CONVERT_LOWERCASE
          CONVERT_UPPERCASE(in) == s ==> CONVERT_UPPERCASE
          UPPERCASE_INITIAL(in) == s ==> UPPERCASE_INITIAL
          if(res.isEmpty) Nil else List((pos, SSubStrFlag(res.toTraversable)))
      }
    }
    
    
    
    def isNumber: Boolean = s.length >= 1 && (s forall (_.isDigit)) && (try{Integer.parseInt(s); true}catch{case e: java.lang.NumberFormatException => false})
    /** Returns a sequence of substrings which are numbers, with a certain number of digit and offset
     *  Returns the start and ending positions in string, and the increment from the given string.
     *  Returns only positions where the source is a token
     **/
    def addedNumberFrom(source: String): Seq[(Int, Int, Int)] = {
      val num = s.toInt
      for((i, j) <- ScalaRegExp.computePositionsOfTokenSimple(NumTok, source);
          increment <- source.e(i, j) to num) yield {
        (i, j, increment)
      }
    }
    /** Returns a sequence of substrings which are numbers, with a certain number of digit and offset
     *  Returns the start and ending positions in string, and the increment from the given string.
     *  Returns only positions where the source is a token
     **/
    def subnumberIncNegativeOf(source: String): Seq[(Int, Int, Int)] = {
      try {
      val num = s.toInt
      for((i, j) <- ScalaRegExp.computePositionsOfTokenSimple(NumTok, source);
          step <- source.e(i, j) reaching num) yield {
        (i, j, step)
      }} catch {
        case e: java.lang.NumberFormatException => Seq()
      }
    }
    
    
    /**
     * Computes an increment from this string to a given number, if it applies.
     */
    def to(t: Int): Option[Int] = {
      if(s.isNumber) {
        val si = s.toInt
        if(si < t) {
          Some(t - s.toInt)
        } else None
      } else None
    }
    
    /**
     * Computes an increment from this string to a given number, if it applies.
     */
    def reaching(t: Int): Option[Int] = {
      if(s.isNumber) {
        val si = s.toInt
        //if(si != t) {
          Some(t - s.toInt)
        //} else None
      } else None
    }
    
    /**
     * Returns the string where all numbers appearing in it appear together separated by spaces
     */
    def numbers: String = {
      (s map {(c: Char) => if(c.isDigit) c else ' '}).mkString("")
    }
    
    /**
     * If this string is a path, retrieves the path until the file inc. Slash
     */
    def getDirectory(): String = {
      s.replaceFirst("""(\\|\/)[^\\/]*$""", """""")
    }
    /**
     * If this string is a path, retrieves the name of the file
     */
    def getFile: String = {
      s.replaceAll(".*\\\\","")
    }
  }
  def timedScope[A](op: => A): (A, Long) = {
    val start = System.nanoTime
    val res = op
    (res, System.nanoTime - start)
  }
  
  def first[T](f: Future[T], g: Future[T]): Future[T] = {
    val p = promise[T]
    f onSuccess {
      case x => p.trySuccess(x)
    }
    g onSuccess {
      case x => p.trySuccess(x)
    }
    p.future
  }
}