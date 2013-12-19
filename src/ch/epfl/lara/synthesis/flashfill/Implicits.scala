

package ch.epfl.lara.synthesis.flashfill

import scala.Option.option2Iterable

object Implicits {
    import Programs._
    implicit class AugString(s: String) {
    def e(i: Int, j: Int) = s.substring(i, j + 1)
    /**
     * Returns the positions of s in source.
     */
    def substringOf(source: String): Seq[Int] = { // TODO : Make it better.
      (0 to (source.length - s.length)).filter{ case pos => source.substring(pos, pos + s.length) == s }
    }
    def isNumber: Boolean = s.length > 1 && (s forall (_.isDigit))
    /** Returns a sequence of substrings which are numbers, with a certain number of digit and offset
     *  Returns the start and ending positions in string, and the increment from the given string.
     *  Returns only positions where the source is a token
     **/
    def subnumberOf(source: String): Seq[(Int, Int, Int)] = {
      val num = s.toInt
      for((i, j) <- ScalaRegExp.computePositionsOfToken(NumTok, source);
          increment <- source.e(i, j) to num) yield {
        (i, j, increment)
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
  }
}