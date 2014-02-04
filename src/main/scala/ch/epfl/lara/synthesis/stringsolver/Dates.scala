package ch.epfl.lara.synthesis.stringsolver

import ProgramsSet._
import Programs._

object Dates extends Extension {
  private val converters: List[SpecialConverter] = List(
      SpecialConverter(NumberToDate1, NumberToDate1.accepts),
      SpecialConverter(NumberToDate1Low, NumberToDate1Low.accepts),
      SpecialConverter(NumberToDate2, NumberToDate2.accepts),
      SpecialConverter(NumberToDate2Low, NumberToDate2Low.accepts),
      SpecialConverter(DateToNumber1, DateToNumber1.accepts),
      SpecialConverter(DateToNumber2, DateToNumber2.accepts)
  )
  
  /**
   * Function to iterate over dates
   */
  def apply(input: String, output: String): Seq[(Int, Int, SSubStr => SSpecialConversion)] = {
    for(i <- 0 until input.length;
        j <- (i+1) to input.length;
        ss = input.substring(i, j);
        c = converters.filter((sc: SpecialConverter) => (sc.accepts(ss) && sc(ss) == output)) if c.size > 0)
      yield {
      val res = (i, j, (ss: SSubStr) => SSpecialConversion(ss, c.toSet))
      res
    }
  }
  
  trait FunctionDomain {
    def accepts(i: String): Boolean
  }
  
  /**
   * Converts number to months
   */
  trait ToMonths extends (String => String) {
    def months: Array[String]
    override def apply(s: String): String = if(s.length <= 2 && s.length >= 1 && s(0).isDigit && (if(s.length ==2) s(1).isDigit else true)) {
      val m = s.toInt
      if(m < 1 || m > 12) "" else months(m-1)
    } else {
      ""
    }
    def accepts(s: String): Boolean = {
      s.length >= 1 && s.length <= 2 && s(0).isDigit && (if(s.length ==2) s(1).isDigit else true) && {
        val m = s.toInt
        m >= 1 && m <= 12
      }
    }
  }
  
  trait ToNumber extends (String => String) {
    def numberFormat(i: Int): String
    def accepts(s: String): Boolean = {
      val ss = s.toLowerCase()
      NumberToDate1Low.months.indexOf(ss) != -1 || NumberToDate2Low.months.indexOf(ss) != -1
    }
    
    override def apply(s: String): String = {
      val ss = s.toLowerCase()
      val t1 = NumberToDate1Low.months.indexOf(ss)
      if(t1 != -1) {
        numberFormat(t1+1)
      } else {
        val t2 = NumberToDate2Low.months.indexOf(ss)
        if(t2 != -1) {
          numberFormat(t2+1)
        } else {
          ""
        }
      }
    }
  }
  
  object NumberToDate1 extends ToMonths {
    val months = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  }
  
  object NumberToDate1Low extends ToMonths {
    val months = Array("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  }
  
  object NumberToDate2 extends ToMonths {
    val months = Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  }
  object NumberToDate2Low extends ToMonths {
    val months = Array("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  }
  object DateToNumber1 extends ToNumber {
    def numberFormat(i: Int) = i.toString
  }
  object DateToNumber2 extends ToNumber {
    def numberFormat(i: Int) = {
      if(i < 10) "0" + i
      else i.toString
    }
  }
}