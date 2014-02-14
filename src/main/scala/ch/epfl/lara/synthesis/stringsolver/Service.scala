/**
 *     _____ _       _         _____     _             
 *    |   __| |_ ___|_|___ ___|   __|___| |_ _ ___ ___ 
 *    |__   |  _|  _| |   | . |__   | . | | | | -_|  _|
 *    |_____|_| |_| |_|_|_|_  |_____|___|_|\_/|___|_|  
 *                        |___|      
 * 
 *  File:   Service.scala
 *  Author: Mikaël Mayer
 *  Date:   15.01.2014
 *  Purpose:Provides filter and partition as a service.
 */
package ch.epfl.lara.synthesis.stringsolver

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{HashMap => MMap}

object Service {
  import Main.Options

  /**
   * Creates a filter out of a list of input/output.
   * @param inputOutput A list of strings partitioned into taken and not taken.
   * @returns (A StringSolver instance which outputs a value equal to the second value of this tuple if the string is accepted,
   *           The substring common to accepted strings)
   *           
   * Example 1: Filter by extension
   * 
   * val Some((c, s)) = Service.getFilter(List(("abc.txt",true), ("defg.txt",true), ("abc2.jpg",false)))
     s should equal (".txt")
     c.solve("file.txt") should equal (s)
     c.solve("myotherfile.txt") should equal (s)
     c.solve("file.pdf") should not equal (s)
     c.solve("file.jpg") should not equal (s)
   * 
   * Example 2: Filter by name prefix
   * 
   * val Some((c, s)) = Service.getFilter(List(("abc1.txt",true), ("defg1.txt",false), ("abc2.txt",true)))
     s should equal ("abc")
     c.solve("file.txt") should not equal (s)
     c.solve("myotherfile.txt") should not equal (s)
     c.solve("abc3.pdf") should equal (s)
     c.solve("abc10.jpg") should equal (s)
   */
  def getFilter(examples: Seq[(String, Boolean)], c: StringSolver = StringSolver(), opt: Options = Options()): Option[(StringSolver, String)] = {
    val debug = opt.debug
    val filterings = examples.groupBy(_._2).toList.sortBy({ case (taken, value) => if(taken) 1 else 2 })
    if(filterings.length < 2) {
      println("# Filtering requires at least two different mappings accepting and not accepting. Got "+filterings.length)
      if(debug) println(filterings)
      return None
    }
    val filterings_w_substrings = filterings map { case (accepted, filtering) => 
      val files = filtering map { case (in, out) => in }
      val common_substrings = files match {
        case List(a) => substrings(a)
        case List(a,b) => intersect(a, b)
        case a::b::q => ((intersect(a, b) /: q){ case (s, a) => s intersect substrings(a) })
      }
      (accepted, (common_substrings, filtering))
    }
    //val substring_to_category = ListBuffer[(String, String)]()
    val filterings_w_determining_substrings = filterings_w_substrings map {
      case (accepted, (substrings, filtering)) =>
        if(accepted) {
          val other_substrings = (filterings_w_substrings.toList.filter{ case (other_accepted, (l2, p)) => !other_accepted }.flatMap{ case (other_accepted, (l2, p)) => l2 } )
          val smallest_set = substrings -- other_substrings
          if(smallest_set.isEmpty) {
            println(s"# Files in accepted ${accepted} do not share a common string not found in others.")
            return None
          }
          (accepted, (smallest_set.toList.sortBy(e => e.length).last, filtering))
        } else {
          (accepted, ("", filtering))
        }
        
        //val representative = smallest_set.toList.sortBy(_.length).last
        //substring_to_category += representative -> accepted
    }
    
    /** To find out which of the substring in the smallest set is determining the filter,
     *  we need to intersect all corresponding programs to generate each one of them. */
    
    // Adding mappings
    val determiningSubstring: String = filterings_w_determining_substrings find {
      case (accepted, _) => accepted} match {
      case Some((_, (determining_substring, _))) =>
        determining_substring
      case None => // cannot happen
        ""
    }
    filterings_w_determining_substrings.toList foreach {
      case (accepted, (_, filtering)) =>
        import ProgramSet._
        import Program._
        if(accepted) {
          filtering foreach {
             case (in, out) =>
               c.add(List(in),determiningSubstring)
          }
        }
    }
    Some((c, determiningSubstring))
  }
  
  /**
   * Creates a partition out of a list of input/output.
   * @param inputOutput A list of strings partitioned along the equivalence class of the second member of the tuple.
   * @returns (A StringSolver instance which outputs the determining substring from the name,
   *           A StringSolver instance which maps the determining substring to the example output,
   *           A function which uses the stringSolver instance to produce the corresponding substring, and the identity if it fails)
   * 
   * These three example can be combined.
   *    
   * //Example 1: Name of categories unrelated to the name of files.
   * 
   * val Some((c, c2, s)) = Service.getPartition(List(("abc.txt","blue"), ("defg.txt","blue"), ("hijk.jpg","red"), ("lmnop.jpg","red")))
     c.solve("file.txt") should equal (".txt")
     c.solve("file.jpg") should equal (".jpg")
     c.solve("file.pdf") should equal (".pdf")
     c2.solve(".txt") should equal ("")
     s(".txt") should equal ("blue")
     s(".jpg") should equal ("red")
     s(".pdf") should equal (".pdf")
   * 
   * //Example 2: Categories are numbered
   * 
   * val Some((c, c2, s)) = Service.getPartition(List(("abc.txt","1"), ("defg.txt","1"), ("hijk.jpg","2"), ("lmnop.jpg","2")))
     c.solve("file.txt") should equal (".txt")
     c.solve("file.jpg") should equal (".jpg")
     c.solve("file.pdf") should equal (".pdf")
     //c2.solve(".txt") should equal ("3") // Do not call because else the counter will increment and the following tests would be wrong
     s(".txt") should equal ("1")
     s(".jpg") should equal ("2")
     s(".pdf") should equal ("3")
     s(".png") should equal ("4")
   * 
   * //Example 3: Name of categories related to the name of files.
   * 
   * val Some((c, c2, s)) = Service.getPartition(List(("abc.txt","category-txt"), ("defg.txt","category-txt"), ("hijk.jpg","category-jpg"), ("lmnop.jpg","category-jpg")))
     c.solve("file.txt") should equal (".txt")
     c.solve("file.jpg") should equal (".jpg")
     c.solve("file.pdf") should equal (".pdf")
     c2.solve(".pdf") should equal ("category-pdf")
     s(".txt") should equal ("category-txt")
     s(".jpg") should equal ("category-jpg")
     s(".pdf") should equal ("category-pdf")
     s(".png") should equal ("category-png")
   */
  def getPartition(examples: Seq[(String, String)], c: StringSolver = StringSolver(), c2: StringSolver = StringSolver(), opt: Options = Options()): Option[(StringSolver, Option[StringSolver], String => String)] = {
    val debug = opt.debug || c.isVerbose
    c.setUseNumbers(false)
    c.setUseDots(false)
    if(c.isVerbose) println(s"Starting partition")
    val category_order = examples.map({case (in, out) => out }).distinct
    val partitions = category_order map { case cat => (cat, examples.filter({ case (in, out) => out == cat})) }
    if(c.isVerbose) println(s"Starting substring extraction...")
    val partitions_w_substrings = partitions map { case (category, partition) => 
      val files = partition map { case (in, out) => in}
      val common_substrings = files match {
        case List(a) => substrings(a)
        case List(a,b) => intersect(a, b)
        case a::b::q => ((intersect(a, b) /: q){ case (s, a) => s intersect substrings(a) })
      }
      if(common_substrings.isEmpty) {
        println("# Impossible to determine the common substrings of " + files.mkString(","))
        return None
      }
      (category, (common_substrings, partition))
    }
    val substring_to_category_w_alternatives = MMap[String, String]()
    
    // Partitions sorted by 
    if(c.isVerbose) println(s"Determining unique greater substrings to all partitions...")
    val partitions_w_determining_substrings = partitions_w_substrings map {
      case (category, (substrings, partition)) =>
        val other_substrings = (partitions_w_substrings.toList.filter{ case (c, (l2, p)) => c != category }.flatMap{ case (c, (l2, p)) => l2 } )
        val smallest_set = substrings -- other_substrings
        if(smallest_set.isEmpty) {
          println(s"# Partition ${category} are too similar to other categories to be put in a different partition")
          return None
        }
        val smallest_set_list = smallest_set.toList.sortBy(s => -s.length)
        val representative = smallest_set_list.head
        val alternatives = smallest_set_list.tail
        substring_to_category_w_alternatives += representative -> category
        (category, (representative, partition, alternatives))
    }
    
    // Adding mappings starting with smaller categories
    var previousCategory: Option[StringSolver] = None
    var stoppedLocal = false
    var stoppedGeneral = false
    val iteration = partitions_w_determining_substrings.toList.sortBy(s => s._1.length)
    
    if(c.isVerbose) println(s"Solving substring extraction for all partitions...")
    val solvers_by_category = for((category, (determining_substring, partition, alternatives)) <- iteration) yield {
      val cTemp = StringSolver()
      cTemp.setUseNumbers(false)
      cTemp.setUseDots(false)
      cTemp.setVerbose(c.isVerbose)
      cTemp.setTimeout(2)
      cTemp.setExtraTimeToComputeLoops(0.1f)
      stoppedLocal = false
      for((in, out) <- partition; if(!stoppedLocal)) {
        val res = cTemp.add(List(in),determining_substring)
        if(cTemp.solve() == None) stoppedLocal = true
        if(!stoppedLocal && !stoppedGeneral) {
          c.add(res)
          if(c.solve() == None) {
            stoppedGeneral = true
            if(c.isVerbose) println(s"Basic algorithm stopped when computing example $in")
          }
        }
        if(stoppedLocal) {
          if(c.isVerbose) println(s"Basic algorithm stopped for partition $category when computing example $in")
        }
      }
      (category, cTemp)
    }
    val (final_c, substring_to_category) = if(stoppedGeneral) { // We need to have another strategy
      if(c.isVerbose) println("Basic partitioning algorithm did not work. Advanced algorithm")
      // Takes the program for each of the partition,
      // applies it on all other provided examples.
      // If it results in non-empty strings, the program giving the longest working strings works.
      val candidates_raw = for((category, solver) <- solvers_by_category if solver.solve() != None) yield {
        stoppedLocal = false
        val newMapping = for((category2, (determining_substring, partition, alternatives)) <- iteration if !stoppedLocal) yield {
          if(c.isVerbose) println(s"Generalizing category $category to category $category2")
          
          val results = if(category == category2) { List((determining_substring)) } else  {
              (for((in, out) <- partition) yield {
              solver.solve(in)
            }).toList.distinct
          }
          results match {
            case List(a) if ((alternatives contains a) || determining_substring == a) && a != "" => // Then it's a good representative substring for this category
              if(c.isVerbose) println(s"Other substring for category $category2: $a rather than $determining_substring")
              (a, category2)
            case _ =>
              stoppedLocal = true
              (determining_substring, category2)
          }
        }
        if(!stoppedLocal) { // Then this one works for all input.
          if(c.isVerbose) println(s"Better tested algorithm for category $category")
          List((solver, newMapping.toMap))
        } else Nil
      }
      val candidates = candidates_raw.flatten
      if(candidates != Nil) {
        candidates/*.sortBy(f)*/.head
      } else (c, substring_to_category_w_alternatives)
    } else (c, substring_to_category_w_alternatives)
    // Take the one for which the category is the smallest, and then aggregates
    
    //val substring_to_category = List[(String, String)]()
    val category_to_substring = substring_to_category.toList.map({ case (a, b) => (b, a)}).toMap
    /**
     * Solver to find a mapping between the representative and the category
     * Finds counters, but also substrings from representative
     */
    val c2 = StringSolver()
    category_order map { case i => (i, category_to_substring(i))} foreach {
      case (category, representative) =>
        if(debug) {
          println(s"Add c2 $representative -> $category")
        }
        c2.add(List(representative), category)
    }
    val extractor = if(c2.solve() != None) Some(c2) else None
    Some((final_c, extractor, new (String => String) {
      val substring_to_category_map = MMap() ++ substring_to_category
      def apply(representative: String): String = {
        substring_to_category_map.getOrElseUpdate(representative, {
          val tmp = c2.solve(representative)
          val res = if(tmp == "") representative else tmp
          if(debug) println(s"Category of $representative is $res")
          res
        })
      }
    }))
  }
  
  
  /**
   * Returns a set of all common substrings
   */
  private def intersect(s1: String, s2: String): Set[String] = {
    (for(i <- 0 until s1.length;
        j <- (i+1) to s1.length;
        c = s1.substring(i, j)
        if s2.indexOf(c) != -1) yield c).toSet
  }
  
  /**
   * Returns the set of all substrings
   */
  private def substrings(s1: String): Set[String] = {
    (for(i <- 0 until s1.length;
          j <- (i+1) to s1.length;
          c = s1.substring(i, j)) yield c).toSet
  } 
}