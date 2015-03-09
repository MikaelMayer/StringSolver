package ch.epfl.lara.synthesis.stringsolver

import ProgramSet._

// Data structures:
object NestedTypes {
  trait Output {
    var input: Option[Tree] = None
    var value: Option[String] = None
    var transformations: Option[ProgramSet[_]] = None
    def update(s: String): Unit = { // Called from javascript
      value = Some(s)
    }
  }

  trait Tree
  type Input = Tree
  case class SList[A <: Tree](elem: List[A], mapOutput: List[Output], reduceOutput: Input) extends Tree
  case class STuple(elem: List[Tree]) extends Tree
  case class SCase(value: String) extends Tree

  // Map operation recorded.
  
  

}