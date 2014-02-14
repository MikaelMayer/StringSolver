/**File name: package.scala
 * Author   : Mikaël Mayer
 * Date     : 14.02.2014
 * Function : Provides top-level functions for this package.
 */
package ch.epfl.lara.synthesis

package object stringsolver {
  def renderer(c: StringSolver): Unit = {
    c.solve() match {
      case Some(prog) => println(Printer(prog))
      case none => println("No program found")
    }
  }
}