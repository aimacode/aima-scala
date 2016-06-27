package aima.core.environment.cellworld

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
  * @author Shawn Garner
  */
class CellSpec extends Specification with ScalaCheck {

  "no arguments take defaults" in new context {
    val cell = new Cell[String]
    cell.x must_== 1
    cell.y must_== 1
    cell.content must beNone
  }


  "cell must be valid for all int and strings" in prop { (x: Int, y: Int, content: String) =>
    val givenContent = Option(content)
    val cell = new Cell[String](x, y, givenContent)
    cell.x must_== x
    cell.y must_== y
    cell.content must_== givenContent
  }


  trait context extends Scope {
    //any data needed for testing to here
  }
}
