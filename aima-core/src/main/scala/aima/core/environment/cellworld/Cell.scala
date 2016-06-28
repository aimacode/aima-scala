package aima.core.environment.cellworld


/**
  * Artificial Intelligence A Modern Approach (3rd Edition): page 645.<br>
  * <br>
  * A representation of a Cell in the environment detailed in Figure 17.1.
  *
  * @param x the x coordinate of the cell
  * @param y the y coordinate of the cell
  * @param content the content of the cell
  * @tparam C the content type of the cell
  * @author Shawn Garner
  * @author Ciaran O'Reilly
  * @author Ravi Mohan
  */
case class Cell[C](x: Int = 1, y: Int = 1, content: Option[C] = None)


