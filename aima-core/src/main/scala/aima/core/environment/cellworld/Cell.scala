package aima.core.environment.cellworld


/**
  * Artificial Intelligence A Modern Approach (3rd Edition): page 645.<br>
  * <br>
  * A representation of a Cell in the environment detailed in Figure 17.1.
  *
  * @param <C> the content type of the cell.
  *
  * @author Ciaran O'Reilly
  * @author Ravi Mohan
  * @author Shawn Garner (Scala version)
  */
case class Cell[C](x: Int = 1, y: Int = 1, content: Option[C] = None)


