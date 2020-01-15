package aima.core.search.uninformed

import aima.core.search.Problem

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * @author Shawn Garner
  */
trait IterativeDeepeningSearch[State, Action] {
  def depthLimitedTreeSearch: DepthLimitedTreeSearch[State, Action]

  def search(
      problem: Problem[State, Action],
      noAction: Action
  ): Try[DLSResult[Action]] = {
    @tailrec def searchHelper(currentDepth: Int): Try[DLSResult[Action]] = {
      val result =
        depthLimitedTreeSearch.search(problem, currentDepth, noAction)

      result match {
        case Success(Solution(_)) | Failure(_) => result
        case _ if currentDepth == Int.MaxValue =>
          Failure[DLSResult[Action]](
            new Exception("Depth has reached Int.MaxValue")
          )
        case _ => searchHelper(currentDepth + 1)
      }
    }

    searchHelper(currentDepth = 0)
  }
}
