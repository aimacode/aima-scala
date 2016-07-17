package aima.core.search.uninformed

import aima.core.search.{Problem, ProblemSearch}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * @author Shawn Garner
  */
trait IterativeDeepeningSearch extends ProblemSearch {
  def depthLimitedTreeSearch: DepthLimitedTreeSearch

  def search(problem: Problem): Try[DLSResult] = {
    @tailrec def searchHelper(currentDepth: Int): Try[DLSResult] = {
      val result = depthLimitedTreeSearch.search(problem, currentDepth)

      result match {
        case Success(Solution(_)) | Failure(_) => result
        case _ if currentDepth == Int.MaxValue => Failure[DLSResult](new Exception("Depth has reached Int.MaxValue"))
        case _                                 => searchHelper(currentDepth + 1)
      }
    }

    searchHelper(currentDepth = 0)
  }
}
