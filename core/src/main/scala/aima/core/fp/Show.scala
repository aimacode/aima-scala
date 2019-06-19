package aima.core.fp

/**
  * @author Shawn Garner
  */
trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A: Show]: Show[A] = implicitly[Show[A]]

  object Implicits {
    final implicit class ShowOps[A: Show](a: A) {
      def show: String = Show[A].show(a)
    }
  }
}
