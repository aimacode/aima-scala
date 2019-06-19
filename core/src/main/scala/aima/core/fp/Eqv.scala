package aima.core.fp

/**
  * @author Shawn Garner
  */
/**
  * @author Shawn Garner
  */
trait Eqv[A] {
  def eqv(a1: A, a2: A): Boolean
}

object Eqv {
  def apply[A: Eqv]: Eqv[A] = implicitly[Eqv[A]]

  object Implicits {
    final implicit class EqvOps[A: Eqv](a1: A) {
      val equiv = Eqv[A]
      import equiv.eqv
      def ===(a2: A): Boolean = eqv(a1, a2)
      def =!=(a2: A): Boolean = !eqv(a1, a2)
    }

    implicit val stringEq: Eqv[String] = new Eqv[String] {
      override def eqv(a1: String, a2: String): Boolean = a1 == a2
    }

  }
}
