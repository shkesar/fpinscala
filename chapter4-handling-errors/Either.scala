
sealed trair Either[+E, +A]

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B]

  def flatMap[EE :> E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE :> E, B :> A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE :> E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]

  def sequence

  def traverse
}
