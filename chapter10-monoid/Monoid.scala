trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

val stringMonoid = new Monoid[String] {
  def op(a1: String, a2: String): String = a1 + a2
  def zero = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  def op(a1: List[A], a2: List[A]) = a1 ++ a2
  def zero = Nil
}

// Exercise 10.1
val intAddition = new Monoid[Int] {
  def op(a1: Int, a2: Int): Int = a1 + a2
  def zero = 0
}

val intMultiplication = new Monoid[Int] {
  def op(a1: Int, a2: Int): Int = a1 * a2
  def zero = 1
}

val booleanOr = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
  def zero = false
}

val booleanAnd = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
  def zero = true
}

// Exercise 10.2
def optionMonoid[A] = new Monoid[Option[A]] {
  def op(a1: Option[A], a2: Option[A]): OptionA = a1 orElse a2
  def zero: None
}
