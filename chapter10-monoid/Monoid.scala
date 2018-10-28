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

// Exercise 10.3
def endoMonoid[A] = Monoid[A => A] {
  def op(a1: A => A, a2: A => A) = a1 compose a2
  def zero = (a: A) => a
}

// Exercise 10.4
// TODO 0 After Reading Part 2


def concatenate[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.zero)(m.op)

// Exercise 10.5
def foldMap[A,B](as: List[A], m: Monoid[A])(f: A => B): B =
  as.foldLeft((b,a) => m.op(b, f(a)))

// Exercise 10.6
def foldLeft[A,B](as: List[A])(z: B)(f: (A, B) => B): B =
  foldMap(as, endoMonoid[B])(f.curried)(z)

def foldRight[A,B](as: List[A])(z: B)(f: A => B): B = ???

// Exercise 10.7
def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  if (v.length == 0) {
    m.zero
  } else if (v.length == 1) {
    f(v(0))
  } else {
    var (l, r) = v.splitAt(v.length/2)
    m.op(foldMapV(l)(f), foldMapV(r)(f))
  }
}



trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: (A,B) => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

// Exercise 10.12
object ListFoldable extends Foldable[List] {
  def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)
  def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)
  def foldMap[A,B](as: List[A])(f: (A,B) => B)(mb: Monoid[B]): B =
    foldLeft(as)(m.zero)((b,a) => m.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)
  def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)
  def foldMap[A,B](as: IndexedSeq[A])(f: (A,B) => B)(mb: Monoid[B]): B =
    foldMapV(as, m)(f)
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)
  def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)
  // Todo - After learning Streams
  def foldMap[A,B](as: Stream[A])(f: (A,B) => B)(mb: Monoid[B]): B = ???
}

object OptionFoldable extends Foldable[Option] {
  def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as match {
    case None    => z
    case Some(a) => f(a, z)
  }
  def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as match {
    case None    => z
    case Some(a) => f(z, a)
  }
  def foldMap[A,B](as: Option[A])(f: (A,B) => B)(mb: Monoid[B]): B = as match {
    case None    => mb.zero
    case Some(a) => f(a)
  }
}
