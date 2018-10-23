
sealed trait Option[+A]

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


trait Option[+A] {
  
  def map[B](f: A => B): Option[B] = this match {
    case Some(get: A) => Some(f(get))
    case None         => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get: A) => get
    case None         => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = 
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = 
    flatMap((get: A) => if (f(get)) Some(get) else None)

  def map2

  def sequence[A](a: List[Option[A]]): Option[List[A]]

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] 
  
}

