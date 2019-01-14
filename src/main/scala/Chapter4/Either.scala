package Chapter4

object Either extends App {

  sealed trait Either[+E, +A] {
    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch {
        case e: Exception => Left(e)
      }

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => b
      case Left(e) => Left(e)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Right(a) => b map (f(a, _))
      case Left(e) => Left(e)
    }

    def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
    }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case h :: t => h.flatMap(x => sequence(t) map (x :: _))
    }

  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

}
