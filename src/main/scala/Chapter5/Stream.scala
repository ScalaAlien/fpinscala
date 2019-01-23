package Chapter5
object Stream extends App {

  import Stream._

  trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      this match {
        case Cons(h, t) =>
          f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case _ => Nil
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

    //    def takeWhile_1(p: A => Boolean): Stream[A] =
    //      foldRight(empty)((a, b) =>
    //        if (p(a)) cons(a,b)
    //        else empty)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    //def headOption: Option[A] = foldRight(None)((a, _) => Some(a))

    // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
    // writing your own function signatures.

    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (f(a)) (cons(a, b)) else b)

    def append[B >: A](as: => Stream[B]): Stream[B] =
      foldRight(as)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => f(a) append b)

    def constant[A](a: A): Stream[A] = {
      cons(a, constant(a))
    }

    def from(n: Int): Stream[Int] = {
      cons(n, from(n + 1))
    }

    //    val fibs: Stream[Int] = {
    //      def loop(a: Int, b: Int): Stream[Int] = {
    //        cons(a, loop(b, a + b))
    //      }
    //
    //      loop(0, 1)
    //    }



    //    val fibs_1 = unfold((0, 1)) { case (h, t) => Some((h, (t, h + t))) }

    def from_1(n: Int): Stream[Int] = unfold(n)((n) => Some(n, n + 1))

    def constant_1[A](a: A): Stream[A] = {
      unfold(a) { case _ => Some(a, a) }
    }

    //    val ones_1: Stream[Int] = unfold(1) { case _ => Some(1, 1) }

    def map_1[B](f: A => B): Stream[B] = {
      unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    }

    def take_1(n: Int): Stream[A] = {
      unfold(this) {
        case Cons(h, t) if (n > 0) => Some(h(), t())
        case _ => None
      }
    }

    def takeWhile_2(p: A => Boolean): Stream[A] = unfold(this) { case Cons(h, t) if p(h()) => Some((h(), t())) case _ => None }

    def zip[B](s2: Stream[B]): Stream[(A,B)] =
      zipWith(s2)((_,_))

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
      case (Cons(h, t), Empty) => Some(((Some(h()), Option.empty[B]), (t(), empty[B])))
      case (Empty, Cons(h, t)) => Some(((Option.empty[A], Some(h())), (empty[A], t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    }

    def startsWith[A](s: Stream[A]): Boolean =
      zipWith(s)((_, _)) forAll {
        case (h, h2) => h == h2
      }

    def tails: Stream[Stream[A]] = {
      unfold(this) { case Cons(_, t) => Some(this, t()) case _ => None }
    }

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a, p0) => {
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      })._2

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    def from(n: Int): Stream[Int] = {
      cons(n, from(n + 1))
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }

}
