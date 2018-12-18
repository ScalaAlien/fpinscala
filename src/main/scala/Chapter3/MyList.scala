object Chapter3 extends App {

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: t => Nil ::: t
  }

  println(tail(List(1, 2, 3)))

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case _ if (n > 0) => drop(tail(l), n - 1)
    case _ => l
  }

  println(drop(List(1, 2, 3), 2))

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case h :: t if (f(h)) => dropWhile(t)(f)
    case _ => l
  }

  println(dropWhile(List(1, 2, 3))(_ < 2))


  def init[A](l: List[A]): List[A] = l.tail match {
    case Nil => Nil
    case _ => l.head :: init(l.tail)
  }

  println(init(List(1, 2, 3)))

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0l
    case h :: t => h * product(t)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case h :: t => f(h, foldRight(t, z)(f))
  }

  println(foldRight(List(1, 2, 3), Nil: List[Int])(_ :: _))

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, x) => x + 1)
  }

  println(length(List(1, 2, 3)))

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case h :: t => foldLeft(t, f(z, h))(f)
  }

  println(foldLeft(List(1, 2, 3), 10)(_ - _))

  def sum(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  println(sum(List(1, 2, 3)))

  def myProduct(l: List[Int]): Int = {
    foldLeft(l, 1)(_ * _)
  }

  println(myProduct(List(1, 2, 3)))

  def myLength[A](l: List[A]): Int = {
    foldLeft(l, 0)((x, _) => x + 1)
  }

  println(myLength(List(1, 2, 3)))

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((t, h) => h :: t)
  }

  println(reverse(List(1, 2, 3)))

  def append[A](l: List[A], bs: List[A]): List[A] = {
    foldRight(l, bs)(_ :: _)
  }

  println(append(List(1, 2, 3), List(1)))

  def append2[A](l: List[A], bs: List[A]): List[A] = {
    foldLeft(reverse(l), bs)((x, xs) => xs :: x)
  }

  println(append2(List(1, 2, 3), List(1)))

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  println(concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8))))

  def incrementEach(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])(_ + 1 :: _)
  }

  println(incrementEach(List(1, 2, 3)))

  def toStringEach(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])(_.toString :: _)
  }

  println(toStringEach(List(1, 2, 3)))

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case (h :: t) => f(h) :: map(t)(f)
  }

  println(map(List(1, 2, 3))(x => x.toDouble))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case (h :: t) if (f(h)) => (h :: filter(t)(f))
    case (_ :: t) => filter(t)(f)
  }

  println(filter(List(1, 2, 3))(_ % 2 == 0))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  println(flatMap(List(1, 2, 3))(i => List(i, i)))

  def flatFilter[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => if (f(x)) List(x) else Nil)
  }

  println(flatFilter(List(List(1, 2), List(3, 4)))(_ == List(1, 2)))

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as,bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => f(h1,h2) :: zipWith(t1,t2)(f)
  }

  println("this")
  println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _))

  def isSubsequenceStartingWithSameHead[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case h :: t if (h == sup.head) => (isSubsequenceStartingWithSameHead(sup.tail, t))
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if isSubsequenceStartingWithSameHead(sup, sub) => true
    case _ :: t => hasSubsequence(t, sub)
  }

  println(hasSubsequence(List(0, 0, 1, 2, 1, 2, 3, 4, 0, 0), List(1, 2, 3, 4)))

}