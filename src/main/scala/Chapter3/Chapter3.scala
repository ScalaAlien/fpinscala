object Chapter3 extends App {
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

  def append[A](l: List[A], t: A): List[A] = {
    foldRight(l, List(t))(_ :: _)
  }

  println(append(List(1, 2, 3), 1))

  def append2[A](l: List[A], t: A): List[A] = {
    foldLeft(reverse(l), List(t))((x, xs) => xs :: x)
  }

  println(append2(List(1, 2, 3), 1))

}