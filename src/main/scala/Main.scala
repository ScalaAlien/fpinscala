object Main extends App {
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
    case h :: t => foldLeft(t,f(z,h))(f)
  }
}