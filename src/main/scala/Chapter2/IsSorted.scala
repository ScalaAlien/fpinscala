package Chapter2

object IsSorted extends App {
  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case _ if as.tail.isEmpty => true
    case l if (ordered(l.head, l.tail.head)) => isSorted(l.tail, ordered)
    case _ => false
  }

  val nArray = Array(1, 2, 3)

  def ordered(a: Int, b: Int): Boolean = {
    a < b
  }

  println(isSorted(nArray, ordered))
}
