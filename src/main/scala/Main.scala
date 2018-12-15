import Chapter3._

object Main extends App {

  //出力の想定
  // List(List(1, 2)
  // List(1, 2)
  // List(1)
  // List(2)
  // List()

  def flatten[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ => {
      println(l.head)
      flatten(l.tail)
    }
  }

  println(flatten(List(List(List(1, 2)))))

}