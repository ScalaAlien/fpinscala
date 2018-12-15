package Chapter2

object Fib extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = n match {
      case 0 => b
      case 1 => a
      case _ => loop(n - 1, b, a + b)
    }
    loop(n, 0, 1)
  }
  print(fib(6))
}
