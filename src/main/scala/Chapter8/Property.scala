package Chapter8

import Chapter6.State._
import Chapter8.Property.Prop.{FailedCase, SuccessCount}

object Property extends App {

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]

    //def &&(p: Prop): Prop = new Prop {
    //def check = Prop.this.check && p.check
    //}
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  case class Gen[A](sample: State[RNG, A])

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(Chapter6.State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]) = Gen(sequence(List.fill(n)(g.sample)))
}