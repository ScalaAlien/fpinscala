package Chapter8

import Chapter5.Stream._
import Chapter6.State._

object Property extends App {

  case class SGen[+A](forSize: Int => Gen[A])

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    def &&(p: Prop) = Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
      }
    }

    def ||(p: Prop): Prop = Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
    }

    def tag(msg: String): Prop = Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max,n,rng) =>
        val casesPerSize = (n - 1) / max + 1
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }

//    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
//      randomStream(as)(rng)
//        .zip(Stream.from(0))
//        .take(n)
//        .map {
//          case (a, i) =>
//            try {
//              if (f(a)) Passed else Falsified(a.toString, i)
//            } catch {
//              case e: Exception => Falsified(buildMsg(a, e), i)
//            }
//        }
//        .find(_.isFalsified)
//        .getOrElse(Passed)
//    }

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  case class Gen[+A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap (n => this.listOfN(n))

    def unSized: SGen[A] = SGen(_ => this)

    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g listOfN)
  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): State[RNG, Int] =
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

    def unit[A](a: => A): Gen[A] = Gen(Chapter6.State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
      Gen(State(RNG.double).flatMap(d =>
        if (d < g1Threshold) g1._1.sample else g2._1.sample))
    }
  }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
    def isFalsified = true
  }

}
