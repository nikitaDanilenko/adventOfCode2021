package day15

import cats.syntax.order.*
import spire.algebra.{ AdditiveMonoid, Order }
import spire.syntax.additiveMonoid.*

sealed trait Tropical[+A]

object Tropical {
  case object Infinity extends Tropical[Nothing]
  case class Number[A](value: A) extends Tropical[A]

  implicit def tropicalOrder[A: Order]: Order[Tropical[A]] =
    (x, y) =>
      (x, y) match {
        case (Infinity, Infinity)   => 0
        case (x, Infinity)          => -1
        case (Infinity, y)          => 1
        case (Number(a), Number(b)) => Order[A].compare(a, b)
      }

  def minimum[A: Order](seq: Seq[A]): Tropical[A] =
    seq.foldLeft(Infinity: Tropical[A])((t, n) => t.min(Number(n)))

  implicit def tropicalAdditiveMonoid[A: AdditiveMonoid]: AdditiveMonoid[Tropical[A]] =
    new AdditiveMonoid[Tropical[A]] {
      override def zero: Tropical[A] = Number(AdditiveMonoid[A].zero)

      override def plus(a: Tropical[A], b: Tropical[A]): Tropical[A] =
        (a, b) match {
          case (Infinity, _)          => Infinity
          case (_, Infinity)          => Infinity
          case (Number(x), Number(y)) => Number(AdditiveMonoid[A].plus(x, y))
        }

    }

}
