package scalaz.base

import cats.Order
import cats.instances.char._

abstract class Enumerable[A](implicit A: Order[A]) {
  implicit val order: Order[A] = A
  def range(a: A, b: A): List[A]
}
object Enumerable {
  implicit val charEnum: Enumerable[Char] = new Enumerable[Char] {
    def range(a: Char, b: Char): List[Char] = (a to b).toList
  }
}
