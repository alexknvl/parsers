package scalaz.parsers

import cats.effect.IO

object refeq {
  sealed class StableName[A]
  object StableName {
    private[this] final class Instance[A <: AnyRef](val value: A) extends StableName[A] {
      override def clone(): AnyRef = this

      override def equals(obj: Any): Boolean = obj match {
        case that: Instance[_] => that.value eq this.value
        case _ => false
      }

      override def hashCode(): Int =
        System.identityHashCode(value)
    }

    def apply[A <: AnyRef](value: A): IO[StableName[A]] =
      IO { new Instance[A](value) }
  }
}
