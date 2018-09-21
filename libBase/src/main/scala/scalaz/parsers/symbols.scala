package scalaz.parsers

import cats.Order
import cats.syntax.order._
import cats.instances.char._
import scalaz.base._

object symbols {
  sealed abstract class SymbolSet[P] protected () {
    type Type <: P

    def isValid: P => Boolean

    def unapply(ch: P): Option[Type] =
      if (isValid(ch)) Some(ch.asInstanceOf[Type]) else None

    def prism : Prism[P, Type] =
      Prism.unsafe(unapply, t => t)

    def intervals(implicit E: Enumerable[P]): List[(P, P)]
  }
  object SymbolSet {
    final case class Range[P: Order](ch1: P, ch2: P) extends SymbolSet[P] {
      val start: P = ch1 min ch2
      val end: P   = ch1 max ch2

      def isValid = c => start <= c && c <= end

      def intervals(implicit E: Enumerable[P]) = List(start /\ end)
    }

    final case class Subset[P](sup: SymbolSet[P])(pred: P => Boolean) extends SymbolSet[P] {
      def bounds: (P, P) = sup match {
        case r@Range(_, _) => (r.start, r.end)
        case r@Subset(_)   => r.bounds
      }

      def isValid = x => sup.isValid(x) && pred(x)

      def intervals(implicit E: Enumerable[P]): List[(P, P)] = {
        val (l, u) = bounds
        type State = List[P /\ P] /\ Option[P /\ P]
        E.range(l, u).foldLeft(Nil /\ None : State) {
          case (l /\ None, p) =>
            if (isValid(p)) l /\ Some(p /\ p)
            else l /\ None
          case (l /\ Some(p1 /\ p2), p3) =>
            if (isValid(p3)) l /\ Some(p1 /\ p3)
            else (p1 /\ p2 :: l) /\ None
        } match {
          case l /\ None    => l.reverse
          case l /\ Some(p) => (p :: l).reverse
        }
      }
    }

    val Ascii: SymbolSet[Char] = Range('\u0000', '\u007F')
    type Ascii = Ascii.Type
  }

}
