package scalaz.parsers.backend

import _root_.parseback.Parser
import cats.Functor
import scalaz.base._
import scalaz.parsers.parsers.ContextFree
import scalaz.parsers.symbols.SymbolSet

object parseback {
  sealed trait PBWrapper[A] extends Product with Serializable {
    def force: Parser[A] = this match {
      case PBWrapper.Value(p) => p
      case PBWrapper.Delay(p) => p()
    }
  }
  object PBWrapper {
    final case class Value[A](value: Parser[A]) extends PBWrapper[A]
    final case class Delay[A](value: () => Parser[A]) extends PBWrapper[A]

    implicit val functor: Functor[PBWrapper] = new Functor[PBWrapper] {
      override def map[A, B](fa: PBWrapper[A])(f: A => B): PBWrapper[B] = fa match {
        case Value(x) => Value(x.map(f))
        case Delay(x) => Delay(() => x().map(f))
      }
    }
  }

  implicit val parsebackParsing: ContextFree[PBWrapper] { type Symbol = Char } =
    new ContextFree[PBWrapper] {
      type Symbol = Char
      type F[A] = PBWrapper[A]

      type Token[_] <: AnyRef

      def pure[A](a: A): F[A] = PBWrapper.Value(Parser.Epsilon(a))

      def any: F[Symbol] =
        PBWrapper.Value(Parser.Regex(".".r).map(_.head))

      def sym(symbol: Symbol): F[Unit] =
        PBWrapper.Value(Parser.Literal(symbol.toString).map(_ => *))

      /** Language union. */
      def alt[A, B](A: F[A], B: F[B]): F[A \/ B] =
        PBWrapper.Value(Parser.Union(() => A.force.map(-\/(_)), () => B.force.map(\/-(_))))

      /** Sequential composition. */
      def zip[A, B](A: F[A], B: F[B]): F[A /\ B] = (A, B) match {
        case (PBWrapper.Value(pa), PBWrapper.Value(pb)) => PBWrapper.Value(Parser.Sequence(pa, None, pb))
        case (pa, pb) => PBWrapper.Delay(() => Parser.Sequence(pa.force, None, pb.force))
      }

      def delay[A](A: => F[A]): F[A] =
        PBWrapper.Delay(() => A.force)

      def rule[A](name: String, f: F[A]): F[A] = f

      // THIS IS HORRIBLE, but technically correct.
      def anyOf(r: SymbolSet[Symbol]): F[r.Type] =
        r.enumerate.foldLeft(None : Option[Parser[r.Type]]) {
          case (None, s) => Some(Parser.Literal(s.toString).map(_ => s))
          case (Some(p), s) => Some(Parser.Union(() => p, () => Parser.Literal(s.toString).map(_ => s)))
        } match {
          case Some(p) => PBWrapper.Value(p)
          case None => PBWrapper.Value(Parser.Failure(Nil))
        }
    }
}
