package scalaz.parsers.grammars

import cats.Invariant
import cats.data.NonEmptyList
import cats.evidence.===
import scalaz.base.{/\, Inv, Iso, Separated, Separated1, \/}
import scalaz.parsers.symbols.SymbolSet

trait ContextSensitive[F[_]] extends ContextFree[F] {
  type Token[_]
  def capture[A, B](fa: F[A])(t: Token[A] => F[B]): F[(A, B)]
  def insert[A](t: Token[A]): F[Unit]
}
object ContextSensitive {
  sealed trait Initial[S, A] extends Product with Serializable { A =>
    def ^^[B](f: Iso[A, B]): Initial[S, B] =
      Initial.Map(this, Inv(f.to, f.from))

    def iso[B](f: Iso[A, B]): Initial[S, B] =
      Initial.Map(this, Inv(f.to, f.from))

    def opt: Initial[S, Option[A]] =
      Initial.Alt(Initial.unit[S], A).iso(Iso.optionEither.flip)
    def ? : Initial[S, Option[A]] =
      A.opt

    def bool(implicit ev: A === Unit): Initial[S, Boolean] =
      Initial.Alt(Initial.unit[S], ev.substitute(A)).iso(Iso.bool)
    def ?? (implicit ev: A === Unit): Initial[S, Boolean] =
      bool

    def zip[B](B: Initial[S, B]): Initial[S, (A, B)] = Initial.Zip(A, B)

    def zipL(B: Initial[S, Unit]): Initial[S, A] =
      Initial.Zip(A, B).iso(Iso.Product.unitR[A].flip)

    def zipR[B](B: Initial[S, B])(implicit ev: A === Unit): Initial[S, B] =
      (ev.substitute(A) zip B).iso(Iso.Product.unitL[B].flip)

    def many: Initial[S, List[A]] = {
      lazy val r: Initial[S, List[A]] =
        Initial.Alt(Initial.unit[S], Initial.Zip[S, A, List[A]](A, Initial.Delay(r))).iso(Iso.list[A])
      r
    }
    def many1: Initial[S, NonEmptyList[A]] =
      Initial.Zip(A, A.many).iso(Iso.nonEmptyList)

    def * : Initial[S, List[A]] = A.many
    def + : Initial[S, NonEmptyList[A]] = A.many1

    def sepBy1[R](R: Initial[S, R]): Initial[S, Separated1[R, A]] = {
      lazy val r: Initial[S, Separated1[R, A]] =
        Initial.Zip(A, Initial.Zip[S, R, Separated1[R, A]](R, Initial.Delay(r)).opt)
          .iso(Separated1.iso[R, A])
      r
    }

    def sepBy[R](S: Initial[S, R]): Initial[S, Separated[R, A]] =
      A.sepBy1(S).opt.iso(Separated.iso[R, A])

    def @: (name: String): Initial[S, A] = Initial.Rule(name, A)

    def ~ [B](B: Initial[S, B]): Initial[S, (A, B)] = Initial.Zip(A, B)
    def ~> [B](B: Initial[S, B])(implicit ev: A === Unit): Initial[S, B] = ev.substitute(A).zipR(B)
    def <~ (B: Initial[S, Unit]): Initial[S, A] = A.zipL(B)

    def capture[B](t: Initial.Token[S, A] => Initial[S, B]): Initial[S, (A, B)] =
      Initial.Capture(this, t)

    def compile[F[_]](implicit F: ContextSensitive[F] { type Symbol = S }, FF: Invariant[F]): F[A] = (this match {
      case Initial.Map(a, iso) => FF.imap(a.compile[F])(iso.to)(iso.from)
      case Initial.Pure(a) => F.pure(a)
      case Initial.Any() => F.any.asInstanceOf[F[A]]
      case s: Initial.Sym[S] => F.sym(s.symbol).asInstanceOf[F[A]]
      case s: Initial.Alt[S, a, b] => F.alt(s.first.compile[F], s.second.compile[F]).asInstanceOf[F[A]]
      case s: Initial.Zip[S, a, b] => F.zip(s.first.compile[F], s.second.compile[F]).asInstanceOf[F[A]]
      case delay: Initial.Delay[S, A] => F.delay(delay.value().compile[F])
      case Initial.Rule(name, f) => F.rule(name, f.compile[F])
      case s: Initial.AnyOf[S, u] => F.anyOf(s.r).asInstanceOf[F[A]]
      case s: Initial.Capture[S, a, b] =>
        F.capture(s.p.compile[F])(t => s.f(t.asInstanceOf[Initial.Token[S, a]]).compile[F]).asInstanceOf[F[A]]
      case s: Initial.Insert[S, A] =>
        F.insert(s.t.asInstanceOf[F.Token[A]]).asInstanceOf[F[A]]
    })
  }
  object Initial {
    final case class Map[S, A, B](A: Initial[S, A], iso: Inv[A, B]) extends Initial[S, B]
    final case class Pure[S, A](a: A)     extends Initial[S, A]
    final case class Any[S]()          extends Initial[S, S]
    final case class Sym[S](symbol: S) extends Initial[S, Unit]
    final case class Alt[S, A, B](first: Initial[S, A], second: Initial[S, B]) extends Initial[S, A \/ B]
    final case class Zip[S, A, B](first: Initial[S, A], second: Initial[S, B]) extends Initial[S, A /\ B]
    final class Delay[S, A](val value: () => Initial[S, A]) extends Initial[S, A] {
      override def productElement(n: Int): scala.Any = value()
      override def productArity: Int = 1
      override def canEqual(that: scala.Any): Boolean = that match {
        case _: Initial[_, _] => true
        case _ => false
      }
    }
    object Delay {
      def apply[S, A](f: => Initial[S, A]): Initial[S, A] = new Delay[S, A](() => f)
    }
    final case class Rule[S, A](name: String, f: Initial[S, A]) extends Initial[S, A]
    final case class AnyOf[S, U <: S](r: SymbolSet[S] { type Type = U }) extends Initial[S, U]

    /** Accepts empty input. */
    def pure[S, A](a: A): Initial[S, A] = Pure(a)
    def unit[S]: Initial[S, Unit] = Pure(())
    def any[S]: Initial[S, S] = Any[S]()
    def sym[S](symbol: S): Initial[S, Unit] = Sym(symbol)
    def delay[S, A](A: => Initial[S, A]): Initial[S, A] = Delay(A)
    def anyOf[S](r: SymbolSet[S]): Initial[S, r.Type] =
      AnyOf[S, r.Type](r)

    type Token[_, _]
    final case class Capture[S, A, B](p: Initial[S, A], f: Token[S, A] => Initial[S, B]) extends Initial[S, (A, B)]
    final case class Insert[S, A](t: Token[S, A]) extends Initial[S, Unit]

    def insert[S](t: Token[S, _]): Initial[S, Unit] = Insert(t)

    implicit final class toLazyInitialParsingOps[S, A](A: => Initial[S, A]) {
      def | [B](B: => Initial[S, B]): Initial[S, A \/ B] =
        Alt(Delay(A), Delay(B))
    }
  }
}