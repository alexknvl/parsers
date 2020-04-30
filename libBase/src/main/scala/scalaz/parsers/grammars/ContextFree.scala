package scalaz.parsers.grammars

import cats.Invariant
import cats.data.NonEmptyList
import cats.evidence.===
import scalaz.base.{/\, Inv, Iso, Separated, Separated1, \/}
import scalaz.parsers.symbols.SymbolSet

trait ContextFree[F[_]] extends ContextFree.ContextFreeDerived[F] with ContextFree.ContextFreeSyntax {
  type Symbol

  /** Accepts empty input. */
  def pure[A](a: A): F[A]

  /** Accepts any symbol. */
  def any: F[Symbol]

  /** Accepts a specific symbol. */
  def sym(symbol: Symbol): F[Unit]

  /** Language union. */
  def alt[A, B](A: F[A], B: F[B]): F[A \/ B]

  /** Sequential composition. */
  def zip[A, B](A: F[A], B: F[B]): F[A /\ B]

  def delay[A](A: => F[A]): F[A]

  def rule[A](name: String, f: F[A]): F[A]

  def anyOf(r: SymbolSet[Symbol]): F[r.Type]
}
object ContextFree {
  def apply[F[_]](implicit F: ContextFree[F] { }): ContextFree[F] { type Symbol = F.Symbol } = F

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
  }
  object Initial {
    final case class Map[S, A, B](A: Initial[S, A], iso: Inv[A, B]) extends Initial[S, B]
    final case class Pure[S, A](a: A)     extends Initial[S, A]
    final case class Any[S]()          extends Initial[S, S]
    final case class Sym[S](symbol: S) extends Initial[S, Unit]
    final case class Alt[S, A, B](A: Initial[S, A], B: Initial[S, B]) extends Initial[S, A \/ B]
    final case class Zip[S, A, B](A: Initial[S, A], B: Initial[S, B]) extends Initial[S, A /\ B]
    final class Delay[S, A](A: () => Initial[S, A]) extends Initial[S, A] {
      override def productElement(n: Int): scala.Any = A()
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

    implicit final class toLazyInitialParsingOps[S, A](A: => Initial[S, A]) {
      def | [B](B: => Initial[S, B]): Initial[S, A \/ B] =
        Alt(Delay(A), Delay(B))
    }
  }

  trait ContextFreeDerived[F[_]] { self: ContextFree[F] =>
    def unit: F[Unit] = pure(())

    def iso[A, B](fa: F[A])(f: Iso[A, B])(implicit I: Invariant[F]): F[B] =
      I.imap(fa)(f.to)(f.from)

    def opt[A](A: F[A])(implicit I: Invariant[F]): F[Option[A]] =
      iso(alt(unit, A))(Iso.optionEither.flip)

    def bool(A: F[Unit])(implicit I: Invariant[F]): F[Boolean] =
      iso(alt(unit, A))(Iso.bool)

    def zipL[A](A: F[A], B: F[Unit])(implicit I: Invariant[F]): F[A] =
      iso(zip(A, B))(Iso.Product.unitR[A].flip)

    def zipR[B](A: F[Unit], B: F[B])(implicit I: Invariant[F]): F[B] =
      iso(zip(A, B))(Iso.Product.unitL[B].flip)

    def many[A](A: F[A])(implicit I: Invariant[F]): F[List[A]] = {
      lazy val r: F[List[A]] =
        iso(alt(unit, zip(A, delay(r))))(Iso.list)
      r
    }
    def many1[A](A: F[A])(implicit I: Invariant[F]): F[NonEmptyList[A]] =
      iso(zip(A, many(A)))(Iso.nonEmptyList)

    def sepBy1[S, A](S: F[S], A: F[A])(implicit I: Invariant[F]): F[Separated1[S, A]] = {
      lazy val r: F[Separated1[S, A]] =
        iso(zip(A, opt(zip(S, delay(r)))))(Separated1.iso)
      r
    }

    def sepBy[S, A](S: F[S], A: F[A])(implicit I: Invariant[F]): F[Separated[S, A]] =
      iso(opt(sepBy1(S, A)))(Separated.iso)
  }

  trait ContextFreeSyntax {
    implicit def toParsingOps[F[_], A](A: F[A]): ToParsingOps[F, A] = new ToParsingOps(A)
    implicit def toLazyParsingOps[F[_], A](A: => F[A]): ToLazyParsingOps[F, A] = new ToLazyParsingOps(A)

    implicit def toInvParsingOps1[F[_], A](A: F[A]) = new ToInvParsingOps1(A)
    implicit def toInvParsingOps2[F[_], A, B](A: F[A /\ B]) = new ToInvParsingOps2(A)
    implicit def toInvParsingOps3_1[F[_], A, B, C](A: F[(A /\ B) /\ C]) = new ToInvParsingOps3_1(A)
  }
  object syntax extends ContextFreeSyntax

  final class ToParsingOps[F[_], A](val A: F[A]) extends AnyVal {
    type PF = ContextFree[F]

    def @: (name: String)(implicit F: PF): F[A] = F.rule(name, A)

    def ? (implicit F: PF, I: Invariant[F]): F[Option[A]] = F.opt(A)
    def ?? (implicit F: PF, ev: A === Unit, I: Invariant[F]): F[Boolean] = F.bool(ev.substitute(A))

    def ~ [B](B: F[B])(implicit F: PF, I: Invariant[F]): F[(A, B)] = F.zip(A, B)
    def ~> [B](B: F[B])(implicit F: PF, ev: A === Unit, I: Invariant[F]): F[B] = F.zipR(ev.substitute(A), B)
    def <~ (B: F[Unit])(implicit F: PF, I: Invariant[F]): F[A] = F.zipL(A, B)

    def * (implicit F: PF, I: Invariant[F]): F[List[A]] = F.many(A)
    def + (implicit F: PF, I: Invariant[F]): F[NonEmptyList[A]] = F.many1(A)

    def sepBy [S](S: F[S])(implicit F: PF, I: Invariant[F]): F[Separated[S, A]] = F.sepBy(S, A)
    def sepBy1[S](S: F[S])(implicit F: PF, I: Invariant[F]): F[Separated1[S, A]] = F.sepBy1(S, A)
  }

  final class ToLazyParsingOps[F[_], A](A: => F[A]) {
    def | [B](B: => F[B])(implicit F: ContextFree[F]): F[A \/ B] =
      F.alt(F.delay(A), F.delay(B))
  }

  final class ToInvParsingOps1[F[_], A](val A: F[A]) {
    def ^^[Z](f: Inv[A, Z])(implicit F: ContextFree[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
    def ^^[Z](f: Iso[A, Z])(implicit F: ContextFree[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
  }
  final class ToInvParsingOps2[F[_], A, B](val A: F[A /\ B]) {
    def ^^[Z](f: Inv[(A, B), Z])(implicit F: ContextFree[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
    def ^^[Z](f: Iso[(A, B), Z])(implicit F: ContextFree[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
  }
  final class ToInvParsingOps3_1[F[_], A, B, C](val A: F[(A /\ B) /\ C]) {
    def ^^[Z](f: Inv[(A, B, C), Z])(implicit F: ContextFree[F], FF: Invariant[F]): F[Z] =
      FF.imap(F.iso(A)(Iso.tupleIso3_1))(f.to)(f.from)
    def ^^[Z](f: Iso[(A, B, C), Z])(implicit F: ContextFree[F], FF: Invariant[F]): F[Z] =
      FF.imap(F.iso(A)(Iso.tupleIso3_1))(f.to)(f.from)
  }
}
