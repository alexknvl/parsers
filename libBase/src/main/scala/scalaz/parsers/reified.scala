package scalaz.parsers

import cats.data.StateT
import cats.effect.IO
import cats.{Functor, Traverse}
import scalaz.parsers.graphs.Graph
import scalaz.parsers.refeq.StableName

import scala.collection.immutable.HashMap

object reified {
  // see http://www.ittc.ku.edu/~andygill/papers/reifyGraph.pdf
  // and https://hackage.haskell.org/package/data-reify

  trait Recursive[T] {
    type Base[X]
    def project(t: T): Base[T]
  }
  object Recursive {
    type Aux[T, F[_]] = Recursive[T] { type Base[X] = F[X] }
    def apply[T](implicit T: Recursive[T]): Aux[T, T.Base] = T
  }

  trait Corecursive[T] {
    type Base[X]
    def embed(t: Base[T]): T
  }
  object Corecursive {
    type Aux[T, F[_]] = Corecursive[T] { type Base[X] = F[X] }
    def apply[T](implicit T: Corecursive[T]): Aux[T, T.Base] = T
  }

  final case class State[T, Base[_], U]
  (size: U,
   names: HashMap[StableName[T], U],
   graph: HashMap[U, Base[U]])

  def fromGraph[T, F[_], U](g: Graph[F, U])(implicit T: Corecursive.Aux[T, F], F: Functor[F]): T =
    fromGraph_(g, g.root)

  def fromGraph_[T, F[_], U](g: Graph[F, U], u: U)(implicit T: Corecursive.Aux[T, F], F: Functor[F]): T =
    g.nodes.get(g.root) match {
      case Some(node) => T.embed(F.map(node)(fromGraph_(g, _)))
    }

  def toGraph[T <: AnyRef, F[_]](t: T)(implicit T: Recursive.Aux[T, F], F: Traverse[F]): IO[Graph[F, BigInt]] =
    toGraph_(t).runS(State(0, HashMap.empty, HashMap.empty)).map(s => Graph(0, s.graph))

  def toGraph_[T <: AnyRef, F[_]](t: T)(implicit T: Recursive.Aux[T, F], F: Traverse[F]): StateT[IO, State[T, F, BigInt], BigInt] =
    for {
      name <- StateT.liftF[IO, State[T, F, BigInt], StableName[T]](StableName(t))
      oldState <- StateT.get[IO, State[T, F, BigInt]]

      r <- oldState.names.get(name) match {
        case Some(key) => StateT.pure[IO, State[T, F, BigInt], BigInt](key)
        case None =>
          val key = oldState.size
          for {
            _ <- StateT.set[IO, State[T, F, BigInt]](oldState.copy(
              size = oldState.size + 1,
              names = oldState.names.updated(name, key)
            ))

            entry <- F.traverse(T.project(t))(toGraph_[T, F](_))

            _ <- StateT.modify[IO, State[T, F, BigInt]](s => s.copy(
              graph = s.graph.updated(key, entry)
            ))
          } yield oldState.size
      }
    } yield r
}
