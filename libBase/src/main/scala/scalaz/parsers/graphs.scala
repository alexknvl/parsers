package scalaz.parsers

object graphs {
  final case class Graph[E[_], U](root: U, nodes: Map[U, E[U]])
}
