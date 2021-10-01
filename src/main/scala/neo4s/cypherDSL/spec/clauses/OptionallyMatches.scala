package neo4s.cypherDSL.spec.clauses

import neo4s.cypherDSL.spec.entities.{Node, NodeType}
import neo4s.cypherDSL.spec.{Context, DSLResult, Path, PathLink, QueryProvider}
import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.ToTraversable

private[cypherDSL] class OptionallyMatches(path: Path) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = path.toQuery(context)
    result.copy(query = s"OPTIONAL MATCH ${result.query}")
  }
}
private[cypherDSL] object OptionallyMatches {
  def apply[T <: Product, TH <: HList](element: Node[T, TH])(
      implicit i0: ToTraversable.Aux[TH, List, Symbol]): OptionallyMatches = {
    val path = new Path(PathLink(None, element, None))
    new OptionallyMatches(path)
  }
  def apply[T <: Product, TH <: HList](element: T)(implicit queryProvider: QueryProvider[T],
                                                   i0: ToTraversable.Aux[TH, List, Symbol]): OptionallyMatches = {
    val path = new Path(PathLink(None, Node(element, HNil), None))
    new OptionallyMatches(path)
  }
  def apply(element: NodeType): OptionallyMatches = {
    val path = new Path(PathLink(None, element, None))
    new OptionallyMatches(path)
  }
  def apply(path: Path) = new OptionallyMatches(path)
}
