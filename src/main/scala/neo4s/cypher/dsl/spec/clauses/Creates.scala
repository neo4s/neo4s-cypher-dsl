package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.entities.{Node, NodeType}
import neo4s.cypher.dsl.spec.{Context, DSLResult, Path, PathLink, QueryProvider}
import shapeless.{HList, HNil}
import shapeless.ops.hlist.ToTraversable

private[spec] class Creates(path: Path) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = path.toQuery(context)
    result.copy(query = s"CREATE ${result.query}")
  }
}

private[spec] object Creates {
  def apply[T <: Product, TH <: HList](element: Node[T, TH])(
    implicit i0: ToTraversable.Aux[TH, List, Symbol]): Creates = {
    val path = new Path(PathLink(None, element, None))
    new Creates(path)
  }
  def apply(element: NodeType): Creates = {
    val path = new Path(PathLink(None, element, None))
    new Creates(path)
  }

  def apply[T <: Product, TH <: HList](element: T)(implicit queryProvider: QueryProvider[T],
                                                   i0: ToTraversable.Aux[TH, List, Symbol]): Creates = {
    val path = new Path(PathLink(None, Node(element, HNil), None))
    new Creates(path)
  }

  def apply(path: Path) = new Creates(path)
}
