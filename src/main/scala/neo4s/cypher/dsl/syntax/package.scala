package neo4s.cypher.dsl

import neo4s.cypher.dsl.clauses.{Creates, Deletes, Limits, Matches, Merges, OptionallyMatches, OrdersBy, Returns, Sets, Skips, SupportsWhere, Where, With}
import neo4s.cypher.dsl.entities.{CypherEntity, Node, NodeType, RelationType}
import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeOf}

package object syntax {
  val patterns: Patterns.type           = Patterns
  val wherewithal: Wherewithal.type     = Wherewithal
  def cypher                            = Statement()

  def any[T <: Product: WeakTypeTag]    = NodeType(weakTypeOf[T])
  def anyRel[T <: Product: WeakTypeTag] = RelationType(weakTypeOf[T])
  def anyNode                           = NodeType(weakTypeOf[Any])
  def anyRelation                       = RelationType(weakTypeOf[Any])

  case object anyLength

  implicit class MatcherStatement(statement: Statement) {
    def MATCH[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }
    def MATCH(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }
    def MATCH[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                               i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }
    def MATCH(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Matches(element))
    }

    def WHERE(supportsWhere: SupportsWhere): Statement = {
      statement.copy(clauses = statement.clauses :+ Where(supportsWhere, false))
    }
    def WHERE_NOT(supportsWhere: SupportsWhere): Statement = {
      statement.copy(clauses = statement.clauses :+ Where(supportsWhere, true))
    }

    def OPTIONAL_MATCH[T <: Product](element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }
    def OPTIONAL_MATCH[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }
    def OPTIONAL_MATCH[T <: Product, TH <: HList](element: Node[T, TH])(
      implicit queryProvider: QueryProvider[T],
      i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }
    def OPTIONAL_MATCH(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ OptionallyMatches(element))
    }

    def CREATE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }
    def CREATE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }
    def CREATE[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                                i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }

    def CREATE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Creates(element))
    }

    def MERGE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }
    def MERGE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }
    def MERGE[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                               i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }

    def MERGE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Merges(element))
    }

    def DELETE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }
    def DELETE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }
    def DELETE(element: RelationType): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }
    def DELETE[T <: Product, TH <: HList](element: Node[T, TH])(implicit queryProvider: QueryProvider[T],
                                                                i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }
    def DELETE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = false))
    }

    def DETACH_DELETE[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }
    def DETACH_DELETE(element: NodeType): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }
    def DETACH_DELETE(element: RelationType): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }
    def DETACH_DELETE[T <: Product, TH <: HList](element: Node[T, TH])(
      implicit queryProvider: QueryProvider[T],
      i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }

    def DETACH_DELETE(element: Path): Statement = {
      statement.copy(clauses = statement.clauses :+ Deletes(element, detaches = true))
    }

    def SET[T <: Product, TH <: HList](element: T, setters: List[(CypherEntity, Any)])(
      implicit queryProvider: QueryProvider[T],
      i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(Sets.setCommand, element, setters))
    }

    def SET[T <: Product](setters: (Node[T, _], Any)*)(
      implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(Sets.setCommand, setters:_*))
    }

    def ON_CREATE_SET[T <: Product, TH <: HList](element: T, setters: List[(CypherEntity, Any)])(
      implicit queryProvider: QueryProvider[T],
      i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(Sets.onCreateSetCommand, element, setters))
    }

    def ON_CREATE_SET[T <: Product](setters: (Node[T, _], Any)*)(
      implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(Sets.onCreateSetCommand, setters:_*))
    }

    def ON_MATCH_SET[T <: Product, TH <: HList](element: T, setters: List[(CypherEntity, Any)])(
      implicit queryProvider: QueryProvider[T],
      i0: ToTraversable.Aux[TH, List, Symbol]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(Sets.onMatchSetCommand, element, setters))
    }

    def ON_MATCH_SET[T <: Product](setters: (Node[T, _], Any)*)(
      implicit queryProvider: QueryProvider[T]): Statement = {
      statement.copy(clauses = statement.clauses :+ Sets(Sets.onMatchSetCommand, setters:_*))
    }

    def SKIP(count: Int): Statement = {
      statement.copy(clauses = statement.clauses :+ Skips(count))
    }

    def LIMIT(count: Int): Statement = {
      statement.copy(clauses = statement.clauses :+ Limits(count))
    }

    def RETURN(elements: Product*): Statement = {
      statement.copy(clauses = statement.clauses :+ Returns(elements: _*))
    }

    def WITH[T <: Product](elements: T*): Statement = {
      statement.copy(clauses = statement.clauses :+ With(elements: _*))
    }

    def ORDER_BY[T <: Product](elements: T*): Statement = {
      statement.copy(clauses = statement.clauses :+ OrdersBy(elements: _*))
    }
  }

}
