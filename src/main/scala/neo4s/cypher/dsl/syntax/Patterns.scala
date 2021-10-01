package neo4s.cypher.dsl.syntax

import neo4s.cypher.dsl.entities._
import neo4s.cypher.dsl.{Path, PathLink, QueryProvider}
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record.Selector
import shapeless.{HList, HNil, LabelledGeneric, Witness}

import scala.annotation.implicitNotFound

private[dsl] object Patterns extends SyntaxSupport {

  implicit class EnrichedNode[T <: Product, TH <: HList](element: Node[T, TH]) {
    def --[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def --(rel: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def -->(rel: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def <--(rel: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def -|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val rels = Relationship(rel.element, rel.properties)
      new Path(PathLink(None, element, Some("-")), PathLink(None, rels, None))
    }

    def -|[U <: Product, UH <: HList](rel: RelationType) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, rel, None))

    def <-|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|(rel: RelationType) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, rel, None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val rels = Relationship(rel.element, rel.properties)
      new Path(PathLink(None, element, Some("<-")), PathLink(None, rels, None))
    }

    def -|*(range: Range)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(range.start, range.end))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(length: Int)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(length))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(x:anyLength.type)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation())
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, length: Int)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, range: Range)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         qpT: QueryProvider[T],
                                                         i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], length: Int)(implicit qpU: QueryProvider[U],
                                                                      qpT: QueryProvider[T],
                                                                      i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], range: Range)(implicit qpU: QueryProvider[U],
                                                                       qpT: QueryProvider[T],
                                                                       i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def DESC: Descending = Descending(element)
  }
  implicit class RichNodeType(element: NodeType) {
    def --[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        i1: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def --[U <: Product, UH <: HList](rel: NodeType) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("-"), rel, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit i1: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def -->[U <: Product, UH <: HList](rel: NodeType) =
      new Path(PathLink(None, element, Some("-")), PathLink(Some("->"), rel, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit i1: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def <--[U <: Product, UH <: HList](rel: NodeType) =
      new Path(PathLink(None, element, Some("<-")), PathLink(Some("-"), rel, None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def -|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, Relationship(rel.element, rel.properties), None))

    def -|[U <: Product, UH <: HList](rel: RelationType) =
      new Path(PathLink(None, element, Some("-")), PathLink(None, rel, None))

    def <-|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, Relationship(rel.element, rel.properties), None))

    def <-|[U <: Product, UH <: HList](rel: RelationType) =
      new Path(PathLink(None, element, Some("<-")), PathLink(None, rel, None))

    def -|*(range: Range) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(range.start, range.end))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], range: Range)(implicit qpU: QueryProvider[U],
                                                                       i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product](rel: U, range: Range)(implicit qpU: QueryProvider[U]) = {
      val variableLengthRelationship =
        Relationship(rel, HNil, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, range: Range) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(length: Int) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(length))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, length: Int) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, length: Int)(implicit qpU: QueryProvider[U],
                                                            i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel, HNil, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], length: Int)(implicit qpU: QueryProvider[U],
                                                                      i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(x:anyLength.type) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation())
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def -|*[U <: Product](rel: U)(implicit qpU: QueryProvider[U]) = {
      val variableLengthRelationship =
        Relationship(rel, HNil, Option(VariableLengthRelation()))
      new Path(PathLink(None, element, Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def DESC: Descending = Descending(element)
  }
  implicit class RichProduct[T <: Product, TH <: HList](element: T) {
    def --[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("-"), Node(rel, HNil), None))

    def --[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("-"), rel, None))

    def --(node: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("-"), node, None))

    def -->[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("->"), Node(rel, HNil), None))

    def -->[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("->"), rel, None))

    def -->(node: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(Some("->"), node, None))

    def <--[U <: Product, UH <: HList](rel: U)(implicit qpT: QueryProvider[T], qpU: QueryProvider[U]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(Some("-"), Node(rel, HNil), None))

    def <--[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(Some("-"), rel, None))

    def <--(node: NodeType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(Some("-"), node, None))

    def <-|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         qpT: QueryProvider[T],
                                                         i0: ToTraversable.Aux[UH, List, Symbol]) = {
      val rels = Relationship(rel.element, rel.properties)
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, rels, None))
    }
    def <-|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, Relationship(rel, HNil), None))

    def <-|(rel: RelationType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("<-")), PathLink(None, rel, None))

    def -|[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, Relationship(rel, HNil), None))

    def -|(rel: RelationType)(implicit qpT: QueryProvider[T]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, rel, None))

    def -|[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                        qpT: QueryProvider[T],
                                                        i0: ToTraversable.Aux[UH, List, Symbol]) =
      new Path(PathLink(None, Node(element, HNil), Some("-")),
               PathLink(None, Relationship(rel.element, rel.properties), None))

    def -|*(range: Range)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(range.start, range.end))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(length: Int)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation(length))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(x:anyLength.type)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship = VariableLengthRelationship(VariableLengthRelation())
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def -|*[U <: Product, UH <: HList](rel: U)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation()))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, length: Int)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: U, range: Range)(implicit qpU: QueryProvider[U], qpT: QueryProvider[T]) = {
      val variableLengthRelationship = Relationship(rel, HNil, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH])(implicit qpU: QueryProvider[U],
                                                         qpT: QueryProvider[T],
                                                         i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation()))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], length: Int)(implicit qpU: QueryProvider[U],
                                                                      qpT: QueryProvider[T],
                                                                      i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship = Relationship(rel.element, rel.properties, Option(VariableLengthRelation(length)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*[U <: Product, UH <: HList](rel: Node[U, UH], range: Range)(implicit qpU: QueryProvider[U],
                                                                       qpT: QueryProvider[T],
                                                                       i1: ToTraversable.Aux[UH, List, Symbol]) = {
      val variableLengthRelationship =
        Relationship(rel.element, rel.properties, Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, range: Range)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(range.start, range.end)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType, length: Int)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation(length)))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }
    def -|*(rel: RelationType)(implicit qpT: QueryProvider[T]) = {
      val variableLengthRelationship =
        rel.copy(variableLengthRelation = Option(VariableLengthRelation()))
      new Path(PathLink(None, Node(element, HNil), Some("-")), PathLink(None, variableLengthRelationship, None))
    }

    def DESC: Descending = Descending(element)
  }
}
