package neo4s.cypher.dsl.syntax

import neo4s.cypher.dsl.{Condition, Predicate}
import neo4s.cypher.dsl.entities._
import shapeless.HList

private[dsl] object Wherewithal {

  implicit class NodeWhereSupport[T <: Product, TH <: HList](element: Node[T, TH]) {
    def ===(value: Any) = new Condition(Predicate(element, "=", value))

    def >(value: Any) = new Condition(Predicate(element, ">", value))
    def >=(value: Any) = new Condition(Predicate(element, ">=", value))

    def <(value: Any) = new Condition(Predicate(element, "<", value))
    def <=(value: Any) = new Condition(Predicate(element, "<=", value))

    def =~(value: String) = new Condition(Predicate(element, "=~", value))

    def STARTS_WITH(value: String) = new Condition(Predicate(element, "STARTS WITH", value))
    def ENDS_WITH(value: String) = new Condition(Predicate(element, "ENDS WITH", value))
    def CONTAINS(value: String) = new Condition(Predicate(element, "CONTAINS", value))

    def IN(value: List[Any]) = new Condition(Predicate(element, "IN", value))

    /* TODO
    def IS_NULL = Predicate(element, "IS NULL", None)
    def EXISTS =

     */

  }
}
