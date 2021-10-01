package neo4s.cypher.dsl.spec

import neo4s.cypher.dsl.spec.entities.{NodeType, RelationType}

import scala.reflect.runtime.universe.{weakTypeOf, WeakTypeTag}

package object syntax {
  val patterns: Patterns.type           = Patterns
  def any[T <: Product: WeakTypeTag]    = NodeType(weakTypeOf[T])
  def anyRel[T <: Product: WeakTypeTag] = RelationType(weakTypeOf[T])
  case object anyLength
}
