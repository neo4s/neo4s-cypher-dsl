package neo4s.cypher.dsl.entities

import neo4s.cypher.dsl.operators.Operator
import neo4s.cypher.dsl.{Context, DSLResult}

private[cypher] trait CypherEntity {
  def toQuery(context: Context = new Context()): DSLResult
  def toWhereQuery(context: Context = new Context(), operator: Option[Operator] = None): DSLResult
  def toSetterQuery(context: Context = new Context(), includeNodeId: Boolean): DSLResult
}
