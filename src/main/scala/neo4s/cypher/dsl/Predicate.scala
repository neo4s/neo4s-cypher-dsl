package neo4s.cypher.dsl

import neo4s.cypher.dsl.clauses.SupportsWhere
import neo4s.cypher.dsl.entities.CypherEntity
import neo4s.cypher.dsl.operators.Operator

private[cypher] case class Predicate(left: CypherEntity, operator: String, right: Any) extends SupportsWhere {
  def toWhereQuery(context: Context = new Context()): DSLResult = {
    left.toWhereQuery(context, Some(Operator(operator,right)))
  }
}
