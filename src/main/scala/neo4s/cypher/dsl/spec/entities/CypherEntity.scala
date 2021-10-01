package neo4s.cypher.dsl.spec.entities

import neo4s.cypher.dsl.spec.{Context, DSLResult}

private[dsl] trait CypherEntity {
  def toQuery(context: Context = new Context()): DSLResult
  def toSetterQuery(context: Context = new Context()): DSLResult
}
