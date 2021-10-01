package neo4s.cypher.dsl.spec.operators

import neo4s.cypher.dsl.spec.{Context, DSLResult}

private[spec] trait Operator {
  def toQuery(context: Context): DSLResult
}
