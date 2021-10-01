package neo4s.cypher.dsl.operators

import neo4s.cypher.dsl.{Context, DSLResult}

private[dsl] trait UnaryOperator {
  def toQuery(context: Context): DSLResult
}
