package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.{Context, DSLResult}

private[spec] trait Clause {
  def toQuery(context: Context): DSLResult
}
