package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.{Context, DSLResult}

private[dsl] trait Clause {
  def toQuery(context: Context): DSLResult
}
