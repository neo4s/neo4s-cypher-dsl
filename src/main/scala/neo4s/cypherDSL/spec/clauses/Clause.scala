package neo4s.cypherDSL.spec.clauses

import neo4s.cypherDSL.spec.{Context, DSLResult}

private[spec] trait Clause {
  def toQuery(context: Context): DSLResult
}
