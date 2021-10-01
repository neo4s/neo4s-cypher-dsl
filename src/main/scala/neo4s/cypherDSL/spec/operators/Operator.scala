package neo4s.cypherDSL.spec.operators

import neo4s.cypherDSL.spec.{Context, DSLResult}

private[spec] trait Operator {
  def toQuery(context: Context): DSLResult
}
