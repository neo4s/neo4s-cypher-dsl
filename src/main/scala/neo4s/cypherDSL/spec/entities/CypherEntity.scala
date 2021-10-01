package neo4s.cypherDSL.spec.entities

import neo4s.cypherDSL.spec.{Context, DSLResult}

private[cypherDSL] trait CypherEntity {
  def toQuery(context: Context = new Context()): DSLResult
  def toSetterQuery(context: Context = new Context()): DSLResult
}
