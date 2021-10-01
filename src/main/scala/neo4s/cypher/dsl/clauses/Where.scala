package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl._

private[dsl] trait SupportsWhere {
  def toWhereQuery(context: Context): DSLResult
}

private[dsl] class Where(path: SupportsWhere, negated: Boolean) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = path.toWhereQuery(context)
    if (negated) result.copy(query = s"WHERE NOT ${result.query}")
    else result.copy(query = s"WHERE ${result.query}")
  }
}

private[dsl] object Where {
  def apply(supportsWhere: SupportsWhere, negated: Boolean) = new Where(supportsWhere, negated)
}

