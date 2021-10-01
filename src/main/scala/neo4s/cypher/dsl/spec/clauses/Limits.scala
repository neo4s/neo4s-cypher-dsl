package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.{Context, DSLResult}

private[dsl] class Limits(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = DSLResult(s"LIMIT $count")
}
private[dsl] object Limits {
  def apply(count: Int) = new Limits(count)
}
