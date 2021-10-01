package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.{Context, DSLResult}

private[cypher] class Limits(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = DSLResult(s"LIMIT $count")
}
private[cypher] object Limits {
  def apply(count: Int) = new Limits(count)
}
