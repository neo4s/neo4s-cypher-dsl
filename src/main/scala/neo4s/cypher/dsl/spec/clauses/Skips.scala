package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.{Context, DSLResult}

private[spec] class Skips(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = DSLResult(s"SKIP $count")
}
private[spec] object Skips {
  def apply(count: Int) = new Skips(count)
}
