package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.{Context, DSLResult}

private[dsl] class Skips(count: Int) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = DSLResult(s"SKIP $count")
}
private[dsl] object Skips {
  def apply(count: Int) = new Skips(count)
}
