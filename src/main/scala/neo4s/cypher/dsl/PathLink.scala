package neo4s.cypher.dsl

import neo4s.cypher.dsl.entities.CypherEntity

private[cypher] case class PathLink(leftLink: Option[String], element: CypherEntity, rightLink: Option[String]) {
  def toQuery(context: Context = new Context()): DSLResult = {
    val entityResult = element.toQuery(context)
    entityResult.copy(query = leftLink.mkString + entityResult.query + rightLink.mkString)
  }

  def toWhereQuery(context: Context = new Context()): DSLResult = {
    val entityResult = element.toWhereQuery(context, None)
    entityResult.copy(query = leftLink.mkString + entityResult.query + rightLink.mkString)
  }
}
