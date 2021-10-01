package neo4s.cypherDSL.spec

import neo4s.cypherDSL.spec.entities.CypherEntity

private[cypherDSL] case class PathLink(leftLink: Option[String], element: CypherEntity, rightLink: Option[String]) {
  def toQuery(context: Context = new Context()): DSLResult = {
    val entityResult = element.toQuery(context)
    entityResult.copy(
      query = leftLink.map(_.toString).mkString + entityResult.query + rightLink.map(_.toString).mkString)
  }
}
