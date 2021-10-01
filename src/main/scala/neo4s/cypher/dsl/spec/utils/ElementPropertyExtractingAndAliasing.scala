package neo4s.cypher.dsl.spec.utils

import neo4s.cypher.dsl.spec.entities.{CypherType, Node, NodeType, RelationType}
import neo4s.cypher.dsl.spec.Utils._

private[dsl] trait ElementPropertyExtracting {
  def getElementAndProperties(element: Product): (Any, List[String]) =
    element match {
      case s: Node[_, _]   => (s.element, s.properties.toList[Symbol].map(_.name))
      case s: RelationType => (s.fingerprint, List.empty[String])
      case s: NodeType     => (s.fingerprint, List.empty[String])
      case s               => (s, List.empty[String])
    }
}

private[dsl] trait ElementPropertyAliasing {
  private val tooManyPropertiesToAliasMessage = "Alias one property at a time!"

  @throws[AssertionError]
  def makeAliasedString(identifier: String, properties: List[String], alias: Option[String] = None): String = {
    if (properties.length > 1 && alias.isDefined) {
      throw new AssertionError(tooManyPropertiesToAliasMessage)
    }
    val identifierString = properties.map(p => s"$identifier.$p").mkString(",")
    val aliasString      = alias.map(a => s" as $a").mkString
    (if (identifierString.isEmpty) identifier else identifierString) + aliasString
  }
}

private[dsl] trait ElementPropertyExtractingAndAliasing
  extends ElementPropertyExtracting with ElementPropertyAliasing {}
