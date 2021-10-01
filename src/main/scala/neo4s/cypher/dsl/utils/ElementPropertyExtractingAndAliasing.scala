package neo4s.cypher.dsl.utils

import neo4s.cypher.dsl.entities.{CypherType, Node, NodeType, RelationType}
import neo4s.cypher.dsl.Utils._

private[cypher] trait ElementPropertyExtracting {
  def getElementAndProperties(element: Product): (Any, List[String]) =
    element match {
      case s: Node[_, _] => (s.element, s.properties.toList[Symbol].map(_.name))
      case RelationType(tpe,_,uuid) => (RelationType(tpe,None,uuid), List.empty[String])
      case s               => (s, List.empty[String])
    }
}

private[cypher] trait ElementPropertyAliasing {
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

private[cypher] trait ElementPropertyExtractingAndAliasing
  extends ElementPropertyExtracting with ElementPropertyAliasing {}
