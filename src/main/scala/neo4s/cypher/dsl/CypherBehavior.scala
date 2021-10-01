package neo4s.cypher.dsl

object CypherBehavior {
  type PropertyPlaceholderMapper = (String, (String, Any)) => Map[String,Any]
  type FieldTypeMapper = PartialFunction[(String,Any),(String,Any)]

  def defaultIdFieldTransform: (String) => String = (str) => str
  def defaultFieldMapper: FieldTypeMapper = {
    case (key, value) => (key, value)
  }
}

case class CypherBehavior(
  nodeIdTransform: (String) => String = CypherBehavior.defaultIdFieldTransform,
  mapper: CypherBehavior.FieldTypeMapper = CypherBehavior.defaultFieldMapper,
)

