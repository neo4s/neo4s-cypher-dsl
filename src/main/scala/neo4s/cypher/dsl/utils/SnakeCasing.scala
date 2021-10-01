package neo4s.cypher.dsl.utils

private[dsl] trait SnakeCasing {
  def upperSnakeCased(string: String): String = snakeCased(string).toUpperCase

  def snakeCased(rawName: String): String = {
    val name = if (isAllCaps(rawName)) rawName.toLowerCase else rawName
    "[A-Z\\d]".r.replaceAllIn(name, { m => "_" + m.group(0).toLowerCase() }).stripPrefix("_")
  }

  private def isAllCaps(name: String): Boolean = {
    name.filterNot(_ == '_').forall(_.isUpper)
  }
}
