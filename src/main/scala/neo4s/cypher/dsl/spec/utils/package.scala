package neo4s.cypher.dsl.spec

package object utils {
  implicit class RichString(input: String) {
    def stripSemanticSugar: String = {
      input.substring(input.indexOf(":") + 1, input.length - 1)
    }
  }
}
