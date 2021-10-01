package neo4s.cypherDSL.spec

package object utils {
  implicit class RichString(input: String) {
    def stripSemanticSugar: String = {
      input.substring(input.indexOf(":") + 1, input.length - 1)
    }
  }
}
