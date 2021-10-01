package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.DSLResult
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SkipsTest extends AnyWordSpec with should.Matchers {

  "Skips" should {
    "provide query for a given count" in {
      Skips(10).toQuery() shouldBe DSLResult("SKIP 10")
    }
  }

}
