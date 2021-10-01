package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.DSLResult
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class LimitsTest extends AnyWordSpec with should.Matchers {
  "Limits" should {
    "provide query for a given count" in {
      Limits(10).toQuery() shouldBe DSLResult("LIMIT 10")
    }
  }
}
