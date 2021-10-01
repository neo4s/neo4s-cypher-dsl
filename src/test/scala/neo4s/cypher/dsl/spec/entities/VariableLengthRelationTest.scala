package neo4s.cypherDSL.spec.entities

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class VariableLengthRelationTest extends AnyWordSpec with should.Matchers {
  "VariableLengthRelation" should {
    "provide *A..B query string" in {
      val relation = VariableLengthRelation(2, 3)
      relation.toQuery() shouldBe "*2..3"
    }
    "provide *A query string" in {
      val relation = VariableLengthRelation(2)
      relation.toQuery() shouldBe "*2"
    }
    "provide * query string" in {
      val relation = VariableLengthRelation()
      relation.toQuery() shouldBe "*"
    }
  }
}
