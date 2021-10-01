package neo4s.cypher.dsl.spec

import neo4s.cypher.dsl.spec.clauses.{Matches, Returns}
import neo4s.cypher.dsl.spec.utils.Random._
import neo4s.cypher.dsl.spec.utils.TestClasses.Person
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class StatementTest extends AnyWordSpec with should.Matchers {

  private val person = randomize[Person]

  "A Statement" must {

    "return a result for some clauses" in {
      Statement(
        Seq(
          Matches(person),
          Returns(person)
        )).toQuery() shouldBe DSLResult(
        """MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})
          |RETURN a0""".stripMargin,
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age)
      )
    }
    "return empty result if passed no clauses" in {
      Statement().toQuery() shouldBe DSLResult.empty
    }
  }

}
