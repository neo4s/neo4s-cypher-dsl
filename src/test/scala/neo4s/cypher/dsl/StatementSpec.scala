package neo4s.cypher.dsl

import neo4s.cypher.dsl.clauses.{Matches, Returns}
import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.Person
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class StatementSpec extends AnyWordSpec with should.Matchers {

  private val person = randomize[Person]

  "A Statement" must {

    "return a result for some clauses" in {
      Statement(
        Seq(
          Matches(person),
          Returns(person)
        )).toQuery() shouldBe DSLResult(
        """MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})
          |RETURN person""".stripMargin,
        Map("person_id" -> person.id, "person_name" -> person.name, "person_age" -> person.age)
      )
    }
    "return empty result if passed no clauses" in {
      Statement().toQuery() shouldBe DSLResult.empty
    }
  }

}
