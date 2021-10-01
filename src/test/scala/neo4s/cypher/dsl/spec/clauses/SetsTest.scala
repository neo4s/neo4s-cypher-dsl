package neo4s.cypherDSL.spec.clauses

import neo4s.cypherDSL.spec.syntax.patterns._
import neo4s.cypherDSL.spec.utils.Random.{randomize, _}
import neo4s.cypherDSL.spec.utils.TestClasses.Person
import neo4s.cypherDSL.spec.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SetsTest extends AnyWordSpec with should.Matchers {

  "Sets" should {
    val person: Person = randomize[Person]

    val context = new Context()

    Matches(person).toQuery(context)

    "provide query for a cypher entity" in {
      val sets = Sets(person('age) -> 25, person('name) -> "jane")
      sets.toQuery(context) shouldBe DSLResult("SET a0.age = {a0_age},a0.name = {a0_name}",
                                               Map("a0_name" -> "jane", "a0_age" -> 25))
    }

    "provide query for a case class when setters selected" in {
      val sets = Sets(person, List(person('name) -> "jane", person('id) -> "12"))
      sets.toQuery(context) shouldBe DSLResult("SET a0 = {a0.name = {a0_name},a0.id = {a0_id}}",
                                               Map("a0_name" -> "jane", "a0_id" -> "12"))
    }

    "provide query for a case class when no setters selected" in {
      val sets = Sets(person, List.empty)
      sets.toQuery(context) shouldBe DSLResult("SET a0 = {}", Map.empty)
    }
  }
}
