package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.DSLResult
import neo4s.cypher.dsl.spec.syntax.any
import neo4s.cypher.dsl.spec.syntax.patterns._
import neo4s.cypher.dsl.spec.utils.Random.randomize
import neo4s.cypher.dsl.spec.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.spec.utils.TestClasses.{Department, Person}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class OptionallyMatchesTest extends AnyWordSpec with should.Matchers {
  "Matches" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a single case class" in {
      val matches = OptionallyMatches(person)
      matches.toQuery() shouldBe DSLResult("OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
                                           Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node instance" in {
      val matches = OptionallyMatches(person('name))
      matches.toQuery() shouldBe DSLResult("OPTIONAL MATCH (a0:Person {name: {a0_name}})",
                                           Map("a0_name" -> person.name))
    }
    "provide query for a any node" in {
      val matches = OptionallyMatches(any[Person])
      matches.toQuery() shouldBe DSLResult("OPTIONAL MATCH (a0:Person)")

    }
    "provide query for a path" in {
      val matches = OptionallyMatches(person --> department)
      matches.toQuery() shouldBe DSLResult(
        "OPTIONAL MATCH (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})",
        Map("a0_id"   -> person.id,
            "a0_name" -> person.name,
            "a0_age"  -> person.age,
            "a1_id"   -> department.id,
            "a1_name" -> department.name)
      )

    }

  }
}
