package neo4s.cypherDSL.spec.clauses

import neo4s.cypherDSL.spec.DSLResult
import neo4s.cypherDSL.spec.syntax.any
import neo4s.cypherDSL.spec.syntax.patterns._
import neo4s.cypherDSL.spec.utils.Random.randomize
import neo4s.cypherDSL.spec.utils.TestClasses.{Department, Person}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class MergesTest extends AnyWordSpec with should.Matchers {

  "Merges" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a single case class" in {
      val merges = Merges(person)
      merges.toQuery() shouldBe DSLResult("MERGE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
        Map("a0_id" -> person.id, "a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query for a node instance" in {
      val merges = Merges(person('name))
      merges.toQuery() shouldBe DSLResult("MERGE (a0:Person {name: {a0_name}})", Map("a0_name" -> person.name))
    }
    "provide query for a any node" in {
      val merges = Merges(any[Person])
      merges.toQuery() shouldBe DSLResult("MERGE (a0:Person)")
    }
    "provide query for a path" in {
      val merges = Merges(person --> department)
      merges.toQuery() shouldBe DSLResult(
        "MERGE (a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})",
        Map("a0_id"   -> person.id,
          "a0_name" -> person.name,
          "a0_age"  -> person.age,
          "a1_id"   -> department.id,
          "a1_name" -> department.name)
      )
    }

  }


}
