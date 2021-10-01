package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.DSLResult
import neo4s.cypher.dsl.syntax._
import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.{Department, Person}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class MatchesSpec extends AnyWordSpec with should.Matchers {
  "Matches" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a single case class" in {
      val matches = Matches(person)
      matches.toQuery() shouldBe DSLResult("MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})",
                                           Map("person_id" -> person.id, "person_name" -> person.name, "person_age" -> person.age))
    }
    "provide query for a node instance" in {
      val matches = Matches(person('name))
      matches.toQuery() shouldBe DSLResult("MATCH (person:Person {name: $person_name})", Map("person_name" -> person.name))
    }
    "provide query for a any node" in {
      val matches = Matches(any[Person])
      matches.toQuery() shouldBe DSLResult("MATCH (person:Person)")
    }
    "provide query for a path" in {
      val matches = Matches(person --> department)
      matches.toQuery() shouldBe DSLResult(
        "MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-->(department:Department {id: $department_id,name: $department_name})",
        Map("person_id"   -> person.id,
            "person_name" -> person.name,
            "person_age"  -> person.age,
            "department_id"   -> department.id,
            "department_name" -> department.name)
      )
    }

  }
}
