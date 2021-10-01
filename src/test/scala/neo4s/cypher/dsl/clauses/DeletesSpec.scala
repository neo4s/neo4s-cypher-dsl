package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.{Department, Person}
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class DeletesSpec extends AnyWordSpec with should.Matchers {


  "Deletes" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a path" in {
      val context = new Context()
      context.add(person)
      context.add(department)
      val deletes = Deletes(department, detaches = false)
      deletes.toQuery(context) shouldBe DSLResult("DELETE department")
    }
  }
}
