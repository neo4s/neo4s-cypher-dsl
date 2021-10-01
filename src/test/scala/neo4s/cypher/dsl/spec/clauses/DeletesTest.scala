package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.{Context, DSLResult}
import neo4s.cypher.dsl.spec.utils.Random.randomize
import neo4s.cypher.dsl.spec.utils.TestClasses.{Department, Person, WorksIn}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class DeletesTest extends AnyWordSpec with should.Matchers {


  "Deletes" should {
    val person: Person         = randomize[Person]
    val department: Department = randomize[Department]

    "provide query for a path" in {
      val context = new Context()
      context.add(person)
      context.add(department)
      val deletes = Deletes(department, detaches = false)
      deletes.toQuery(context) shouldBe DSLResult("DELETE a1")
    }
  }
}
