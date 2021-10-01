package neo4s.cypher.dsl.operators

import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.Person
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class DistinctSpec extends AnyWordSpec with should.Matchers with BeforeAndAfterEach {
  private var context: Context = _
  private var person: Person   = _

  "DISTINCT" should {
    "provide query for an aliased case class if no alias provided in context" in {
      Distinct(person).toQuery(context) shouldBe DSLResult("DISTINCT person")
    }
    "provide query for an aliased case class if alias provided in context" in {
      Distinct(person -> "worker").toQuery(context) shouldBe DSLResult("DISTINCT person as worker")
    }
    "provide query for an aliased node if no alias provided in context" in {
      Distinct(person('name)).toQuery(context) shouldBe DSLResult("DISTINCT person.name")
    }
    "provide query for an aliased node if alias provided in context" in {
      Distinct(person('name) -> "workerName")
        .toQuery(context) shouldBe DSLResult("DISTINCT person.name as workerName")
    }
  }

  override def beforeEach() {
    context = new Context()
    person = randomize[Person]
    context.add(person)
  }

}
