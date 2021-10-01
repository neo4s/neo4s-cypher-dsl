package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.syntax.patterns._
import neo4s.cypher.dsl.spec.utils.Random.randomize
import neo4s.cypher.dsl.spec.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.spec.utils.TestClasses.Person
import neo4s.cypher.dsl.spec.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class RemovesTest extends AnyWordSpec with should.Matchers {

  "Removes" should {
    val person: Person         = randomize[Person]
    "remove property from a node" in {
      val context = new Context()
      context.add(person)
      val removes = Removes(person('name))
      removes.toQuery(context) shouldBe DSLResult("REMOVE a0.name");
    }

    "remove multiple properties from a node" in {
      val context = new Context()
      context.add(person)
      val removes = Removes(person('name, 'id))
      removes.toQuery(context) shouldBe DSLResult("REMOVE a0.name,a0.id");
    }
  }
}
