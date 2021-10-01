package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.Person
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class RemovesSpec extends AnyWordSpec with should.Matchers {

  "Removes" should {
    val person: Person = randomize[Person]
    "remove property from a node" in {
      val context = new Context()
      context.add(person)
      val removes = Removes(person('name))
      removes.toQuery(context) shouldBe DSLResult("REMOVE person.name");
    }

    "remove multiple properties from a node" in {
      val context = new Context()
      context.add(person)
      val removes = Removes(person('name, 'id))
      removes.toQuery(context) shouldBe DSLResult("REMOVE person.name,person.id");
    }
  }
}
