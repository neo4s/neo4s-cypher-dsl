package neo4s.cypher.dsl.spec

import neo4s.cypher.dsl.spec.utils.TestClasses.Person
import neo4s.cypher.dsl.spec.utils.Random._
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ContextTest extends AnyWordSpec with should.Matchers {

  "Context" should {
    val personList = (1 until 10).map(_ => randomize[Person])

    ".add" should {
      "allow to add an element to it and map it to an identifier" in {
        val context = new Context()
        context.add(personList.head)
        context.get(personList.head) shouldBe Option("a0")
      }
      "allow to add more than one element to it and map it to an identifier" in {
        val context = new Context()
        personList.map(person => context.add(person))
        val ids = personList.map(person => context.get(person))
        ids shouldBe personList.zipWithIndex.map(zipped => Option(s"a${zipped._2}")).toList
      }
    }
    ".get" should {
      val context = new Context()
      context.add(personList.head)

      "return identifier for an element in context" in {
        context.get(personList.head) shouldBe Option("a0")
      }
    }
    ".map" should {
      val context = new Context()
      context.add(personList.head)

      "allow a function to map on element of context" in {
        val result = context.map(personList.head)(_.toUpperCase)
        result shouldBe Option("A0")
      }
    }
  }
}
