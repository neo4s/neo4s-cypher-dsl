package neo4s.cypher.dsl

import neo4s.cypher.dsl.syntax.any
import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.Person
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ContextSpec extends AnyWordSpec with should.Matchers {

  "Context" should {
    val personList = (1 until 10).map(_ => randomize[Person])

    ".add" should {
      "allow to add an element to it and map it to an identifier" in {
        val context = new Context()
        context.add(personList.head)
        context.get(personList.head) shouldBe Option("person")
      }
      "allow to add more than one element to it and map it to an identifier" in {
        val context = new Context()
        personList.map(person => context.add(person))
        val ids = personList.map(person => context.get(person))
        ids shouldBe personList.zipWithIndex.map(zipped => Option( if (zipped._2 == 0) "person" else s"person_${zipped._2}")).toList
      }
    }
    ".get" should {
      val context = new Context()
      context.add(personList.head)

      "return identifier for an element in context" in {
        context.get(personList.head) shouldBe Option("person")
      }
    }
    ".map" should {
      val context = new Context()
      context.add(personList.head)

      "allow a function to map on element of context" in {
        val result = context.map(personList.head)(_.toUpperCase)
        result shouldBe Option("PERSON")
      }
    }
    ".correlate" should {
      val context = new Context()
      val anyPerson = any[Person]

      "allow two query objects to have the same query id" in {
        context.correlate(personList.head,anyPerson)
        val headId = context.add(personList.head)
        val lastId = context.add(personList.last)
        val anyId = context.add(anyPerson)

        headId shouldBe anyId
        headId shouldBe "person"
        lastId shouldBe "person_1"

        context.get(personList.head) shouldBe Some("person")
        context.get(personList.last) shouldBe Some("person_1")
        context.get(anyPerson) shouldBe Some("person")
      }
    }
  }
}
