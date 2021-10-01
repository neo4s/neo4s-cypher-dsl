package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.{Context, DSLResult}
import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.{Department, Person}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class OrdersBySpec extends AnyWordSpec with should.Matchers {

  private val personA          = randomize[Person]
  private val personB          = randomize[Person]
  private val departmentA      = randomize[Department]
  private implicit val context = new Context()

  "OrdersBy" should {
    context.add(personA)
    context.add(departmentA)

    "default ordering" should {
      "return query for an element in Context" in {
        OrdersBy(personA).toQuery(context) shouldBe DSLResult("ORDER BY person")
      }

      "return empty statement if no elements passed" in {
        OrdersBy().toQuery(context) shouldBe DSLResult.empty
      }

      "return query for more than one element in Context" in {
        OrdersBy(personA, departmentA).toQuery(context) shouldBe DSLResult("ORDER BY person,department")
      }
      "return elements for a property" in {
        OrdersBy(personA('name), departmentA).toQuery(context) shouldBe DSLResult("ORDER BY person.name,department")
      }
      "return elements for multiple properties" in {
        OrdersBy( personA('name, 'age), departmentA('name))
          .toQuery(context) shouldBe DSLResult("ORDER BY person.name,person.age,department.name")
      }
      "throw if element to be returned not in Context" in {
        the[NoSuchElementException] thrownBy {
          OrdersBy(personB).toQuery(context)
        } should have message "One or more of the elements to be returned are not in Context!"
      }
    }
    "DESC ordering" should {
      "return query for an element in Context" in {
        OrdersBy(personA.DESC).toQuery(context) shouldBe DSLResult("ORDER BY person DESC")
      }

      "return empty statement if no elements passed" in {
        OrdersBy().toQuery(context) shouldBe DSLResult.empty
      }

      "return query for more than one element in Context" in {
        OrdersBy(personA.DESC, departmentA).toQuery(context) shouldBe DSLResult("ORDER BY person DESC,department")
      }
      "return elements for a property" in {
        OrdersBy(personA('name).DESC, departmentA).toQuery(context) shouldBe DSLResult("ORDER BY person.name DESC,department")
      }
      "return elements for multiple properties" in {
        OrdersBy(personA('name, 'age).DESC, departmentA('name))
          .toQuery(context) shouldBe DSLResult("ORDER BY person.name DESC,person.age DESC,department.name")
      }
      "throw if element to be returned not in Context" in {
        the[NoSuchElementException] thrownBy {
          OrdersBy(personB.DESC).toQuery(context)
        } should have message "One or more of the elements to be returned are not in Context!"
      }
    }
  }
}
