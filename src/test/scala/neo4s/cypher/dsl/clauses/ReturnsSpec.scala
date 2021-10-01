package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.syntax._
import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.{Department, Person}
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec extends AnyWordSpec with should.Matchers {

  private val personA                   = randomize[Person]
  private val personB                   = randomize[Person]
  private val departmentA               = randomize[Department]
  private val anyPerson                 = any[Person]
  private implicit val context: Context = new Context()

  "Returns" should {
    context.add(personA)
    context.add(departmentA)
    context.add(anyPerson)

    "return query for an element in Context" in {
      Returns(personA).toQuery(context) shouldBe DSLResult("RETURN person")
    }
    "return query for any element in Context" in {
      Returns(anyPerson).toQuery(context) shouldBe DSLResult("RETURN person_1")
    }
    "return query for anyRelation element in Context" in {
      val newContext = new Context()
      val anyRel = anyRelation
      Matches(personA -| anyRel |-> departmentA).toQuery(newContext)
      Returns(anyRel).toQuery(newContext) shouldBe DSLResult("RETURN any")
    }
    "return empty statement if no elements passed" in {
      Returns().toQuery(context) shouldBe DSLResult.empty
    }
    "return query for more than one element in Context" in {
      Returns(personA, departmentA).toQuery(context) shouldBe DSLResult("RETURN person,department")
    }
    "return elements for a property" in {
      Returns(personA('name), departmentA).toQuery(context) shouldBe DSLResult("RETURN person.name,department")
    }
    "return elements for multiple properties" in {
      Returns(personA('name, 'age), departmentA('name)).toQuery(context) shouldBe DSLResult("RETURN person.name,person.age,department.name")
    }
    "throw if element to be returned not in Context" in {
      the[NoSuchElementException] thrownBy {
        Returns(personB).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "return aliased elements" in {
      Returns(personA -> "pers", departmentA -> "dept")
        .toQuery(context) shouldBe DSLResult("RETURN person as pers,department as dept")
    }
    "return non-aliased and aliased elements in a single return" in {
      Returns(personA, departmentA -> "dept").toQuery(context) shouldBe DSLResult("RETURN person,department as dept")
    }
    "return aliased elements for a property" in {
      Returns(personA('name) -> "personName", departmentA -> "dept")
        .toQuery(context) shouldBe DSLResult("RETURN person.name as personName,department as dept")
    }
    "return aliased elements for multiple properties" in {
      Returns(personA('name) -> "name", personA('age) -> "age", departmentA('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("RETURN person.name as name,person.age as age,department.name as departmentName")
    }
    "should throw if multiple properties tried to be aliased" in {
      the[AssertionError] thrownBy {
        Returns(personA('name, 'age) -> "name").toQuery(context)
      } should have message "Alias one property at a time!"
    }
  }
}
