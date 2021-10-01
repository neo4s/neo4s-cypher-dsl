package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.{Department, Person}
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class WithSpec extends AnyWordSpec with should.Matchers with BeforeAndAfterEach {
  private val personA                   = randomize[Person]
  private val personB                   = randomize[Person]
  private val departmentA               = randomize[Department]
  private implicit var context: Context = new Context()

  override def beforeEach(): Unit = {
    context = new Context()
    context.add(personA)
    context.add(departmentA)
  }

  "With" should {

    "WITH query for an element in Context" in {
      With(personA).toQuery(context) shouldBe DSLResult("WITH person")
    }

    "WITH empty statement if no elements passed" in {
      With().toQuery(context) shouldBe DSLResult.empty
    }

    "WITH query for more than one element in Context" in {
      With(personA, departmentA).toQuery(context) shouldBe DSLResult("WITH person,department")
    }
    "WITH elements for a property" in {
      With(personA('name), departmentA).toQuery(context) shouldBe DSLResult("WITH person.name,department")
    }
    "WITH elements for multiple properties" in {
      With(personA('name, 'age), departmentA('name)).toQuery(context) shouldBe DSLResult("WITH person.name,person.age,department.name")
    }
    "throw if element to be WITHed not in Context" in {
      the[NoSuchElementException] thrownBy {
        With(personB).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "WITH aliased elements" in {
      With(personA -> "person", departmentA -> "dept")
        .toQuery(context) shouldBe DSLResult("WITH person as person,department as dept")
    }
    "WITH non-aliased and aliased elements in a single WITH" in {
      With(personA, departmentA -> "dept").toQuery(context) shouldBe DSLResult("WITH person,department as dept")
    }
    "WITH aliased elements for a property" in {
      With(personA('name) -> "personName", departmentA -> "dept")
        .toQuery(context) shouldBe DSLResult("WITH person.name as personName,department as dept")
    }
    "WITH aliased elements for multiple properties" in {
      With(personA('name) -> "name", personA('age) -> "age", departmentA('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("WITH person.name as name,person.age as age,department.name as departmentName")
    }
    "WITH aliased elements should return alias going forward" in {
      With(personA -> "p").toQuery(context)
      Returns(personA).toQuery(context) shouldBe DSLResult("RETURN p")
    }
    "should throw if multiple properties tried to be aliased" in {
      the[AssertionError] thrownBy {
        With(personA('name, 'age) -> "name").toQuery(context)
      } should have message "Alias one property at a time!"
    }
  }
}
