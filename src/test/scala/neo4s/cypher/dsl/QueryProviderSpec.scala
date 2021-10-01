package neo4s.cypher.dsl

import neo4s.cypher.dsl.utils.Random._
import neo4s.cypher.dsl.utils.TestClasses.Person
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

class QueryProviderSpec extends AnyWordSpec with should.Matchers {
  private implicit val context = new Context()
  private val person = randomize[Person]
  context.add(person)

  "QueryProvider" should {
    val queryProvider = aQueryProvider(person)

    "For mandatory product" should {
      "create an instance for a given Product" in {
        queryProvider.getMatchers(person) shouldBe List(
          DSLResult("id: $person_id",Map("person_id" -> person.id)),
          DSLResult("name: $person_name",Map("person_name" -> person.name)),
          DSLResult("age: $person_age",Map("person_age" -> person.age)))
      }
      "create an instance for a given Product for a selected attribute" in {
        queryProvider.getMatchers(person, 'id :: 'name :: HNil) shouldBe List(
          DSLResult("id: $person_id",Map("person_id" -> person.id)),
          DSLResult("name: $person_name",Map("person_name" -> person.name)))
      }
    }
  }

  private def aQueryProvider[T <: Product](element: T)(implicit queryProvider: QueryProvider[T]) = queryProvider
}
