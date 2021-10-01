package neo4s.cypher.dsl.syntax

import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses._
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class WherewithalSpec extends AnyWordSpec with should.Matchers {

  import neo4s.cypher.dsl.syntax.patterns._
  import neo4s.cypher.dsl.syntax.wherewithal._

  "Wherewithal" should {

    "ProductInstances" should {
      val person: Person             = randomize[Person].copy(name = "Cadbury")
      val dept: Department           = randomize[Department]
      val worksIn: WorksIn           = randomize[WorksIn]

      "when not in context" should {
        "throw exception" in {
          val age = 30
          val clause = person('age) > age
          assertThrows[java.lang.AssertionError] {
            clause.toWhereQuery() shouldBe DSLResult("a0.age > $a0_age", Map("a0_age" -> age))
          }
        }
      }

      "when in context" should {
        "provide query strings" should {
          "A{} > Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0", Map("person_age_0" -> 30))
          }
          "A{} >= Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) >= 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age >= $person_age_0", Map("person_age_0" -> 30))
          }
          "A{} < Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) < 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age < $person_age_0", Map("person_age_0" -> 30))
          }
          "A{} <= Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) <= 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age <= $person_age_0", Map("person_age_0" -> 30))
          }
          "A{} === Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) === 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age = $person_age_0", Map("person_age_0" -> 30))
          }
          "A{} =~ Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('name) =~ "Cad.*"
            clause.toWhereQuery(context) shouldBe DSLResult("person.name =~ $person_name_0", Map("person_name_0" -> "Cad.*"))
          }
          "A{} STARTS_WITH Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('name) STARTS_WITH "Cad"
            clause.toWhereQuery(context) shouldBe DSLResult("person.name STARTS WITH $person_name_0", Map("person_name_0" -> "Cad"))
          }
          "A{} ENDS_WITH Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('name) ENDS_WITH "bury"
            clause.toWhereQuery(context) shouldBe DSLResult("person.name ENDS WITH $person_name_0", Map("person_name_0" -> "bury"))
          }
          "A{} CONTAINS Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('name) CONTAINS "db"
            clause.toWhereQuery(context) shouldBe DSLResult("person.name CONTAINS $person_name_0", Map("person_name_0" -> "db"))
          }
          "A{} IN Any" in {
            val context = new Context()
            context.add(person)

            val names =  List("Jack","Jill","Cadbury")
            val clause = person('name) IN names
            clause.toWhereQuery(context) shouldBe DSLResult("person.name IN $person_name_0", Map("person_name_0" -> names))
          }

          "A{} > Any AND A{} < Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 10 AND person('age) < 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0 AND person.age < $person_age_1", Map("person_age_0" -> 10, "person_age_1" -> 30))
          }
          "A{} > Any OR A{} < Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 10 OR person('age) < 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0 OR person.age < $person_age_1", Map("person_age_0" -> 10, "person_age_1" -> 30))
          }
          "A{} > Any XOR A{} < Any" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 10 XOR person('age) < 30
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0 XOR person.age < $person_age_1", Map("person_age_0" -> 10, "person_age_1" -> 30))
          }

          "A{} > Any AND Pattern" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 18 AND person -| worksIn |-> dept
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0 AND (person)-[:WORKS_IN]->(:Department)",Map("person_age_0" -> 18))
          }

          "A{} > Any OR Pattern" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 18 OR person -| worksIn |-> dept
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0 OR (person)-[:WORKS_IN]->(:Department)",Map("person_age_0" -> 18))
          }

          "A{} > Any XOR Pattern" in {
            val context = new Context()
            context.add(person)

            val clause = person('age) > 18 XOR person -| worksIn |-> dept
            clause.toWhereQuery(context) shouldBe DSLResult("person.age > $person_age_0 XOR (person)-[:WORKS_IN]->(:Department)",Map("person_age_0" -> 18))
          }
        }
      }
    }
  }
}
