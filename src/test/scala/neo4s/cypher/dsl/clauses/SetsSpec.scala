package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random.{randomize, _}
import neo4s.cypher.dsl.utils.TestClasses.{OptionalPerson, Person}
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SetsSpec extends AnyWordSpec with should.Matchers {

  "Sets" should {
    val person: Person = randomize[Person]

    val context = new Context()

    Matches(person).toQuery(context)

    "provide query for a cypher entity" in {
      val sets = Sets(Sets.setCommand, person('age) -> 25, person('name) -> "jane")
      sets.toQuery(context) shouldBe DSLResult("SET person.age = $person_age,person.name = $person_name",
                                               Map("person_name" -> "jane", "person_age" -> 25))
    }

    "provide query for a case class when setters selected" in {
      val sets = Sets(Sets.setCommand, person, List(person('name) -> "jane", person('id) -> "12"))
      sets.toQuery(context) shouldBe DSLResult("SET person = {name = $person_name,id = $person_id}",
                                               Map("person_name" -> "jane", "person_id" -> "12"))
    }

    "provide query for a case class when no setters selected" in {
      val sets = Sets(Sets.setCommand, person, List.empty)
      sets.toQuery(context) shouldBe DSLResult("SET person = {}", Map.empty)
    }
  }


  "OnCreateSets" should {
    val person: Person = randomize[Person]

    val context = new Context()

    Matches(person).toQuery(context)

    "provide query for a cypher entity" in {
      val sets = Sets(Sets.onCreateSetCommand, person('age) -> 25, person('name) -> "jane")
      sets.toQuery(context) shouldBe DSLResult("ON CREATE SET person.age = $person_age,person.name = $person_name",
        Map("person_name" -> "jane", "person_age" -> 25))
    }

    "provide query for a case class when setters selected" in {
      val sets = Sets(Sets.onCreateSetCommand, person, List(person('name) -> "jane", person('id) -> "12"))
      sets.toQuery(context) shouldBe DSLResult("ON CREATE SET person = {name = $person_name,id = $person_id}",
        Map("person_name" -> "jane", "person_id" -> "12"))
    }

    "provide query for a case class when no setters selected" in {
      val sets = Sets(Sets.onCreateSetCommand, person, List.empty)
      sets.toQuery(context) shouldBe DSLResult("ON CREATE SET person = {}", Map.empty)
    }
  }

  "OnMatchSets" should {
    val person: Person = randomize[Person]
    val oPerson: OptionalPerson = randomize[OptionalPerson].copy(spouse = None)
    println(s"optionalPerson --> $oPerson")

    val context = new Context()

    Matches(person).toQuery(context)
    Matches(oPerson).toQuery(context)

    "provide query for a cypher entity" in {
      val sets = Sets(Sets.onMatchSetCommand, person('age) -> 25, person('name) -> "jane")
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET person.age = $person_age,person.name = $person_name",
        Map("person_name" -> "jane", "person_age" -> 25))
    }

    "provide query for a case class when setters selected" in {
      val sets = Sets(Sets.onMatchSetCommand, person, List(person('name) -> "jane", person('id) -> "12"))
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET person = {name = $person_name,id = $person_id}",
        Map("person_name" -> "jane", "person_id" -> "12"))
    }

    "provide query for a case class when no setters selected" in {
      val sets = Sets(Sets.onMatchSetCommand, person, List.empty)
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET person = {}", Map.empty)
    }

    "provide query for a case class with a Some Option fields" in {
      val sets = Sets(Sets.onMatchSetCommand, oPerson('name) -> "jane", oPerson('id) -> "12", oPerson('spouse) -> Some("jack"))
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET optional_person.name = $optional_person_name,optional_person.id = $optional_person_id",
        Map("optional_person_name" -> "jane", "optional_person_id" -> "12"))
    }

    "provide query for a case class with a None Option fields" in {
      val sets = Sets(Sets.onMatchSetCommand, oPerson('name) -> "jane", oPerson('id) -> "12", oPerson('spouse) -> None)
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET optional_person.name = $optional_person_name,optional_person.id = $optional_person_id",
        Map("optional_person_name" -> "jane", "optional_person_id" -> "12"))
    }

    "provide query for a case class with a None Option when setters selected" in {
      val sets = Sets(Sets.onMatchSetCommand, oPerson, List(oPerson('name) -> "jane", oPerson('id) -> "12", oPerson('spouse) -> Some("jack")))
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET optional_person = {name = $optional_person_name,id = $optional_person_id}",
        Map("optional_person_name" -> "jane", "optional_person_id" -> "12"))
    }

    "provide query for a case class with a None Option when no setters selected" in {
      val sets = Sets(Sets.onMatchSetCommand, oPerson, List.empty)
      sets.toQuery(context) shouldBe DSLResult("ON MATCH SET optional_person = {}", Map.empty)
    }
  }
}
