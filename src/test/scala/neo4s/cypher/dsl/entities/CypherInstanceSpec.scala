package neo4s.cypher.dsl.entities

import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses._
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

import scala.reflect.runtime.universe.weakTypeOf

class CypherInstanceSpec extends AnyWordSpec with should.Matchers {
  private val person: Person                     = randomize[Person]
  private val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]

  "Node" should {
    ".toQuery" should {
      "provide query string for the product instance when not in context" in {
        val personNode = Node(person, HNil)
        personNode.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})",
                                                Map("person_id"   -> person.id,
                                                    "person_name" -> person.name,
                                                    "person_age"  -> person.age))
      }
      "provide query string for the product instance when in context" in {
        val context = new Context()
        context.add(person)
        val personNode = Node(person, HNil)
        personNode.toQuery(context) shouldBe DSLResult("(person)")
      }
    }
    "provide query string for the product instance with selected properties when not in context" in {
      val personNode = Node(person, 'name :: 'age :: HNil)
      personNode.toQuery() shouldBe DSLResult("(person:Person {name: $person_name,age: $person_age})",
                                              Map("person_name" -> person.name, "person_age" -> person.age))
    }
    "provide query string for the product instance with selected properties when in context" in {
      val context = new Context()
      context.add(person)
      val personNode = Node(person, 'name :: HNil)
      personNode.toQuery(context) shouldBe DSLResult("(person)")
    }
  }
  "Relationship" should {
    "when not in context" should {
      "provide query string for the product instance" in {
        val personRel = Relationship(headOfDepartment, HNil)
        personRel.toQuery() shouldBe DSLResult("[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]",
                                               Map("head_of_department_id" -> headOfDepartment.id, "head_of_department_name" -> headOfDepartment.name))
      }
      "provide query string for the product instance with selected properties" in {
        val personRel = Relationship(headOfDepartment, 'name :: HNil)
        personRel.toQuery() shouldBe DSLResult("[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}]",
                                               Map("head_of_department_name" -> headOfDepartment.name))
      }
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery() shouldBe DSLResult(
          "[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}|:WORKS_IN|:LOCATED_IN]",
          Map("head_of_department_id" -> headOfDepartment.id, "head_of_department_name" -> headOfDepartment.name))
      }
      "provide query string for multiple relationships instance" in {
        val worksIn: WorksIn     = randomize[WorksIn]
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(Relationship(worksIn, HNil)),
               RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery() shouldBe DSLResult(
          "[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}|:WORKS_IN {sinceDays: $works_in_sinceDays}|:LOCATED_IN {area: $located_in_area}]",
          Map("head_of_department_id"        -> headOfDepartment.id,
              "head_of_department_name"      -> headOfDepartment.name,
              "works_in_sinceDays" -> worksIn.sinceDays,
              "located_in_area"      -> locatedIn.area)
        )
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery() shouldBe DSLResult(
          "[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}|:WORKS_IN|:LOCATED_IN {area: $located_in_area}]",
          Map("head_of_department_id" -> headOfDepartment.id, "head_of_department_name" -> headOfDepartment.name, "located_in_area" -> locatedIn.area)
        )
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2, 3)))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*2..3]",
                                                       Map("head_of_department_id"   -> headOfDepartment.id,
                                                           "head_of_department_name" -> headOfDepartment.name))
        }
        "provide [A*2] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2)))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*2]",
                                                       Map("head_of_department_id"   -> headOfDepartment.id,
                                                           "head_of_department_name" -> headOfDepartment.name))
        }
        "provide query string for multiple relationships" in {
          val locatedIn: LocatedIn = randomize[LocatedIn]
          val orRelations =
            List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(Relationship(locatedIn, HNil)))
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2, 3)), orRelations)
          nodeType.toQuery() shouldBe DSLResult(
            "[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}|:WORKS_IN|:LOCATED_IN {area: $located_in_area}*2..3]",
            Map("head_of_department_id" -> headOfDepartment.id, "head_of_department_name" -> headOfDepartment.name,"located_in_area" -> locatedIn.area)
          )
        }
      }
    }
    "when in context" should {
      val context = new Context()
      context.add(headOfDepartment)
      "provide query string for the product instance" in {
        val personRel = Relationship(headOfDepartment, HNil)
        personRel.toQuery(context) shouldBe DSLResult("[head_of_department]")
      }
      "provide query string for the product instance with selected properties when in context" in {
        val personRel = Relationship(headOfDepartment, 'name :: HNil)
        personRel.toQuery(context) shouldBe DSLResult("[head_of_department]")
      }
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery(context) shouldBe DSLResult("[head_of_department]")
      }
      "provide query string for multiple relationships instance" in {
        val worksIn: WorksIn     = randomize[WorksIn]
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(Relationship(worksIn, HNil)),
               RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery(context) shouldBe DSLResult("[head_of_department]")
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orList =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orList)
        nodeType.toQuery(context) shouldBe DSLResult("[head_of_department]")
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2, 3)))
          nodeType.toQuery(context) shouldBe DSLResult("[head_of_department*2..3]")
        }
        "provide [A*2] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2)))
          nodeType.toQuery(context) shouldBe DSLResult("[head_of_department*2]")
        }
      }
    }
  }
}
