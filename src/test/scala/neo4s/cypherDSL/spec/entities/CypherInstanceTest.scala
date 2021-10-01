package neo4s.cypherDSL.spec.entities

import neo4s.cypherDSL.spec.utils.Random.randomize
import neo4s.cypherDSL.spec.utils.TestClasses._
import neo4s.cypherDSL.spec.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

import scala.reflect.runtime.universe.weakTypeOf

class CypherInstanceTest extends AnyWordSpec with should.Matchers {
  private val person: Person                     = randomize[Person]
  private val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]

  "Node" should {
    ".toQuery" should {
      "provide query string for the product instance when not in context" in {
        val personNode = Node(person, HNil)
        personNode.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})",
                                                Map("a0_id"   -> person.id,
                                                    "a0_name" -> person.name,
                                                    "a0_age"  -> person.age))
      }
      "provide query string for the product instance when in context" in {
        val context = new Context()
        context.add(person)
        val personNode = Node(person, HNil)
        personNode.toQuery(context) shouldBe DSLResult("(a0)")
      }
    }
    "provide query string for the product instance with selected properties when not in context" in {
      val personNode = Node(person, 'name :: 'age :: HNil)
      personNode.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name},age: {a0_age}})",
                                              Map("a0_name" -> person.name, "a0_age" -> person.age))
    }
    "provide query string for the product instance with selected properties when in context" in {
      val context = new Context()
      context.add(person)
      val personNode = Node(person, 'name :: HNil)
      personNode.toQuery(context) shouldBe DSLResult("(a0)")
    }
  }
  "Relationship" should {
    "when not in context" should {
      "provide query string for the product instance" in {
        val personRel = Relationship(headOfDepartment, HNil)
        personRel.toQuery() shouldBe DSLResult("[a0:HEAD_OF_DEPARTMENT {id: {a0_id},name: {a0_name}}]",
                                               Map("a0_id" -> headOfDepartment.id, "a0_name" -> headOfDepartment.name))
      }
      "provide query string for the product instance with selected properties" in {
        val personRel = Relationship(headOfDepartment, 'name :: HNil)
        personRel.toQuery() shouldBe DSLResult("[a0:HEAD_OF_DEPARTMENT {name: {a0_name}}]",
                                               Map("a0_name" -> headOfDepartment.name))
      }
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery() shouldBe DSLResult(
          "[a0:HEAD_OF_DEPARTMENT {id: {a0_id},name: {a0_name}}|:WORKS_IN|:LOCATED_IN]",
          Map("a0_id" -> headOfDepartment.id, "a0_name" -> headOfDepartment.name))
      }
      "provide query string for multiple relationships instance" in {
        val worksIn: WorksIn     = randomize[WorksIn]
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(Relationship(worksIn, HNil)),
               RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery() shouldBe DSLResult(
          "[a2:HEAD_OF_DEPARTMENT {id: {a2_id},name: {a2_name}}|:WORKS_IN {sinceDays: {a0_sinceDays}}|:LOCATED_IN {area: {a1_area}}]",
          Map("a2_id"        -> headOfDepartment.id,
              "a2_name"      -> headOfDepartment.name,
              "a0_sinceDays" -> worksIn.sinceDays,
              "a1_area"      -> locatedIn.area)
        )
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery() shouldBe DSLResult(
          "[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}|:WORKS_IN|:LOCATED_IN {area: {a0_area}}]",
          Map("a1_id" -> headOfDepartment.id, "a1_name" -> headOfDepartment.name, "a0_area" -> locatedIn.area)
        )
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2, 3)))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[a0:HEAD_OF_DEPARTMENT {id: {a0_id},name: {a0_name}}*2..3]",
                                                       Map("a0_id"   -> headOfDepartment.id,
                                                           "a0_name" -> headOfDepartment.name))
        }
        "provide [A*2] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2)))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[a0:HEAD_OF_DEPARTMENT {id: {a0_id},name: {a0_name}}*2]",
                                                       Map("a0_id"   -> headOfDepartment.id,
                                                           "a0_name" -> headOfDepartment.name))
        }
        "provide query string for multiple relationships" in {
          val locatedIn: LocatedIn = randomize[LocatedIn]
          val orRelations =
            List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(Relationship(locatedIn, HNil)))
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2, 3)), orRelations)
          nodeType.toQuery() shouldBe DSLResult(
            "[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}|:WORKS_IN|:LOCATED_IN {area: {a0_area}}*2..3]",
            Map("a1_id" -> headOfDepartment.id, "a1_name" -> headOfDepartment.name,"a0_area" -> locatedIn.area)
          )
        }
      }
    }
    "when in context" should {
      val context = new Context()
      context.add(headOfDepartment)
      "provide query string for the product instance" in {
        val personRel = Relationship(headOfDepartment, HNil)
        personRel.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "provide query string for the product instance with selected properties when in context" in {
        val personRel = Relationship(headOfDepartment, 'name :: HNil)
        personRel.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "provide query string for multiple relationships instance" in {
        val worksIn: WorksIn     = randomize[WorksIn]
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(Relationship(worksIn, HNil)),
               RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orRelations)
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orList =
          List(RelationTypeOrInstance(weakTypeOf[WorksIn]), RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = Relationship(headOfDepartment, HNil, None, orList)
        nodeType.toQuery(context) shouldBe DSLResult("[a0]")
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2, 3)))
          nodeType.toQuery(context) shouldBe DSLResult("[a0*2..3]")
        }
        "provide [A*2] query string" in {
          val nodeType = Relationship(headOfDepartment, HNil, Option(VariableLengthRelation(2)))
          nodeType.toQuery(context) shouldBe DSLResult("[a0*2]")
        }
      }
    }
  }
}
