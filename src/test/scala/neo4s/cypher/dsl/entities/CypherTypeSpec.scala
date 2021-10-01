package neo4s.cypher.dsl.entities

import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.{HeadOfDepartment, LocatedIn, Person, WorksIn}
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

import scala.reflect.runtime.universe.weakTypeOf

class CypherTypeSpec extends AnyWordSpec with should.Matchers {
  "NodeType" should {
    val personType = weakTypeOf[Person]
    ".toQuery" should {
      "provide query string when accessed for first time" in {
        val nodeType = NodeType(personType)
        nodeType.toQuery() shouldBe DSLResult("(person:Person)")
      }
      "provide query string when accessed for second or more time" in {
        val nodeType = NodeType(personType)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe DSLResult("(person:Person)")
        nodeType.toQuery(context) shouldBe DSLResult("(person)")
        nodeType.toQuery(context) shouldBe DSLResult("(person)")
      }
    }
  }
  "RelationType" should {
    val worksInType = weakTypeOf[WorksIn]
    ".toQuery" should {
      "provide query string when accessed for first time" in {
        val nodeType = RelationType(worksInType)
        nodeType.toQuery() shouldBe DSLResult("[works_in:WORKS_IN]")
      }
      "provide query string when accessed for second or more time" in {
        val nodeType = RelationType(worksInType)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe DSLResult("[works_in:WORKS_IN]")
        nodeType.toQuery(context) shouldBe DSLResult("[works_in]")
        nodeType.toQuery(context) shouldBe DSLResult("[works_in]")
      }
      "provide query string for multiple relationships type" in {
        val nodeType = RelationType(tpe = worksInType).or(RelationType(weakTypeOf[HeadOfDepartment]))
        nodeType.toQuery() shouldBe DSLResult("[rel:WORKS_IN|:HEAD_OF_DEPARTMENT]")
      }
      "provide query string for multiple relationships instance" in {
        val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]
        val nodeType = RelationType(tpe = worksInType).or(headOfDepartment, HNil)
        nodeType.toQuery() shouldBe DSLResult(
          "[rel:WORKS_IN|:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]",
          Map("head_of_department_id" -> headOfDepartment.id, "head_of_department_name" -> headOfDepartment.name)
        )
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2, 3))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[works_in:WORKS_IN*2..3]")
        }
        "provide [A*2] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[works_in:WORKS_IN*2]")
        }
      }
    }
  }
  "MultiRelationType" should {
    val worksInType = weakTypeOf[WorksIn]
    ".toQuery" should {
      "provide query string for multiple relationships type" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = MultiRelationType(orRelations)
        nodeType.toQuery() shouldBe DSLResult("[rel:HEAD_OF_DEPARTMENT|:LOCATED_IN]")
      }
      "provide query string when accessed for second or more time" in {
        val orRelations =
          List(RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]), RelationTypeOrInstance(weakTypeOf[LocatedIn]))
        val nodeType = MultiRelationType(orRelations)
        val context  = new Context()
        nodeType.toQuery(context) shouldBe DSLResult("[rel:HEAD_OF_DEPARTMENT|:LOCATED_IN]")
        nodeType.toQuery(context) shouldBe DSLResult("[rel]")
        nodeType.toQuery(context) shouldBe DSLResult("[rel]")
      }
      "provide query string for multiple relationships instance" in {
        val headOfDepartment: HeadOfDepartment = randomize[HeadOfDepartment]
        val locatedIn: LocatedIn               = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(worksInType),
            RelationTypeOrInstance(Relationship(headOfDepartment, HNil)),
            RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = MultiRelationType(orRelations)

        nodeType.toQuery() shouldBe DSLResult(
          "[rel:WORKS_IN|:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}|:LOCATED_IN {area: $located_in_area}]",
          Map("head_of_department_id" -> headOfDepartment.id, "head_of_department_name" -> headOfDepartment.name, "located_in_area" -> locatedIn.area)
        )
      }
      "provide query string for multiple relationships instance and type" in {
        val locatedIn: LocatedIn = randomize[LocatedIn]
        val orRelations =
          List(RelationTypeOrInstance(worksInType),
            RelationTypeOrInstance(weakTypeOf[HeadOfDepartment]),
            RelationTypeOrInstance(Relationship(locatedIn, HNil)))
        val nodeType = MultiRelationType(orRelations)

        nodeType.toQuery() shouldBe DSLResult("[rel:WORKS_IN|:HEAD_OF_DEPARTMENT|:LOCATED_IN {area: $located_in_area}]",
          Map("located_in_area" -> locatedIn.area))
      }
      "variable length relations" should {
        "provide [A*2..3] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2, 3))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[works_in:WORKS_IN*2..3]")
        }
        "provide [A*2] query string" in {
          val nodeType = RelationType(worksInType, VariableLengthRelation(2))
          val context  = new Context()
          nodeType.toQuery(context) shouldBe DSLResult("[works_in:WORKS_IN*2]")
        }
      }
    }
  }

}
