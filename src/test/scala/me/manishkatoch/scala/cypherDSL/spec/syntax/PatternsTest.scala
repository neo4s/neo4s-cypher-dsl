package me.manishkatoch.scala.cypherDSL.spec.syntax

import me.manishkatoch.scala.cypherDSL.spec.syntax.patterns._
import me.manishkatoch.scala.cypherDSL.spec.utils.Random.randomize
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses.ImplicitCache._
import me.manishkatoch.scala.cypherDSL.spec.utils.TestClasses._
import me.manishkatoch.scala.cypherDSL.spec.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
class PatternsTest extends AnyWordSpec with should.Matchers {

  "Patterns" should {

    "ProductInstances" should {
      val person: Person             = randomize[Person]
      val dept: Department           = randomize[Department]
      val deptHead: HeadOfDepartment = randomize[HeadOfDepartment]
      val worksIn: WorksIn           = randomize[WorksIn]
      val locatedIn: LocatedIn       = randomize[LocatedIn]
      val region: Region             = randomize[Region]

      "when not in context" should {
        "provide query strings" should {
          "A -- B" in {
            val path = person -- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})--(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A -- B{}" in {
            val path = person -- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})--(a1:Department {name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_name" -> dept.name))
          }
          "A{} -- B" in {
            val path = person('name, 'age) -- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name},age: {a0_age}})--(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A{} -- B{}" in {
            val path = person('age) -- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {age: {a0_age}})--(a1:Department {name: {a1_name}})",
              Map("a0_age"  -> person.age,
                "a1_name" -> dept.name))
          }
          "A --> B" in {
            val path = person --> dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A --> B{}" in {
            val path = person --> dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-->(a1:Department {name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_name" -> dept.name))
          }
          "A{} --> B" in {
            val path = person('id) --> dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-->(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A{} --> B{}" in {
            val path = person('id) --> dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-->(a1:Department {name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a1_name" -> dept.name))
          }
          "A <-- B" in {
            val path = person <-- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<--(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A <-- B{}" in {
            val path = person <-- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<--(a1:Department {id: {a1_id}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id))
          }
          "A{} <-- B" in {
            val path = person('id) <-- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})<--(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A{} <-- B{}" in {
            val path = person('id) <-- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})<--(a1:Department {name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a1_name" -> dept.name))
          }
          "A -[C]- B" in {
            val path = person -| deptHead |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C|D]- B" in {
            val path = person -| deptHead | worksIn |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a2:HEAD_OF_DEPARTMENT {id: {a2_id},name: {a2_name}}|:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a3:Department {id: {a3_id},name: {a3_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_sinceDays" -> worksIn.sinceDays,
                "a2_id"   -> deptHead.id,
                "a2_name" -> deptHead.name,
                "a3_id"   -> dept.id,
                "a3_name" -> dept.name))
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| deptHead |- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]-(a2:Department {name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_name" -> dept.name))
          }
          "A -[C {}]- B" in {
            val path = person -| deptHead('id) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A{} -[C {}]- B{}" in {
            val path = person('id) -| deptHead('id) |- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]-(a2:Department {name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> deptHead.id,
                "a2_name" -> dept.name))
          }
          "A -[C]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
              "a0_name" -> person.name,
              "a0_age"  -> person.age,
              "a1_id"   -> deptHead.id,
              "a1_name" -> deptHead.name,
              "a2_id"   -> dept.id,
              "a2_name" -> dept.name))
          }
          "A{} -[C]-> B{}" in {
            val path = person('name) -| deptHead |-> dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id))
          }
          "A -[C {}]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A{} -[C {}]-> B{}" in {
            val path = person('id) -| deptHead('id) |-> dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]->(a2:Department {name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> deptHead.id,
                "a2_name" -> dept.name))
          }
          "A <-[C]- B" in {
            val path = person <-| deptHead |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A{} <-[C]- B{}" in {
            val path = person('id) <-| deptHead |- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})<-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]-(a2:Department {name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_name" -> dept.name))
          }
          "A <-[C {}]- B" in {
            val path = person <-| deptHead('id) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A{} <-[C {}]- B{}" in {
            val path = person('id) <-| deptHead('id) |- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})<-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]-(a2:Department {name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a1_id"   -> deptHead.id,
                "a2_name" -> dept.name))
          }
          "(A)-[R]-(B)-[R2]-(A2)" in {
            val path = person -| worksIn |- dept -| locatedIn |- region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region {name: {a4_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_sinceDays" -> worksIn.sinceDays,
                "a3_area"   -> locatedIn.area,
                "a4_name" -> region.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "(A)-[R]->(B)-[R2]->(A2)" in {
            val path = person -| worksIn |-> dept -| locatedIn |-> region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN {area: {a3_area}}]->(a4:Region {name: {a4_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_sinceDays" -> worksIn.sinceDays,
                "a3_area"   -> locatedIn.area,
                "a4_name" -> region.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "(A)<-[R]-(B)<-[R2]-(A2)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region {name: {a4_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_sinceDays" -> worksIn.sinceDays,
                "a3_area"   -> locatedIn.area,
                "a4_name" -> region.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "(A)<-[R]-(B)-[R2]->(A2)" in {
            val path = person <-| worksIn |- dept -| deptHead |-> person
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:HEAD_OF_DEPARTMENT {id: {a3_id},name: {a3_name}}]->(a0)",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_sinceDays" -> worksIn.sinceDays,
                "a3_id"   -> deptHead.id,
                "a3_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "(A)-[R]->(B)<-[R2]-(A2)" in {
            val path = person -| deptHead |-> dept <-| worksIn |- person
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}]->(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:WORKS_IN {sinceDays: {a3_sinceDays}}]-(a0)",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a3_sinceDays" -> worksIn.sinceDays,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "(A)<-[R]-(B)<-[R2]-(A2)-->(C)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region --> person
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region {name: {a4_name}})-->(a0)",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_sinceDays" -> worksIn.sinceDays,
                "a3_area"   -> locatedIn.area,
                "a4_name" -> region.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }

          "A -[*1..3]- B" in {
            val path = person -|* (1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[*1..3]-(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A -[*1]- B" in {
            val path = person -|* 1 |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[*1]-(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A -[*]- B" in {
            val path = person -|* anyLength |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[*]-(a1:Department {id: {a1_id},name: {a1_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> dept.id,
                "a1_name" -> dept.name))
          }
          "A -[C*]- B" in {
            val path = person -|* deptHead |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C*1]- B" in {
            val path = person -|* (deptHead, 1) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*1]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C*1..3]- B" in {
            val path = person -|* (deptHead, 1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*1..3]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C{}*]- B" in {
            val path = person -|* deptHead('name) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C{}*1]- B" in {
            val path = person -|* (deptHead('name), 1) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*1]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C{}*1..3]- B" in {
            val path = person -|* (deptHead('name), 1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*1..3]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id"   -> person.id,
                "a0_name" -> person.name,
                "a0_age"  -> person.age,
                "a1_name" -> deptHead.name,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
          "A{} -[*1..3]- B{}" in {
            val path = person('name) -|* (1 to 3) |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[*1..3]-(a1:Department {id: {a1_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> dept.id))
          }
          "A{} -[*1]- B{}" in {
            val path = person('name) -|* 1 |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[*1]-(a1:Department {id: {a1_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> dept.id))
          }
          "A{} -[*]- B{}" in {
            val path = person('name) -|* anyLength |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[*]-(a1:Department {id: {a1_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> dept.id))
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person('name) -|* (deptHead, 1 to 3) |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*1..3]-(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> deptHead.id,
              "a1_name" -> deptHead.name,
              "a2_id" -> dept.id))
          }
          "A{} -[C*1]- B{}" in {
            val path = person('name) -|* (deptHead, 1) |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*1]-(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id" -> dept.id))
          }
          "A{} -[C*]- B{}" in {
            val path = person('name) -|* deptHead |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*]-(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_id"   -> deptHead.id,
                "a1_name" -> deptHead.name,
                "a2_id" -> dept.id))
          }
          "A{} -[C{}*1..3]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1 to 3) |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*1..3]-(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_name" -> deptHead.name,
                "a2_id" -> dept.id))
          }
          "A{} -[C{}*1]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1) |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*1]-(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_name" -> deptHead.name,
                "a2_id" -> dept.id))
          }
          "A{} -[C{}*]- B{}" in {
            val path = person('name) -|* deptHead('name) |- dept('id)
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name}})-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*]-(a2:Department {id: {a2_id}})",
              Map("a0_name" -> person.name,
                "a1_name" -> deptHead.name,
                "a2_id" -> dept.id))
          }
        }
      }
      "when in context" should {
        val context = new Context()
        context.add(person)
        context.add(dept)
        context.add(deptHead)
        context.add(worksIn)
        context.add(region)
        context.add(locatedIn)

        "provide query strings" should {
          "A -- B" in {
            val path = person -- dept
            path.toQuery(context) shouldBe DSLResult("(a0)--(a1)")
          }
          "A -- B{}" in {
            val path = person -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)--(a1)")
          }
          "A{} -- B" in {
            val path = person('name) -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)--(a1)")
          }
          "A{} -- B{}" in {
            val path = person('name) -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)--(a1)")
          }
          "A --> B" in {
            val path = person --> dept
            path.toQuery(context) shouldBe DSLResult("(a0)-->(a1)")
          }
          "A --> B{}" in {
            val path = person --> dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-->(a1)")
          }
          "A{} --> B" in {
            val path = person('age) --> dept
            path.toQuery(context) shouldBe DSLResult("(a0)-->(a1)")
          }
          "A{} --> B{}" in {
            val path = person('id) --> dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-->(a1)")
          }
          "A <-- B" in {
            val path = person <-- dept
            path.toQuery(context) shouldBe DSLResult("(a0)<--(a1)")
          }
          "A <-- B{}" in {
            val path = person <-- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)<--(a1)")
          }
          "A{} <-- B" in {
            val path = person('name) <-- dept
            path.toQuery(context) shouldBe DSLResult("(a0)<--(a1)")
          }
          "A{} <-- B{}" in {
            val path = person('id) <-- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)<--(a1)")
          }
          "A -[C]- B" in {
            val path = person -| deptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]-(a1)")
          }
          "A -[C|D]- B" in {
            val path = person -| deptHead | worksIn |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]-(a1)")
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| deptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]-(a1)")
          }
          "A -[C {}]- B" in {
            val path = person -| deptHead('id) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]-(a1)")
          }
          "A{} -[C {}]- B{}" in {
            val path = person('id) -| deptHead('id) |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]-(a1)")
          }
          "A -[C]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]->(a1)")
          }
          "A{} -[C]-> B{}" in {
            val path = person('name) -| deptHead |-> dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]->(a1)")
          }
          "A -[C {}]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]->(a1)")
          }
          "A{} -[C {}]-> B{}" in {
            val path = person('id) -| deptHead('id) |-> dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]->(a1)")
          }
          "A <-[C]- B" in {
            val path = person <-| deptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a2]-(a1)")
          }
          "A{} <-[C]- B{}" in {
            val path = person('id) <-| deptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a2]-(a1)")
          }
          "A <-[C {}]- B" in {
            val path = person <-| deptHead('id) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a2]-(a1)")
          }
          "A{} <-[C {}]- B{}" in {
            val path = person('id) <-| deptHead('id) |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a2]-(a1)")
          }
          "(A)-[R]-(B)-[R2]-(A2)" in {
            val path = person -| worksIn |- dept -| locatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(a0)-[a3]-(a1)-[a5]-(a4)")
          }
          "(A)-[R]->(B)-[R2]->(A2)" in {
            val path = person -| worksIn |-> dept -| locatedIn |-> region
            path.toQuery(context) shouldBe DSLResult("(a0)-[a3]->(a1)-[a5]->(a4)")
          }
          "(A)<-[R]-(B)<-[R2]-(A2)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a3]-(a1)<-[a5]-(a4)")
          }
          "(A)<-[R]-(B)-[R2]->(A2)" in {
            val path = person <-| worksIn |- dept -| deptHead |-> person
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a3]-(a1)-[a2]->(a0)")
          }
          "(A)-[R]->(B)<-[R2]-(A2)" in {
            val path = person -| deptHead |-> dept <-| worksIn |- person
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2]->(a1)<-[a3]-(a0)")
          }
          "(A)<-[R]-(B)<-[R2]-(A2)-->(C)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region --> person
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a3]-(a1)<-[a5]-(a4)-->(a0)")
          }
          "A -[*1..3]- B" in {
            val path = person -|* (1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[*1..3]-(a1)")
          }
          "A -[*1]- B" in {
            val path = person -|* 1 |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[*1]-(a1)")
          }
          "A -[*]- B" in {
            val path = person -|* anyLength |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[*]-(a1)")
          }
          "A{} -[*1..3]- B{}" in {
            val path = person('name) -|* (1 to 3) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[*1..3]-(a1)")
          }
          "A{} -[*1]- B{}" in {
            val path = person('name) -|* 1 |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[*1]-(a1)")
          }
          "A{} -[*]- B{}" in {
            val path = person('name) -|* anyLength |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[*]-(a1)")
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person('name) -|* (deptHead, 1 to 3) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1..3]-(a1)")
          }
          "A{} -[C*1]- B{}" in {
            val path = person('name) -|* (deptHead, 1) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1]-(a1)")
          }
          "A{} -[C*]- B{}" in {
            val path = person('name) -|* deptHead |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*]-(a1)")
          }
          "A{} -[C{}*1..3]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1 to 3) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1..3]-(a1)")
          }
          "A{} -[C{}*1]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1]-(a1)")
          }
          "A{} -[C{}*]- B{}" in {
            val path = person('name) -|* deptHead('name) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*]-(a1)")
          }
          "A -[C*]- B" in {
            val path = person -|* deptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*]-(a1)")
          }
          "A -[C*1]- B" in {
            val path = person -|* (deptHead, 1) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1]-(a1)")
          }
          "A -[C*1..3]- B" in {
            val path = person -|* (deptHead, 1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1..3]-(a1)")
          }
          "A -[C{}*]- B" in {
            val path = person -|* deptHead('name) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*]-(a1)")
          }
          "A -[C{}*1]- B" in {
            val path = person -|* (deptHead('name), 1) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1]-(a1)")
          }
          "A -[C{}*1..3]- B" in {
            val path = person -|* (deptHead('name), 1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a2*1..3]-(a1)")
          }
        }
      }
    }
    "ProductClasses" should {
      val person       = randomize[Person]
      val dept         = randomize[Department]
      val deptHead     = randomize[HeadOfDepartment]
      val worksIn      = randomize[WorksIn]
      val locatedIn    = randomize[LocatedIn]
      val region       = randomize[Region]
      val anyPerson    = any[Person]
      val anyDept      = any[Department]
      val anyDeptHead  = anyRel[HeadOfDepartment]
      val anyWorksIn   = anyRel[WorksIn]
      val anyLocatedIn = anyRel[LocatedIn]
      val anyRegion    = any[Region]

      "when not in context" should {
        "provide query strings" should {
          "A -- B" in {
            val path = anyPerson -- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)--(a1:Department)")
          }
          "A -- B{}" in {
            val path = anyPerson -- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person)--(a1:Department {name: {a1_name}})",
              Map("a1_name" -> dept.name))
          }
          "A{} -- B" in {
            val path = person('name, 'age) -- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person {name: {a0_name},age: {a0_age}})--(a1:Department)",
              Map("a0_name" -> person.name,
                "a0_age"  -> person.age))
          }
          "A --> B" in {
            val path = anyPerson --> anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-->(a1:Department)")
          }
          "A --> B{}" in {
            val path = anyPerson --> dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person)-->(a1:Department {name: {a1_name}})",
              Map("a1_name" -> dept.name))
          }
          "A{} --> B" in {
            val path = person('id) --> anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-->(a1:Department)",
              Map("a0_id"   -> person.id))
          }
          "A <-- B" in {
            val path = anyPerson <-- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)<--(a1:Department)")
          }
          "A <-- B{}" in {
            val path = anyPerson <-- dept('name)
            path.toQuery() shouldBe DSLResult("(a0:Person)<--(a1:Department {name: {a1_name}})",
              Map("a1_name" -> dept.name))
          }
          "A{} <-- B" in {
            val path = person('id) <-- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})<--(a1:Department)",
              Map("a0_id"   -> person.id))
          }
          "A -[C]- B" in {
            val path = anyPerson -| anyDeptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT]-(a2:Department)")
          }
          "A -[C|D]- B" in {
            val path = anyPerson -| anyDeptHead | anyWorksIn |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT|:WORKS_IN]-(a2:Department)")
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| anyDeptHead |- dept('name)
            path
              .toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-[a1:HEAD_OF_DEPARTMENT]-(a2:Department {name: {a2_name}})",
              Map("a0_id" -> person.id, "a2_name" -> dept.name))
          }
          "A -[C {}]- B" in {
            val path = anyPerson -| deptHead('id) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]-(a2:Department)",
              Map("a1_id" -> deptHead.id))
          }
          "A -[C]-> B" in {
            val path = anyPerson -| anyDeptHead |-> anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT]->(a2:Department)")
          }
          "A{} -[C]-> B{}" in {
            val path = person('id) -| anyDeptHead |-> dept('name)
            path
              .toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})-[a1:HEAD_OF_DEPARTMENT]->(a2:Department {name: {a2_name}})",
              Map("a0_id" -> person.id, "a2_name" -> dept.name))
          }
          "A -[C {}]-> B" in {
            val path = anyPerson -| deptHead('id) |-> anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]->(a2:Department)",
              Map("a1_id" -> deptHead.id))
          }
          "A <-[C]- B" in {
            val path = anyPerson <-| anyDeptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)<-[a1:HEAD_OF_DEPARTMENT]-(a2:Department)")
          }
          "A{} <-[C]- B{}" in {
            val path = person('id) <-| anyDeptHead |- dept('name)
            path
              .toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id}})<-[a1:HEAD_OF_DEPARTMENT]-(a2:Department {name: {a2_name}})",
              Map("a0_id" -> person.id, "a2_name" -> dept.name))
          }
          "A <-[C {}]- B" in {
            val path = anyPerson <-| deptHead('id) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)<-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}]-(a2:Department)",
              Map("a1_id" -> deptHead.id))
          }
          "(anyA)-[R]-(anyB)-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |- anyDept -| locatedIn |- anyRegion
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department)-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region)",
              Map("a1_sinceDays" -> worksIn.sinceDays, "a3_area" -> locatedIn.area))
          }
          "(A)-[anyR]-(B)-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |- dept -| anyLocatedIn |- region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN]-(a4:Region {name: {a4_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name,
              "a4_name" -> region.name))
          }
          "(anyA)-[R]->(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept -| locatedIn |-> anyRegion
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department)-[a3:LOCATED_IN {area: {a3_area}}]->(a4:Region)",
              Map("a1_sinceDays" -> worksIn.sinceDays ,
                "a3_area" -> locatedIn.area))
          }
          "(A)-[anyR]->(B)-[anyR2]->(A2)" in {
            val path = person -| anyWorksIn |-> dept -| anyLocatedIn |-> region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN]->(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN]->(a4:Region {name: {a4_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name,
                "a4_name" -> region.name))
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion
            path.toQuery() shouldBe DSLResult("(a0:Person)<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department)<-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region)",
              Map("a1_sinceDays" -> worksIn.sinceDays ,
              "a3_area" -> locatedIn.area))
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN]-(a4:Region {name: {a4_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name,
                "a4_name" -> region.name))
          }
          "(anyA)<-[R]-(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept -| locatedIn |-> anyRegion
            path.toQuery() shouldBe DSLResult("(a0:Person)<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department)-[a3:LOCATED_IN {area: {a3_area}}]->(a4:Region)",
              Map("a1_sinceDays" -> worksIn.sinceDays ,
                "a3_area" -> locatedIn.area))
          }
          "(A)<-[anyR]-(B)-[anyR2]->(A2)" in {
            val path = person <-| anyWorksIn |- dept -| anyLocatedIn |-> region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN]-(a2:Department {id: {a2_id},name: {a2_name}})-[a3:LOCATED_IN]->(a4:Region {name: {a4_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name,
                "a4_name" -> region.name))
          }
          "(anyA)-[R]->(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept <-| locatedIn |- anyRegion
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]->(a2:Department)<-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region)",
              Map("a1_sinceDays" -> worksIn.sinceDays ,
                "a3_area" -> locatedIn.area))
          }
          "(A)-[anyR]->(B)<-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |-> dept <-| anyLocatedIn |- region
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:WORKS_IN]->(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN]-(a4:Region {name: {a4_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name,
                "a4_name" -> region.name))
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)-->(anyA)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion --> anyPerson
            path.toQuery() shouldBe DSLResult("(a0:Person)<-[a1:WORKS_IN {sinceDays: {a1_sinceDays}}]-(a2:Department)<-[a3:LOCATED_IN {area: {a3_area}}]-(a4:Region)-->(a0)",
              Map("a1_sinceDays" -> worksIn.sinceDays ,
                "a3_area" -> locatedIn.area))
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)-->(A)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region --> person
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})<-[a1:WORKS_IN]-(a2:Department {id: {a2_id},name: {a2_name}})<-[a3:LOCATED_IN]-(a4:Region {name: {a4_name}})-->(a0)",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name,
                "a4_name" -> region.name))
          }
          "A -[*1..3]- B" in {
            val path = anyPerson -|* (1 to 3) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[*1..3]-(a1:Department)")
          }
          "A -[*1]- B" in {
            val path = anyPerson -|* 1 |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[*1]-(a1:Department)")
          }
          "A -[*]- B" in {
            val path = anyPerson -|* anyLength |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[*]-(a1:Department)")
          }
          "A -[C*1..3]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1 to 3) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT*1..3]-(a2:Department)")
          }
          "A -[C{}*1..3]- B" in {
            val path = anyPerson -|* (deptHead('id), 1 to 3) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}*1..3]-(a2:Department)",
              Map("a1_id" -> deptHead.id))
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person -|* (anyDeptHead, 1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT*1..3]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C*1]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT*1]-(a2:Department)")
          }
          "A -[C{}*1]- B" in {
            val path = anyPerson -|* (deptHead, 1) |- anyDept
            path
              .toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*1]-(a2:Department)",
              Map("a1_id" -> deptHead.id,
                "a1_name" -> deptHead.name))
          }
          "A -[C{{}}*1]- B" in {
            val path = anyPerson -|* (deptHead('id), 1) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {id: {a1_id}}*1]-(a2:Department)",
              Map("a1_id" -> deptHead.id))
          }
          "A{} -[C*1]- B{}" in {
            val path = person -|* (anyDeptHead, 1) |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT*1]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_id" -> person.id,
                "a0_name" -> person.name,
                "a0_age" -> person.age,
                "a2_id" -> dept.id,
                "a2_name" -> dept.name))
          }
          "A -[C*]- B" in {
            val path = anyPerson -|* anyDeptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT*]-(a2:Department)")
          }
          "A -[C{}*]- B" in {
            val path = anyPerson -|* deptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {id: {a1_id},name: {a1_name}}*]-(a2:Department)",
              Map("a1_id" -> deptHead.id,"a1_name" -> deptHead.name))
          }
          "A -[C{{}}*]- B" in {
            val path = anyPerson -|* deptHead('name) |- anyDept
            path.toQuery() shouldBe DSLResult("(a0:Person)-[a1:HEAD_OF_DEPARTMENT {name: {a1_name}}*]-(a2:Department)",
              Map("a1_name" -> deptHead.name))
          }
          "A{} -[C*]- B{}" in {
            val path = person -|* anyDeptHead |- dept
            path.toQuery() shouldBe DSLResult("(a0:Person {id: {a0_id},name: {a0_name},age: {a0_age}})-[a1:HEAD_OF_DEPARTMENT*]-(a2:Department {id: {a2_id},name: {a2_name}})",
              Map("a0_name" -> person.name,
                "a0_id"   -> person.id,
                "a0_age"   -> person.age,
                "a2_id"   -> dept.id,
                "a2_name" -> dept.name))
          }
        }
      }
      "when in context" should {
        val context = new Context()
        context.add(person)
        context.add(dept)
        context.add(deptHead)
        context.add(worksIn)
        context.add(locatedIn)
        context.add(region)

        "provide query strings" should {
          "A -- B" in {
            val path = anyPerson -- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6:Person)--(a7:Department)")
          }
          "A -- B{}" in {
            val path = anyPerson -- dept
            path.toQuery(context) shouldBe DSLResult("(a6)--(a1)")
          }
          "A -- B{{}}" in {
            val path = anyPerson -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a6)--(a1)")
          }
          "A{} -- B" in {
            val path = person('name, 'age) -- anyDept
            path.toQuery(context) shouldBe DSLResult("(a0)--(a7)")
          }
          "A --> B" in {
            val path = anyPerson --> anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-->(a7)")
          }
          "A --> B{{}}" in {
            val path = anyPerson --> dept
            path.toQuery(context) shouldBe DSLResult("(a6)-->(a1)")
          }
          "A --> B{}" in {
            val path = anyPerson --> dept('name)
            path.toQuery(context) shouldBe DSLResult("(a6)-->(a1)")
          }
          "A{} --> B" in {
            val path = person('id) --> anyDept
            path.toQuery(context) shouldBe DSLResult("(a0)-->(a7)")
          }
          "A <-- B" in {
            val path = anyPerson <-- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)<--(a7)")
          }
          "A <-- B{}" in {
            val path = anyPerson <-- dept
            path.toQuery(context) shouldBe DSLResult("(a6)<--(a1)")
          }
          "A <-- B{{}}" in {
            val path = anyPerson <-- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a6)<--(a1)")
          }
          "A{} <-- B" in {
            val path = person('id) <-- anyDept
            path.toQuery(context) shouldBe DSLResult("(a0)<--(a7)")
          }
          "A -[C]- B" in {
            val path = anyPerson -| anyDeptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a8:HEAD_OF_DEPARTMENT]-(a7)")
          }
          "A -[C|D]- B" in {
            val path = anyPerson -| anyDeptHead | anyWorksIn |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a9:HEAD_OF_DEPARTMENT|:WORKS_IN]-(a7)")
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| anyDeptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a8]-(a1)")
          }
          "A -[C {{}}]- B" in {
            val path = anyPerson -| deptHead('id) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2]-(a7)")
          }
          "A -[C {}]- B" in {
            val path = anyPerson -| deptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2]-(a7)")
          }
          "A -[C]-> B" in {
            val path = anyPerson -| anyDeptHead |-> anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a8]->(a7)")
          }
          "A{} -[C]-> B{}" in {
            val path = person('id) -| anyDeptHead |-> dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a8]->(a1)")
          }
          "A{{}} -[C]-> B{}" in {
            val path = person -| anyDeptHead |-> dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)-[a8]->(a1)")
          }
          "A -[C {}]-> B" in {
            val path = anyPerson -| deptHead('id) |-> anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2]->(a7)")
          }
          "A <-[C]- B" in {
            val path = anyPerson <-| anyDeptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)<-[a8]-(a7)")
          }
          "A{{}} <-[C]- B{}" in {
            val path = person('id) <-| anyDeptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a8]-(a1)")
          }
          "A{} <-[C]- B{{}}" in {
            val path = person <-| anyDeptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a8]-(a1)")
          }
          "A <-[C {}]- B" in {
            val path = anyPerson <-| deptHead('id) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)<-[a2]-(a7)")
          }
          "(anyA)-[R]-(anyB)-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |- anyDept -| locatedIn |- anyRegion
            path.toQuery(context) shouldBe DSLResult("(a6)-[a3]-(a7)-[a4]-(a10:Region)")
          }
          "(A)-[anyR]-(B)-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |- dept -| anyLocatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(a0)-[a11:WORKS_IN]-(a1)-[a12:LOCATED_IN]-(a5)")
          }
          "(anyA)-[R]->(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept -| locatedIn |-> anyRegion
            path.toQuery(context) shouldBe DSLResult("(a6)-[a3]->(a7)-[a4]->(a10)")
          }
          "(A)-[anyR]->(B)-[anyR2]->(A2)" in {
            val path = person -| anyWorksIn |-> dept -| anyLocatedIn |-> region
            path.toQuery(context) shouldBe DSLResult("(a0)-[a11]->(a1)-[a12]->(a5)")
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion
            path.toQuery(context) shouldBe DSLResult("(a6)<-[a3]-(a7)<-[a4]-(a10)")
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a11]-(a1)<-[a12]-(a5)")
          }
          "(anyA)<-[R]-(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept -| locatedIn |-> anyRegion
            path.toQuery(context) shouldBe DSLResult("(a6)<-[a3]-(a7)-[a4]->(a10)")
          }
          "(A)<-[anyR]-(B)-[anyR2]->(A2)" in {
            val path = person <-| anyWorksIn |- dept -| anyLocatedIn |-> region
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a11]-(a1)-[a12]->(a5)")
          }
          "(anyA)-[R]->(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept <-| locatedIn |- anyRegion
            path.toQuery(context) shouldBe DSLResult("(a6)-[a3]->(a7)<-[a4]-(a10)")
          }
          "(A)-[anyR]->(B)<-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |-> dept <-| anyLocatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(a0)-[a11]->(a1)<-[a12]-(a5)")
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)-->(anyA)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion --> anyPerson
            path.toQuery(context) shouldBe DSLResult("(a6)<-[a3]-(a7)<-[a4]-(a10)-->(a6)")
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)-->(A)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region --> person
            path.toQuery(context) shouldBe DSLResult("(a0)<-[a11]-(a1)<-[a12]-(a5)-->(a0)")
          }
          "A -[*1..3]- B" in {
            val path = anyPerson -|* (1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[*1..3]-(a7)")
          }
          "A -[*1]- B" in {
            val path = anyPerson -|* 1 |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[*1]-(a7)")
          }
          "A -[*]- B" in {
            val path = anyPerson -|* (anyLength) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[*]-(a7)")
          }
          "A -[C*1..3]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a8*1..3]-(a7)")
          }
          "A -[C{}*1..3]- B" in {
            val path = anyPerson -|* (deptHead, 1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2*1..3]-(a7)")
          }
          "A -[C{{}}*1..3]- B" in {
            val path = anyPerson -|* (deptHead('id), 1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2*1..3]-(a7)")
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person -|* (anyDeptHead, 1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a8*1..3]-(a1)")
          }
          "A -[C*1]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a8*1]-(a7)")
          }
          "A -[C{}*1]- B" in {
            val path = anyPerson -|* (deptHead, 1) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2*1]-(a7)")
          }
          "A -[C{{}}*1]- B" in {
            val path = anyPerson -|* (deptHead('id), 1) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2*1]-(a7)")
          }
          "A{} -[C*1]- B{}" in {
            val path = person -|* (anyDeptHead, 1) |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a8*1]-(a1)")
          }
          "A -[C*]- B" in {
            val path = anyPerson -|* anyDeptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a8*]-(a7)")
          }
          "A -[C{}*]- B" in {
            val path = anyPerson -|* deptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2*]-(a7)")
          }
          "A -[C{{}}*]- B" in {
            val path = anyPerson -|* deptHead('name) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(a6)-[a2*]-(a7)")
          }
          "A{} -[C*]- B{}" in {
            val path = person -|* anyDeptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(a0)-[a8*]-(a1)")
          }
        }
      }
    }
  }
}
