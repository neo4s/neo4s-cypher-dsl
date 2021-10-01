package neo4s.cypher.dsl.syntax

import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses._
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
class PatternsSpec extends AnyWordSpec with should.Matchers {

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
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})--(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -- B{}" in {
            val path = person -- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})--(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_name" -> dept.name))
          }
          "A{} -- B" in {
            val path = person('name, 'age) -- dept
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name,age: $person_age})--(department:Department {id: $department_id,name: $department_name})",
              Map("person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} -- B{}" in {
            val path = person('age) -- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {age: $person_age})--(department:Department {name: $department_name})",
              Map("person_age"  -> person.age,
                "department_name" -> dept.name))
          }
          "A --> B" in {
            val path = person --> dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-->(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A --> B{}" in {
            val path = person --> dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-->(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_name" -> dept.name))
          }
          "A{} --> B" in {
            val path = person('id) --> dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-->(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} --> B{}" in {
            val path = person('id) --> dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-->(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "department_name" -> dept.name))
          }
          "A <-- B" in {
            val path = person <-- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<--(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A <-- B{}" in {
            val path = person <-- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<--(department:Department {id: $department_id})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id))
          }
          "A{} <-- B" in {
            val path = person('id) <-- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})<--(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} <-- B{}" in {
            val path = person('id) <-- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})<--(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "department_name" -> dept.name))
          }
          "A -[C]- B" in {
            val path = person -| deptHead |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C|D]- B" in {
            val path = person -| deptHead | worksIn |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}|:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "head_of_department_id" -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| deptHead |- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]-(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_name" -> dept.name))
          }
          "A -[C {}]- B" in {
            val path = person -| deptHead('id) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} -[C {}]- B{}" in {
            val path = person('id) -| deptHead('id) |- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]-(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "head_of_department_id"   -> deptHead.id,
                "department_name" -> dept.name))
          }
          "A -[C]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]->(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
              "person_name" -> person.name,
              "person_age"  -> person.age,
              "head_of_department_id"   -> deptHead.id,
              "head_of_department_name" -> deptHead.name,
              "department_id"   -> dept.id,
              "department_name" -> dept.name))
          }
          "A{} -[C]-> B{}" in {
            val path = person('name) -| deptHead |-> dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]->(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id))
          }
          "A -[C {}]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]->(department:Department {id: $department_id,name: $department_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "head_of_department_id" -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} -[C {}]-> B{}" in {
            val path = person('id) -| deptHead('id) |-> dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]->(department:Department {name: $department_name})",
              Map("person_id" -> person.id,
                "head_of_department_id" -> deptHead.id,
                "department_name" -> dept.name))
          }
          "A <-[C]- B" in {
            val path = person <-| deptHead |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} <-[C]- B{}" in {
            val path = person('id) <-| deptHead |- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})<-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]-(department:Department {name: $department_name})",
              Map("person_id"   -> person.id,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_name" -> dept.name))
          }
          "A <-[C {}]- B" in {
            val path = person <-| deptHead('id) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} <-[C {}]- B{}" in {
            val path = person('id) <-| deptHead('id) |- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})<-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]-(department:Department {name: $department_name})",
              Map("person_id" -> person.id,
                "head_of_department_id" -> deptHead.id,
                "department_name" -> dept.name))
          }
          "(A)-[R]-(B)-[R2]-(A2)" in {
            val path = person -| worksIn |- dept -| locatedIn |- region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department {id: $department_id,name: $department_name})-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "located_in_area" -> locatedIn.area,
                "region_name" -> region.name,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
          }
          "(A)-[R]->(B)-[R2]->(A2)" in {
            val path = person -| worksIn |-> dept -| locatedIn |-> region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department {id: $department_id,name: $department_name})-[located_in:LOCATED_IN {area: $located_in_area}]->(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "located_in_area" -> locatedIn.area,
                "region_name" -> region.name,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
          }
          "(A)<-[R]-(B)<-[R2]-(A2)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department {id: $department_id,name: $department_name})<-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region {name: $region_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "located_in_area"   -> locatedIn.area,
                "region_name" -> region.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "(A)<-[R]-(B)-[R2]->(A2)" in {
            val path = person <-| worksIn |- dept -| deptHead |-> person
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department {id: $department_id,name: $department_name})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]->(person)",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "(A)-[R]->(B)<-[R2]-(A2)" in {
            val path = person -| deptHead |-> dept <-| worksIn |- person
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}]->(department:Department {id: $department_id,name: $department_name})<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(person)",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "(A)<-[R]-(B)<-[R2]-(A2)-->(C)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region --> person
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department {id: $department_id,name: $department_name})<-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region {name: $region_name})-->(person)",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "works_in_sinceDays" -> worksIn.sinceDays,
                "located_in_area"   -> locatedIn.area,
                "region_name" -> region.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }

          "A -[*1..3]- B" in {
            val path = person -|* (1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[*1..3]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[*1]- B" in {
            val path = person -|* 1 |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[*1]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[*]- B" in {
            val path = person -|* anyLength |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[*]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C*]- B" in {
            val path = person -|* deptHead |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C*1]- B" in {
            val path = person -|* (deptHead, 1) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*1]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C*1..3]- B" in {
            val path = person -|* (deptHead, 1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*1..3]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C{}*]- B" in {
            val path = person -|* deptHead('name) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C{}*1]- B" in {
            val path = person -|* (deptHead('name), 1) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*1]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C{}*1..3]- B" in {
            val path = person -|* (deptHead('name), 1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*1..3]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id"   -> person.id,
                "person_name" -> person.name,
                "person_age"  -> person.age,
                "head_of_department_name" -> deptHead.name,
                "department_id"   -> dept.id,
                "department_name" -> dept.name))
          }
          "A{} -[*1..3]- B{}" in {
            val path = person('name) -|* (1 to 3) |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[*1..3]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "department_id"   -> dept.id))
          }
          "A{} -[*1]- B{}" in {
            val path = person('name) -|* 1 |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[*1]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "department_id"   -> dept.id))
          }
          "A{} -[*]- B{}" in {
            val path = person('name) -|* anyLength |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[*]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "department_id"   -> dept.id))
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person('name) -|* (deptHead, 1 to 3) |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*1..3]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_id"   -> deptHead.id,
              "head_of_department_name" -> deptHead.name,
              "department_id" -> dept.id))
          }
          "A{} -[C*1]- B{}" in {
            val path = person('name) -|* (deptHead, 1) |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*1]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id))
          }
          "A{} -[C*]- B{}" in {
            val path = person('name) -|* deptHead |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_id"   -> deptHead.id,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id))
          }
          "A{} -[C{}*1..3]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1 to 3) |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*1..3]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id))
          }
          "A{} -[C{}*1]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1) |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*1]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id))
          }
          "A{} -[C{}*]- B{}" in {
            val path = person('name) -|* deptHead('name) |- dept('id)
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name})-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*]-(department:Department {id: $department_id})",
              Map("person_name" -> person.name,
                "head_of_department_name" -> deptHead.name,
                "department_id" -> dept.id))
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
            path.toQuery(context) shouldBe DSLResult("(person)--(department)")
          }
          "A -- B{}" in {
            val path = person -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)--(department)")
          }
          "A{} -- B" in {
            val path = person('name) -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)--(department)")
          }
          "A{} -- B{}" in {
            val path = person('name) -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)--(department)")
          }
          "A --> B" in {
            val path = person --> dept
            path.toQuery(context) shouldBe DSLResult("(person)-->(department)")
          }
          "A --> B{}" in {
            val path = person --> dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-->(department)")
          }
          "A{} --> B" in {
            val path = person('age) --> dept
            path.toQuery(context) shouldBe DSLResult("(person)-->(department)")
          }
          "A{} --> B{}" in {
            val path = person('id) --> dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-->(department)")
          }
          "A <-- B" in {
            val path = person <-- dept
            path.toQuery(context) shouldBe DSLResult("(person)<--(department)")
          }
          "A <-- B{}" in {
            val path = person <-- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)<--(department)")
          }
          "A{} <-- B" in {
            val path = person('name) <-- dept
            path.toQuery(context) shouldBe DSLResult("(person)<--(department)")
          }
          "A{} <-- B{}" in {
            val path = person('id) <-- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)<--(department)")
          }
          "A -[C]- B" in {
            val path = person -| deptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]-(department)")
          }
          "A -[C|D]- B" in {
            val path = person -| deptHead | worksIn |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]-(department)")
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| deptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]-(department)")
          }
          "A -[C {}]- B" in {
            val path = person -| deptHead('id) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]-(department)")
          }
          "A{} -[C {}]- B{}" in {
            val path = person('id) -| deptHead('id) |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]-(department)")
          }
          "A -[C]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]->(department)")
          }
          "A{} -[C]-> B{}" in {
            val path = person('name) -| deptHead |-> dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]->(department)")
          }
          "A -[C {}]-> B" in {
            val path = person -| deptHead |-> dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]->(department)")
          }
          "A{} -[C {}]-> B{}" in {
            val path = person('id) -| deptHead('id) |-> dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]->(department)")
          }
          "A <-[C]- B" in {
            val path = person <-| deptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(person)<-[head_of_department]-(department)")
          }
          "A{} <-[C]- B{}" in {
            val path = person('id) <-| deptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)<-[head_of_department]-(department)")
          }
          "A <-[C {}]- B" in {
            val path = person <-| deptHead('id) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)<-[head_of_department]-(department)")
          }
          "A{} <-[C {}]- B{}" in {
            val path = person('id) <-| deptHead('id) |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)<-[head_of_department]-(department)")
          }
          "(A)-[R]-(B)-[R2]-(A2)" in {
            val path = person -| worksIn |- dept -| locatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(person)-[works_in]-(department)-[located_in]-(region)")
          }
          "(A)-[R]->(B)-[R2]->(A2)" in {
            val path = person -| worksIn |-> dept -| locatedIn |-> region
            path.toQuery(context) shouldBe DSLResult("(person)-[works_in]->(department)-[located_in]->(region)")
          }
          "(A)<-[R]-(B)<-[R2]-(A2)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(person)<-[works_in]-(department)<-[located_in]-(region)")
          }
          "(A)<-[R]-(B)-[R2]->(A2)" in {
            val path = person <-| worksIn |- dept -| deptHead |-> person
            path.toQuery(context) shouldBe DSLResult("(person)<-[works_in]-(department)-[head_of_department]->(person)")
          }
          "(A)-[R]->(B)<-[R2]-(A2)" in {
            val path = person -| deptHead |-> dept <-| worksIn |- person
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department]->(department)<-[works_in]-(person)")
          }
          "(A)<-[R]-(B)<-[R2]-(A2)-->(C)" in {
            val path = person <-| worksIn |- dept <-| locatedIn |- region --> person
            path.toQuery(context) shouldBe DSLResult("(person)<-[works_in]-(department)<-[located_in]-(region)-->(person)")
          }
          "A -[*1..3]- B" in {
            val path = person -|* (1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[*1..3]-(department)")
          }
          "A -[*1]- B" in {
            val path = person -|* 1 |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[*1]-(department)")
          }
          "A -[*]- B" in {
            val path = person -|* anyLength |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[*]-(department)")
          }
          "A{} -[*1..3]- B{}" in {
            val path = person('name) -|* (1 to 3) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[*1..3]-(department)")
          }
          "A{} -[*1]- B{}" in {
            val path = person('name) -|* 1 |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[*1]-(department)")
          }
          "A{} -[*]- B{}" in {
            val path = person('name) -|* anyLength |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[*]-(department)")
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person('name) -|* (deptHead, 1 to 3) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1..3]-(department)")
          }
          "A{} -[C*1]- B{}" in {
            val path = person('name) -|* (deptHead, 1) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1]-(department)")
          }
          "A{} -[C*]- B{}" in {
            val path = person('name) -|* deptHead |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*]-(department)")
          }
          "A{} -[C{}*1..3]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1 to 3) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1..3]-(department)")
          }
          "A{} -[C{}*1]- B{}" in {
            val path = person('name) -|* (deptHead('name), 1) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1]-(department)")
          }
          "A{} -[C{}*]- B{}" in {
            val path = person('name) -|* deptHead('name) |- dept('id)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*]-(department)")
          }
          "A -[C*]- B" in {
            val path = person -|* deptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*]-(department)")
          }
          "A -[C*1]- B" in {
            val path = person -|* (deptHead, 1) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1]-(department)")
          }
          "A -[C*1..3]- B" in {
            val path = person -|* (deptHead, 1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1..3]-(department)")
          }
          "A -[C{}*]- B" in {
            val path = person -|* deptHead('name) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*]-(department)")
          }
          "A -[C{}*1]- B" in {
            val path = person -|* (deptHead('name), 1) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1]-(department)")
          }
          "A -[C{}*1..3]- B" in {
            val path = person -|* (deptHead('name), 1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department*1..3]-(department)")
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
            path.toQuery() shouldBe DSLResult("(person:Person)--(department:Department)")
          }
          "A -- B{}" in {
            val path = anyPerson -- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person)--(department:Department {name: $department_name})",
              Map("department_name" -> dept.name))
          }
          "A{} -- B" in {
            val path = person('name, 'age) -- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person {name: $person_name,age: $person_age})--(department:Department)",
              Map("person_name" -> person.name,
                "person_age"  -> person.age))
          }
          "A --> B" in {
            val path = anyPerson --> anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-->(department:Department)")
          }
          "A --> B{}" in {
            val path = anyPerson --> dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person)-->(department:Department {name: $department_name})",
              Map("department_name" -> dept.name))
          }
          "A{} --> B" in {
            val path = person('id) --> anyDept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-->(department:Department)",
              Map("person_id"   -> person.id))
          }
          "A <-- B" in {
            val path = anyPerson <-- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)<--(department:Department)")
          }
          "A <-- B{}" in {
            val path = anyPerson <-- dept('name)
            path.toQuery() shouldBe DSLResult("(person:Person)<--(department:Department {name: $department_name})",
              Map("department_name" -> dept.name))
          }
          "A{} <-- B" in {
            val path = person('id) <-- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id})<--(department:Department)",
              Map("person_id"   -> person.id))
          }
          "A -[C]- B" in {
            val path = anyPerson -| anyDeptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT]-(department:Department)")
          }
          "A -[C|D]- B" in {
            val path = anyPerson -| anyDeptHead | anyWorksIn |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[rel:HEAD_OF_DEPARTMENT|:WORKS_IN]-(department:Department)")
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| anyDeptHead |- dept('name)
            path
              .toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-[head_of_department:HEAD_OF_DEPARTMENT]-(department:Department {name: $department_name})",
              Map("person_id" -> person.id, "department_name" -> dept.name))
          }
          "A -[C {}]- B" in {
            val path = anyPerson -| deptHead('id) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]-(department:Department)",
              Map("head_of_department_id" -> deptHead.id))
          }
          "A -[C]-> B" in {
            val path = anyPerson -| anyDeptHead |-> anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT]->(department:Department)")
          }
          "A{} -[C]-> B{}" in {
            val path = person('id) -| anyDeptHead |-> dept('name)
            path
              .toQuery() shouldBe DSLResult("(person:Person {id: $person_id})-[head_of_department:HEAD_OF_DEPARTMENT]->(department:Department {name: $department_name})",
              Map("person_id" -> person.id, "department_name" -> dept.name))
          }
          "A -[C {}]-> B" in {
            val path = anyPerson -| deptHead('id) |-> anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]->(department:Department)",
              Map("head_of_department_id" -> deptHead.id))
          }
          "A <-[C]- B" in {
            val path = anyPerson <-| anyDeptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)<-[head_of_department:HEAD_OF_DEPARTMENT]-(department:Department)")
          }
          "A{} <-[C]- B{}" in {
            val path = person('id) <-| anyDeptHead |- dept('name)
            path
              .toQuery() shouldBe DSLResult("(person:Person {id: $person_id})<-[head_of_department:HEAD_OF_DEPARTMENT]-(department:Department {name: $department_name})",
              Map("person_id" -> person.id, "department_name" -> dept.name))
          }
          "A <-[C {}]- B" in {
            val path = anyPerson <-| deptHead('id) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)<-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}]-(department:Department)",
              Map("head_of_department_id" -> deptHead.id))
          }
          "(anyA)-[R]-(anyB)-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |- anyDept -| locatedIn |- anyRegion
            path.toQuery() shouldBe DSLResult("(person:Person)-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department)-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region)",
              Map("works_in_sinceDays" -> worksIn.sinceDays, "located_in_area" -> locatedIn.area))
          }
          "(A)-[anyR]-(B)-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |- dept -| anyLocatedIn |- region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN]-(department:Department {id: $department_id,name: $department_name})-[located_in:LOCATED_IN]-(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name,
                "region_name" -> region.name))
          }
          "(anyA)-[R]->(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept -| locatedIn |-> anyRegion
            path.toQuery() shouldBe DSLResult("(person:Person)-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department)-[located_in:LOCATED_IN {area: $located_in_area}]->(region:Region)",
              Map("works_in_sinceDays" -> worksIn.sinceDays ,
                "located_in_area" -> locatedIn.area))
          }
          "(A)-[anyR]->(B)-[anyR2]->(A2)" in {
            val path = person -| anyWorksIn |-> dept -| anyLocatedIn |-> region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN]->(department:Department {id: $department_id,name: $department_name})-[located_in:LOCATED_IN]->(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name,
                "region_name" -> region.name))
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion
            path.toQuery() shouldBe DSLResult("(person:Person)<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department)<-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region)",
              Map("works_in_sinceDays" -> worksIn.sinceDays ,
              "located_in_area" -> locatedIn.area))
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[works_in:WORKS_IN]-(department:Department {id: $department_id,name: $department_name})<-[located_in:LOCATED_IN]-(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name,
                "region_name" -> region.name))
          }
          "(anyA)<-[R]-(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept -| locatedIn |-> anyRegion
            path.toQuery() shouldBe DSLResult("(person:Person)<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department)-[located_in:LOCATED_IN {area: $located_in_area}]->(region:Region)",
              Map("works_in_sinceDays" -> worksIn.sinceDays ,
                "located_in_area" -> locatedIn.area))
          }
          "(A)<-[anyR]-(B)-[anyR2]->(A2)" in {
            val path = person <-| anyWorksIn |- dept -| anyLocatedIn |-> region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[works_in:WORKS_IN]-(department:Department {id: $department_id,name: $department_name})-[located_in:LOCATED_IN]->(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name,
                "region_name" -> region.name))
          }
          "(anyA)-[R]->(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept <-| locatedIn |- anyRegion
            path.toQuery() shouldBe DSLResult("(person:Person)-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department)<-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region)",
              Map("works_in_sinceDays" -> worksIn.sinceDays ,
                "located_in_area" -> locatedIn.area))
          }
          "(A)-[anyR]->(B)<-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |-> dept <-| anyLocatedIn |- region
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN]->(department:Department {id: $department_id,name: $department_name})<-[located_in:LOCATED_IN]-(region:Region {name: $region_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name,
                "region_name" -> region.name))
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)-->(anyA)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion --> anyPerson
            path.toQuery() shouldBe DSLResult("(person:Person)<-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]-(department:Department)<-[located_in:LOCATED_IN {area: $located_in_area}]-(region:Region)-->(person)",
              Map("works_in_sinceDays" -> worksIn.sinceDays ,
                "located_in_area" -> locatedIn.area))
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)-->(A)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region --> person
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})<-[works_in:WORKS_IN]-(department:Department {id: $department_id,name: $department_name})<-[located_in:LOCATED_IN]-(region:Region {name: $region_name})-->(person)",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name,
                "region_name" -> region.name))
          }
          "A -[*1..3]- B" in {
            val path = anyPerson -|* (1 to 3) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[*1..3]-(department:Department)")
          }
          "A -[*1]- B" in {
            val path = anyPerson -|* 1 |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[*1]-(department:Department)")
          }
          "A -[*]- B" in {
            val path = anyPerson -|* anyLength |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[*]-(department:Department)")
          }
          "A -[C*1..3]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1 to 3) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT*1..3]-(department:Department)")
          }
          "A -[C{}*1..3]- B" in {
            val path = anyPerson -|* (deptHead('id), 1 to 3) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}*1..3]-(department:Department)",
              Map("head_of_department_id" -> deptHead.id))
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person -|* (anyDeptHead, 1 to 3) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT*1..3]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C*1]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT*1]-(department:Department)")
          }
          "A -[C{}*1]- B" in {
            val path = anyPerson -|* (deptHead, 1) |- anyDept
            path
              .toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*1]-(department:Department)",
              Map("head_of_department_id" -> deptHead.id,
                "head_of_department_name" -> deptHead.name))
          }
          "A -[C{{}}*1]- B" in {
            val path = anyPerson -|* (deptHead('id), 1) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id}*1]-(department:Department)",
              Map("head_of_department_id" -> deptHead.id))
          }
          "A{} -[C*1]- B{}" in {
            val path = person -|* (anyDeptHead, 1) |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT*1]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_id" -> person.id,
                "person_name" -> person.name,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
          }
          "A -[C*]- B" in {
            val path = anyPerson -|* anyDeptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT*]-(department:Department)")
          }
          "A -[C{}*]- B" in {
            val path = anyPerson -|* deptHead |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {id: $head_of_department_id,name: $head_of_department_name}*]-(department:Department)",
              Map("head_of_department_id" -> deptHead.id,"head_of_department_name" -> deptHead.name))
          }
          "A -[C{{}}*]- B" in {
            val path = anyPerson -|* deptHead('name) |- anyDept
            path.toQuery() shouldBe DSLResult("(person:Person)-[head_of_department:HEAD_OF_DEPARTMENT {name: $head_of_department_name}*]-(department:Department)",
              Map("head_of_department_name" -> deptHead.name))
          }
          "A{} -[C*]- B{}" in {
            val path = person -|* anyDeptHead |- dept
            path.toQuery() shouldBe DSLResult("(person:Person {id: $person_id,name: $person_name,age: $person_age})-[head_of_department:HEAD_OF_DEPARTMENT*]-(department:Department {id: $department_id,name: $department_name})",
              Map("person_name" -> person.name,
                "person_id" -> person.id,
                "person_age" -> person.age,
                "department_id" -> dept.id,
                "department_name" -> dept.name))
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
            path.toQuery(context) shouldBe DSLResult("(person_1:Person)--(department_1:Department)")
          }
          "A -- B{}" in {
            val path = anyPerson -- dept
            path.toQuery(context) shouldBe DSLResult("(person_1)--(department)")
          }
          "A -- B{{}}" in {
            val path = anyPerson -- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person_1)--(department)")
          }
          "A{} -- B" in {
            val path = person('name, 'age) -- anyDept
            path.toQuery(context) shouldBe DSLResult("(person)--(department_1)")
          }
          "A --> B" in {
            val path = anyPerson --> anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-->(department_1)")
          }
          "A --> B{{}}" in {
            val path = anyPerson --> dept
            path.toQuery(context) shouldBe DSLResult("(person_1)-->(department)")
          }
          "A --> B{}" in {
            val path = anyPerson --> dept('name)
            path.toQuery(context) shouldBe DSLResult("(person_1)-->(department)")
          }
          "A{} --> B" in {
            val path = person('id) --> anyDept
            path.toQuery(context) shouldBe DSLResult("(person)-->(department_1)")
          }
          "A <-- B" in {
            val path = anyPerson <-- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)<--(department_1)")
          }
          "A <-- B{}" in {
            val path = anyPerson <-- dept
            path.toQuery(context) shouldBe DSLResult("(person_1)<--(department)")
          }
          "A <-- B{{}}" in {
            val path = anyPerson <-- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person_1)<--(department)")
          }
          "A{} <-- B" in {
            val path = person('id) <-- anyDept
            path.toQuery(context) shouldBe DSLResult("(person)<--(department_1)")
          }
          "A -[C]- B" in {
            val path = anyPerson -| anyDeptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department_1:HEAD_OF_DEPARTMENT]-(department_1)")
          }
          "A -[C|D]- B" in {
            val path = anyPerson -| anyDeptHead | anyWorksIn |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[rel:HEAD_OF_DEPARTMENT|:WORKS_IN]-(department_1)")
          }
          "A{} -[C]- B{}" in {
            val path = person('id) -| anyDeptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department_1]-(department)")
          }
          "A -[C {{}}]- B" in {
            val path = anyPerson -| deptHead('id) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department]-(department_1)")
          }
          "A -[C {}]- B" in {
            val path = anyPerson -| deptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department]-(department_1)")
          }
          "A -[C]-> B" in {
            val path = anyPerson -| anyDeptHead |-> anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department_1]->(department_1)")
          }
          "A{} -[C]-> B{}" in {
            val path = person('id) -| anyDeptHead |-> dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department_1]->(department)")
          }
          "A{{}} -[C]-> B{}" in {
            val path = person -| anyDeptHead |-> dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department_1]->(department)")
          }
          "A -[C {}]-> B" in {
            val path = anyPerson -| deptHead('id) |-> anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department]->(department_1)")
          }
          "A <-[C]- B" in {
            val path = anyPerson <-| anyDeptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)<-[head_of_department_1]-(department_1)")
          }
          "A{{}} <-[C]- B{}" in {
            val path = person('id) <-| anyDeptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(person)<-[head_of_department_1]-(department)")
          }
          "A{} <-[C]- B{{}}" in {
            val path = person <-| anyDeptHead |- dept('name)
            path.toQuery(context) shouldBe DSLResult("(person)<-[head_of_department_1]-(department)")
          }
          "A <-[C {}]- B" in {
            val path = anyPerson <-| deptHead('id) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)<-[head_of_department]-(department_1)")
          }
          "(anyA)-[R]-(anyB)-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |- anyDept -| locatedIn |- anyRegion
            path.toQuery(context) shouldBe DSLResult("(person_1)-[works_in]-(department_1)-[located_in]-(region_1:Region)")
          }
          "(A)-[anyR]-(B)-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |- dept -| anyLocatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(person)-[works_in_1:WORKS_IN]-(department)-[located_in_1:LOCATED_IN]-(region)")
          }
          "(anyA)-[R]->(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept -| locatedIn |-> anyRegion
            path.toQuery(context) shouldBe DSLResult("(person_1)-[works_in]->(department_1)-[located_in]->(region_1)")
          }
          "(A)-[anyR]->(B)-[anyR2]->(A2)" in {
            val path = person -| anyWorksIn |-> dept -| anyLocatedIn |-> region
            path.toQuery(context) shouldBe DSLResult("(person)-[works_in_1]->(department)-[located_in_1]->(region)")
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion
            path.toQuery(context) shouldBe DSLResult("(person_1)<-[works_in]-(department_1)<-[located_in]-(region_1)")
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(person)<-[works_in_1]-(department)<-[located_in_1]-(region)")
          }
          "(anyA)<-[R]-(anyB)-[R2]->(anyA2)" in {
            val path = anyPerson <-| worksIn |- anyDept -| locatedIn |-> anyRegion
            path.toQuery(context) shouldBe DSLResult("(person_1)<-[works_in]-(department_1)-[located_in]->(region_1)")
          }
          "(A)<-[anyR]-(B)-[anyR2]->(A2)" in {
            val path = person <-| anyWorksIn |- dept -| anyLocatedIn |-> region
            path.toQuery(context) shouldBe DSLResult("(person)<-[works_in_1]-(department)-[located_in_1]->(region)")
          }
          "(anyA)-[R]->(anyB)<-[R2]-(anyA2)" in {
            val path = anyPerson -| worksIn |-> anyDept <-| locatedIn |- anyRegion
            path.toQuery(context) shouldBe DSLResult("(person_1)-[works_in]->(department_1)<-[located_in]-(region_1)")
          }
          "(A)-[anyR]->(B)<-[anyR2]-(A2)" in {
            val path = person -| anyWorksIn |-> dept <-| anyLocatedIn |- region
            path.toQuery(context) shouldBe DSLResult("(person)-[works_in_1]->(department)<-[located_in_1]-(region)")
          }
          "(anyA)<-[R]-(anyB)<-[R2]-(anyA2)-->(anyA)" in {
            val path = anyPerson <-| worksIn |- anyDept <-| locatedIn |- anyRegion --> anyPerson
            path.toQuery(context) shouldBe DSLResult("(person_1)<-[works_in]-(department_1)<-[located_in]-(region_1)-->(person_1)")
          }
          "(A)<-[anyR]-(B)<-[anyR2]-(A2)-->(A)" in {
            val path = person <-| anyWorksIn |- dept <-| anyLocatedIn |- region --> person
            path.toQuery(context) shouldBe DSLResult("(person)<-[works_in_1]-(department)<-[located_in_1]-(region)-->(person)")
          }
          "A -[*1..3]- B" in {
            val path = anyPerson -|* (1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[*1..3]-(department_1)")
          }
          "A -[*1]- B" in {
            val path = anyPerson -|* 1 |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[*1]-(department_1)")
          }
          "A -[*]- B" in {
            val path = anyPerson -|* anyLength |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[*]-(department_1)")
          }
          "A -[C*1..3]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department_1*1..3]-(department_1)")
          }
          "A -[C{}*1..3]- B" in {
            val path = anyPerson -|* (deptHead, 1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department*1..3]-(department_1)")
          }
          "A -[C{{}}*1..3]- B" in {
            val path = anyPerson -|* (deptHead('id), 1 to 3) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department*1..3]-(department_1)")
          }
          "A{} -[C*1..3]- B{}" in {
            val path = person -|* (anyDeptHead, 1 to 3) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department_1*1..3]-(department)")
          }
          "A -[C*1]- B" in {
            val path = anyPerson -|* (anyDeptHead, 1) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department_1*1]-(department_1)")
          }
          "A -[C{}*1]- B" in {
            val path = anyPerson -|* (deptHead, 1) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department*1]-(department_1)")
          }
          "A -[C{{}}*1]- B" in {
            val path = anyPerson -|* (deptHead('id), 1) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department*1]-(department_1)")
          }
          "A{} -[C*1]- B{}" in {
            val path = person -|* (anyDeptHead, 1) |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department_1*1]-(department)")
          }
          "A -[C*]- B" in {
            val path = anyPerson -|* anyDeptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department_1*]-(department_1)")
          }
          "A -[C{}*]- B" in {
            val path = anyPerson -|* deptHead |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department*]-(department_1)")
          }
          "A -[C{{}}*]- B" in {
            val path = anyPerson -|* deptHead('name) |- anyDept
            path.toQuery(context) shouldBe DSLResult("(person_1)-[head_of_department*]-(department_1)")
          }
          "A{} -[C*]- B{}" in {
            val path = person -|* anyDeptHead |- dept
            path.toQuery(context) shouldBe DSLResult("(person)-[head_of_department_1*]-(department)")
          }
        }
      }
    }
  }
}
