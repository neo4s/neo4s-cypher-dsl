package neo4s.cypher.dsl.syntax

import neo4s.cypher.dsl.operators.Distinct
import neo4s.cypher.dsl.syntax.Wherewithal.NodeWhereSupport
import neo4s.cypher.dsl.syntax.patterns._
import neo4s.cypher.dsl.utils.Random.randomize
import neo4s.cypher.dsl.utils.TestClasses.ImplicitCache._
import neo4s.cypher.dsl.utils.TestClasses.{Department, Person, WorksIn}
import neo4s.cypher.dsl.{Context, DSLResult}
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class SyntaxSpec extends AnyWordSpec with should.Matchers {
  val person: Person   = randomize[Person]
  val anyPerson        = any[Person]
  val worksIn: WorksIn = randomize[WorksIn]
  val dept: Department = randomize[Department]

  "MATCH" should {
    "provide query for an instance" in {
      cypher.MATCH(person).toQuery(new Context()) shouldBe DSLResult(
        "MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})",
        Map("person_id" -> person.id, "person_name" -> person.name, "person_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.MATCH(person('name)).toQuery(new Context()) shouldBe DSLResult("MATCH (person:Person {name: $person_name})",
                                                                            Map("person_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.MATCH(anyPerson).toQuery(new Context()) shouldBe DSLResult("MATCH (person:Person)")
    }
    "provide query for a path" in {
      cypher.MATCH(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department {id: $department_id,name: $department_name})",
        Map("person_id" -> person.id,
            "person_name" -> person.name,
            "person_age" -> person.age,
            "works_in_sinceDays" -> worksIn.sinceDays,
            "department_id" -> dept.id,
            "department_name" -> dept.name)
      )
    }
  }

  "WHERE" should {
    "provide query for a path" in {
      val context = new Context()
      context.add(person)
      cypher.WHERE(person -| worksIn |-> dept).toQuery(context) shouldBe DSLResult("WHERE (person)-[:WORKS_IN]->(:Department)", Map())
    }
    "provide query for a condition" in {
      val context = new Context()
      context.add(person)
      cypher.WHERE(person('age) < 30 AND person('age) > 10).toQuery(context) shouldBe DSLResult(
        "WHERE person.age < $person_age_0 AND person.age > $person_age_1",
        Map("person_age_0" -> 30, "person_age_1" -> 10))
    }
  }

  "WHERE_NOT" should {
    "provide query for a path" in {
      val context = new Context()
      context.add(person)
      cypher.WHERE_NOT(person -| worksIn |-> dept).toQuery(context) shouldBe DSLResult("WHERE NOT (person)-[:WORKS_IN]->(:Department)", Map())
    }
    "provide query for a condition" in {
      val context = new Context()
      context.add(person)
      cypher.WHERE_NOT(person('age) < 30 AND person('age) > 10).toQuery(context) shouldBe DSLResult(
        "WHERE NOT person.age < $person_age_0 AND person.age > $person_age_1",
        Map("person_age_0" -> 30, "person_age_1" -> 10))
    }
  }

  "CREATE" should {
    "provide query for an instance" in {
      cypher.CREATE(person).toQuery(new Context()) shouldBe DSLResult(
        "CREATE (person:Person {id: $person_id,name: $person_name,age: $person_age})",
        Map("person_id" -> person.id, "person_name" -> person.name, "person_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.CREATE(person('name)).toQuery(new Context()) shouldBe DSLResult("CREATE (person:Person {name: $person_name})",
                                                                            Map("person_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.CREATE(anyPerson).toQuery(new Context()) shouldBe DSLResult("CREATE (person:Person)")
    }
    "provide query for a path" in {
      cypher.CREATE(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "CREATE (person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department {id: $department_id,name: $department_name})",
        Map("person_id" -> person.id,
            "person_name" -> person.name,
            "person_age" -> person.age,
            "works_in_sinceDays" -> worksIn.sinceDays,
            "department_id" -> dept.id,
            "department_name" -> dept.name)
      )
    }
  }
  "MERGE" should {
    "provide query for an instance" in {
      cypher.MERGE(person).toQuery(new Context()) shouldBe DSLResult(
        "MERGE (person:Person {id: $person_id,name: $person_name,age: $person_age})",
        Map("person_id" -> person.id, "person_name" -> person.name, "person_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.MERGE(person('name)).toQuery(new Context()) shouldBe DSLResult("MERGE (person:Person {name: $person_name})",
        Map("person_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.MERGE(anyPerson).toQuery(new Context()) shouldBe DSLResult("MERGE (person:Person)")
    }
    "provide query for a path" in {
      cypher.MERGE(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "MERGE (person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department {id: $department_id,name: $department_name})",
        Map("person_id" -> person.id,
            "person_name" -> person.name,
            "person_age" -> person.age,
            "works_in_sinceDays" -> worksIn.sinceDays,
            "department_id" -> dept.id,
            "department_name" -> dept.name)
      )
    }
  }
  "DELETE" should {

    "provide query for a node in path" in {
      val anyDept = any[Department]
      cypher.MATCH(person --> anyDept).DELETE(anyDept).toQuery(new Context()) shouldBe DSLResult(
        """MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-->(department:Department)
          |DELETE department""".stripMargin,
        Map("person_id" -> person.id,
          "person_name" -> person.name,
          "person_age" -> person.age)
      )
    }

    "provide query for a relation in path" in {
      val anyWorksIn = anyRel[WorksIn]
      cypher.MATCH(person -| anyWorksIn |-> dept).DELETE(anyWorksIn).toQuery(new Context()) shouldBe DSLResult(
        """MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN]->(department:Department {id: $department_id,name: $department_name})
          |DELETE works_in""".stripMargin,
        Map("person_id" -> person.id,
          "person_name"-> person.name,
          "person_age" -> person.age,
          "department_id" -> dept.id,
          "department_name" -> dept.name)
      )
    }
  }
  "DETACH_DELETE" should {
    "provide query for a node in path" in {
      val anyDept = any[Department]
      cypher.MATCH(person --> anyDept).DETACH_DELETE(anyDept).toQuery(new Context()) shouldBe DSLResult(
        """MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-->(department:Department)
          |DETACH DELETE department""".stripMargin,
        Map("person_id" -> person.id,
          "person_name" -> person.name,
          "person_age" -> person.age)
      )
    }

    "provide query for a relation in path" in {
      val anyWorksIn = anyRel[WorksIn]
      cypher.MATCH(person -| anyWorksIn |-> dept).DETACH_DELETE(anyWorksIn).toQuery(new Context()) shouldBe DSLResult(
        """MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN]->(department:Department {id: $department_id,name: $department_name})
          |DETACH DELETE works_in""".stripMargin,
        Map("person_id" -> person.id,
          "person_name" -> person.name,
          "person_age" -> person.age,
          "department_id" -> dept.id,
          "department_name" -> dept.name)
      )
    }
  }

  "SET" should {
    val context = new Context()
    cypher.MATCH(person).toQuery(context)
    cypher.MATCH(anyPerson).toQuery(context)
    "provide query for an instance with setters" in {
      cypher.SET(person, List(person('name) -> "tom")).toQuery(context) shouldBe DSLResult(
        "SET person = {name = $person_name}",
        Map("person_name" -> "tom"))
    }
    "provide query for an instance with no setters" in {
      cypher.SET(person, List.empty).toQuery(context) shouldBe DSLResult(
        "SET person = {}")
    }
    "provide query for a node" in {
      cypher.SET(person('name) -> "tom", person('age) -> 12).toQuery(context) shouldBe
        DSLResult("SET person.name = $person_name,person.age = $person_age",
        Map("person_name" -> "tom", "person_age" -> 12))
    }
  }

  "ON_MATCH_SET" should {
    val context = new Context()
    cypher.MATCH(person).toQuery(context)
    cypher.MATCH(anyPerson).toQuery(context)
    "provide query for an instance with setters" in {
      cypher.ON_MATCH_SET(person, List(person('name) -> "tom")).toQuery(context) shouldBe DSLResult(
        "ON MATCH SET person = {name = $person_name}",
        Map("person_name" -> "tom"))
    }
    "provide query for an instance with no setters" in {
      cypher.ON_MATCH_SET(person, List.empty).toQuery(context) shouldBe DSLResult(
        "ON MATCH SET person = {}")
    }
    "provide query for a node" in {
      cypher.ON_MATCH_SET(person('name) -> "tom", person('age) -> 12).toQuery(context) shouldBe
        DSLResult("ON MATCH SET person.name = $person_name,person.age = $person_age",
          Map("person_name" -> "tom", "person_age" -> 12))
    }
  }

  "ON_CREATE_SET" should {
    val context = new Context()
    cypher.MATCH(person).toQuery(context)
    cypher.MATCH(anyPerson).toQuery(context)
    "provide query for an instance with setters" in {
      cypher.ON_CREATE_SET(person, List(person('name) -> "tom")).toQuery(context) shouldBe DSLResult(
        "ON CREATE SET person = {name = $person_name}",
        Map("person_name" -> "tom"))
    }
    "provide query for an instance with no setters" in {
      cypher.ON_CREATE_SET(person, List.empty).toQuery(context) shouldBe DSLResult(
        "ON CREATE SET person = {}")
    }
    "provide query for a node" in {
      cypher.ON_CREATE_SET(person('name) -> "tom", person('age) -> 12).toQuery(context) shouldBe
        DSLResult("ON CREATE SET person.name = $person_name,person.age = $person_age",
          Map("person_name" -> "tom", "person_age" -> 12))
    }
  }

  "OPTIONAL_MATCH" should {
    "provide query for an instance" in {
      cypher.OPTIONAL_MATCH(person).toQuery(new Context()) shouldBe DSLResult(
        "OPTIONAL MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})",
        Map("person_id" -> person.id, "person_name" -> person.name, "person_age" -> person.age))
    }
    "provide query for a node" in {
      cypher.OPTIONAL_MATCH(person('name)).toQuery(new Context()) shouldBe DSLResult(
        "OPTIONAL MATCH (person:Person {name: $person_name})",
        Map("person_name" -> person.name))
    }
    "provide query for a class" in {
      cypher.OPTIONAL_MATCH(any[Person]).toQuery(new Context()) shouldBe DSLResult("OPTIONAL MATCH (person:Person)")
    }
    "provide query for a path" in {
      cypher.OPTIONAL_MATCH(person -| worksIn |-> dept).toQuery(new Context()) shouldBe DSLResult(
        "OPTIONAL MATCH (person:Person {id: $person_id,name: $person_name,age: $person_age})-[works_in:WORKS_IN {sinceDays: $works_in_sinceDays}]->(department:Department {id: $department_id,name: $department_name})",
        Map("person_id" -> person.id,
            "person_name" -> person.name,
            "person_age" -> person.age,
            "works_in_sinceDays" -> worksIn.sinceDays,
            "department_id" -> dept.id,
            "department_name" -> dept.name)
      )
    }
  }
  "SKIP" should {
    "provide query for a given count" in {
      cypher.SKIP(10).toQuery(new Context()) shouldBe DSLResult("SKIP 10")
    }
  }
  "LIMIT" should {
    "provide query for a given count" in {
      cypher.LIMIT(10).toQuery(new Context()) shouldBe DSLResult("LIMIT 10")
    }
  }
  "RETURN" should {
    val context = new Context()
    context.add(person)
    context.add(anyPerson)
    context.add(dept)
    "return query for an element in Context" in {
      val context = new Context()
      context.add(person)
      cypher.RETURN(person).toQuery(context) shouldBe DSLResult("RETURN person")
    }
    "return query for any element in Context" in {
      val context = new Context()
      context.add(person)
      cypher.MATCH(anyPerson).toQuery(context)
      cypher.RETURN(anyPerson).toQuery(context) shouldBe DSLResult("RETURN person_1")
    }
    "return empty statement if no elements passed" in {
      cypher.RETURN().toQuery(context) shouldBe DSLResult.empty
    }
    "return query for more than one element in Context" in {
      val context = new Context()
      context.add(person)
      cypher.MATCH(anyPerson).toQuery(context)
      cypher.RETURN(person, anyPerson).toQuery(context) shouldBe DSLResult("RETURN person,person_1")
    }
    "return elements for a property" in {
      cypher.RETURN(person('name), dept).toQuery(context) shouldBe DSLResult("RETURN person.name,department")
    }
    "return elements for multiple properties" in {
      cypher.RETURN(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult(
        "RETURN person.name,person.age,department.name")
    }
    "throw if element to be returned not in Context" in {
      the[NoSuchElementException] thrownBy {
        cypher.RETURN(worksIn).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "return aliased elements" in {
      cypher.RETURN(person -> "pers", dept -> "dept").toQuery(context) shouldBe DSLResult(
        "RETURN person as pers,department as dept")
    }
    "return non-aliased and aliased elements in a single return" in {
      cypher.RETURN(person, dept -> "dept").toQuery(context) shouldBe DSLResult("RETURN person,department as dept")
    }
    "return aliased elements for a property" in {
      cypher.RETURN(person('name) -> "personName", dept -> "dept").toQuery(context) shouldBe DSLResult(
        "RETURN person.name as personName,department as dept")
    }
    "return aliased elements for multiple properties" in {
      cypher
        .RETURN(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("RETURN person.name as name,person.age as age,department.name as departmentName")
    }
    "RETURN DISTINCT aliased elements" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .RETURN(Distinct(person -> "pers", dept -> "dept"))
        .toQuery(context) shouldBe DSLResult("RETURN DISTINCT person as pers,department as dept")
    }
    "RETURN DISTINCT  aliased elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .RETURN(Distinct(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName"))
        .toQuery(context) shouldBe DSLResult("RETURN DISTINCT person.name as name,person.age as age,department.name as departmentName")
    }
  }
  "WITH" should {
    val context = new Context()
    context.add(person)
    context.add(dept)
    "WITH query for an element in Context" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person).toQuery(context) shouldBe DSLResult("WITH person")
    }

    "WITH empty statement if no elements passed" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH().toQuery(context) shouldBe DSLResult.empty
    }

    "WITH query for more than one element in Context" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person, dept).toQuery(context) shouldBe DSLResult("WITH person,department")
    }
    "WITH elements for a property" in {
      cypher.WITH(person('name), dept).toQuery(context) shouldBe DSLResult("WITH person.name,department")
    }
    "WITH elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult("WITH person.name,person.age,department.name")
    }
    "throw if element to be WITHed not in Context" in {
      the[NoSuchElementException] thrownBy {
        cypher.WITH(worksIn).toQuery(context)
      } should have message "One or more of the elements to be returned are not in Context!"
    }
    "WITH aliased elements" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person -> "pers", dept -> "dept").toQuery(context) shouldBe DSLResult(
        "WITH person as pers,department as dept")
    }
    "WITH non-aliased and aliased elements in a single WITH" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person, dept -> "dept").toQuery(context) shouldBe DSLResult("WITH person,department as dept")
    }
    "WITH aliased elements for a property" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher.WITH(person('name) -> "personName", dept -> "dept").toQuery(context) shouldBe DSLResult(
        "WITH person.name as personName,department as dept")
    }
    "WITH aliased elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(person('name) -> "name", person('age) -> "age", dept('name) -> "departmentName")
        .toQuery(context) shouldBe DSLResult("WITH person.name as name,person.age as age,department.name as departmentName")
    }
    "WITH DISTINCT aliased elements" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(Distinct(person -> "pers", dept -> "dept"))
        .toQuery(context) shouldBe DSLResult("WITH DISTINCT person as pers,department as dept")
    }
    "WITH DISTINCT  aliased elements for multiple properties" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      cypher
        .WITH(Distinct(person('name) -> "name"), dept('name) -> "departmentName")
        .RETURN(person('name))
        .toQuery(context) shouldBe DSLResult(
          """WITH DISTINCT person.name as name,department.name as departmentName
            |RETURN name""".stripMargin)
    }
    "WITH aliased elements should return alias going forward" in {
      val context = new Context()
      context.add(person)
      context.add(dept)
      val anyPerson = any[Person]
      val anyRel = anyRelation
      cypher.MATCH(anyPerson -| anyRel |-> anyNode).toQuery(context)
      cypher.WITH(person -> "p", anyPerson -> "p2", anyRel -> "ar").toQuery(context)
      cypher.RETURN(person, anyPerson, anyRel).toQuery(context) shouldBe DSLResult("RETURN p,p2,ar")
    }
  }
  "ORDER_BY" should {
    val context = new Context()
    context.add(person)
    context.add(dept)

    "return query for an element in Context" in {
      cypher.ORDER_BY(person).toQuery(context) shouldBe DSLResult("ORDER BY person")
    }

    "return empty statement if no elements passed" in {
      cypher.ORDER_BY().toQuery(context) shouldBe DSLResult.empty
    }

    "return query for more than one element in Context" in {
      cypher.ORDER_BY(person, dept).toQuery(context) shouldBe DSLResult("ORDER BY person,department")
    }
    "return elements for a property" in {
      cypher.ORDER_BY(person('name), dept).toQuery(context) shouldBe DSLResult("ORDER BY person.name,department")
    }
    "return elements for multiple properties" in {
      cypher.ORDER_BY(person('name, 'age), dept('name)).toQuery(context) shouldBe DSLResult(
        "ORDER BY person.name,person.age,department.name")
    }

    "return query for an element descending in Context" in {
      cypher.ORDER_BY(person.DESC).toQuery(context) shouldBe DSLResult("ORDER BY person DESC")
    }
    "return query for more than one element with mixed descending in Context" in {
      cypher.ORDER_BY(person.DESC, dept).toQuery(context) shouldBe DSLResult("ORDER BY person DESC,department")
    }
    "return elements for a property descending" in {
      cypher.ORDER_BY(person('name).DESC, dept).toQuery(context) shouldBe DSLResult("ORDER BY person.name DESC,department")
    }
    "return elements for multiple properties descending" in {
      cypher.ORDER_BY(person('name, 'age).DESC, dept('name)).toQuery(context) shouldBe DSLResult(
        "ORDER BY person.name DESC,person.age DESC,department.name")
    }
  }

  "anyNode" should {
    "provide right query when not in context" in {
      cypher.MATCH(anyNode).toQuery() shouldBe DSLResult("MATCH (any)")
    }
    "provide right query when in context" in {
      val ctx   = new Context()
      val anyN  = anyNode
      val anyN2 = anyNode
      cypher.MATCH(anyN).toQuery(ctx) shouldBe DSLResult("MATCH (any)")
      cypher.MATCH(anyN).toQuery(ctx) shouldBe DSLResult("MATCH (any)")
      cypher.MATCH(anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (any_1)")
    }
  }
  "anyRelation" should {
    "provide right query when not in context" in {
      cypher.MATCH(anyNode).toQuery() shouldBe DSLResult("MATCH (any)")
    }
    "provide right query when in context" in {
      val ctx   = new Context()
      val anyN  = anyNode
      val anyN2 = anyNode
      val anyR  = anyRelation
      val anyR2 = anyRelation
      cypher.MATCH(anyN -| anyR |-> anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (any)-[any_1]->(any_2)")
      cypher.MATCH(anyN -| anyR |-> anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (any)-[any_1]->(any_2)")
      cypher.MATCH(anyN -| anyR2 |-> anyN2).toQuery(ctx) shouldBe DSLResult("MATCH (any)-[any_3]->(any_2)")
    }
  }
}
