<!---
# scala-cypher-dsl [![Build Status](https://travis-ci.org/manishkkatoch/scala-cypher-dsl.svg?branch=master)](https://travis-ci.org/manishkkatoch/scala-cypher-dsl) ![Sonatype Nexus (Releases)](https://img.shields.io/nexus/r/https/oss.sonatype.org/me.manishkatoch/scala-cypher-dsl.svg) ![Maven Central](https://img.shields.io/maven-central/v/me.manishkatoch/scala-cypher-dsl.svg)
-->

# neo4s-cypher-dsl

A type-safe, compile time DSL for writing Cypher queries in Scala, forked from the excellent [scala-cypher-dsl](https://github.com/manishkkatoch/scala-cypher-dsl) 
project, lead by `Manish Katoch`.

## Motivation

From the [scala-cypher-dsl](https://github.com/manishkkatoch/scala-cypher-dsl) project: 

> With Neo4J and Scala, the ORMs satisfy only a small subset of querying needs and majority of the fairly complex cypher query tend to be in the form of strings.Cypher strings (like SQL strings) have inherent issues like no type safety, minimal syntax checking, difficulty in composing etc.
>
> Scala-Cypher-DSL aims to alleviate above by providing following:
> 1. Type-safe constructs using user defined models and ADTs
> 2. Chainable DSL like Cypher.
> 3. Automatic identifiers generation( you don't have to manage identifiers, just work with instances/models).
> 4. Parameterized queries and automatic creation of parameters map. 
> 
> Note: _It does not provide drivers for Neo4J but only concerns with query and query parameters creation_

Additional motivations, specifically for this fork: 
1. Updated support to version 4 of the Cypher language (it has been tested with `4.3.4`)
2. Some admitted fussiness regarding the package structure, specifically to remove usernames and to correct `cypherDSL` => `cypher.dsl`. 
Primarily, we dislike the proliferation of usernames in our proprietary and open source code. Sincerely, no offense is meant for Manish.
3. Added support for the WHERE clause
4. The Context implementation was fragile, the DSL format would result in entity names that would change depnding upon minor semantic differences in 
expression order. We have updated it to remain consistent (and based upon DSL expression names) instead
5. Better support for the `return` clause

**NOTE** - We greatly appreciate the inspiration and design of the original [scala-cypher-dsl](https://github.com/manishkkatoch/scala-cypher-dsl) 
project and will be watching and incoporating updates and improvements. We haven't filed a PR to push our changes back because many of our 
changes (and motivations) would be overly disruptive to the source project.

## Installation

Binary release artifacts are published to the Sonatype OSS Repository Hosting service and synced to Maven Central.

#### SBT
```scala
libraryDependencies ++= Seq(
  "org.neo4j.driver" % "neo4j-java-driver" % "4.3.4",
  "io.github.neo4s" %% "neo4s-cypher-dsl" % "0.5.0",
  "io.github.neo4s" %% "neo4s-query-support" % "0.1.2"
)
```

## Usage

Consider following domain models representing people working in a fictitious department and friendly by nature. 
```scala
//sample domain models
case class Person(id: String, name: String, age: Int)
case class WorksIn(sinceDays: Int)
case class IsFriendOf(since: Int, lastConnectedOn: String)
case class Department(id: String, name: String)
```
To start writing query DSL, import the following
```scala
import neo4s.cypher.dsl.syntax._
import neo4s.cypher.dsl.syntax.patterns._ //optional, import for expressing paths.
```

using DSL for a simple match query generation for an instance of model
```scala
// Assumes a Person case class 
case class Person(dept: String, name: String, age: Int)

//for a person John Doe
val johnDoe = Person("AX31SD", "John Doe", 50)

//match and return Neo4J data
val johnDoeQuery = cypher.MATCH(johnDoe)
    .RETURN(johnDoe)
    .toQuery()

johnDoeQuery.query
//val res0: String = MATCH (person:Person {dept: $person_dept,name: $person_name,age: $person_age}) RETURN pers

johnDoeQuery.queryMap
// val res1: Map[String,Any] = Map(person_dept -> AX31SD, person_name -> John Doe, person_age -> 50)
```

match Person only by a property(e.g. name)
```scala
//for a person John Doe
val johnDoe = Person("AX31SD", "John Doe", 50)

//match and return Neo4J data
val johnDoeQuery = cypher.MATCH(johnDoe('name))
    .RETURN(johnDoe)
    .toQuery()

johnDoeQuery.query
//res0: String = DSLResult(MATCH (person:Person {name: $person_name}) RETURN person

johnDoeQuery.queryMap
//res1: Map[String,Any] = Map(person_name -> John Doe))
```

**Note:** if the property doesn't exist, compilation will fail.

using DSL for matching any instance of model.
```scala
//for any person
val anyPerson = any[Person] // any instance of node labelled Person

val result = cypher.MATCH(anyPerson)
    .RETURN(anyPerson)
    .toQuery()

result.query
//res0: String = MATCH (person:Person)
//               RETURN person

result.queryMap
//res1: Map[String,Any] = Map()
```
query for all the friends of John Doe in Science department
```scala
val scienceDept = Department("ZSW12R", "Science")
val anyPerson = any[Person]
val isFriendOf = anyRel[IsFriendOf] //any relation instance of label IsFriendOf

val result = cypher.MATCH(johnDoe -| isFriendOf |-> anyPerson <-- scienceDept)
    .RETURN(anyPerson)
    .toQuery()

result.query
//res0: String = MATCH (person:Person {id: {person_id},name: {person_name},age: {person_age}})-[isFriendOf:IS_FRIEND_OF]->(any_person:Person)<--(department:Department {id: {department_id},name: {department_name}})
//               RETURN any_person

result.queryMap
//res1: Map[String,Any] = Map(person_id -> AX31SD, person_name -> John Doe, department_name -> Science, person_age -> 50, department_id -> ZSW12R)
```

## DSL Specifications

as of v0.5.0

| Cypher Clauses | DSL Support |
|----------------|-------------|
| MATCH | :white_check_mark: |
| OPTIONAL MATCH | :white_check_mark: |
| START | :x: |
| RETURN | :white_check_mark: |
| WITH | :white_check_mark: |
| UNWIND | :x: |
| WHERE | :white_check_mark: |
| ORDER BY | :white_check_mark: |
| SKIP | :white_check_mark: |
| LIMIT | :white_check_mark: |
| CREATE | :white_check_mark: |
| DELETE | :white_check_mark: |
| SET | :white_check_mark: |
| REMOVE | :white_check_mark: |
| FOREACH | :x: |
| MERGE | :white_check_mark: |
| CALL […​YIELD]| :x: |
| CREATE UNIQUE | :x: |
| UNION | :x: |

## Contributors and Participation
[neo4s-cyppher-dsl](https://github.com/neo4s/neo4s-cypher-dsl) is maintained by the [neo4s](https://github.com/neo4s) organization 

Any form of contribution (issue report, PR, etc) is more than welcome.

## Special Mentions
[neo4s-cyppher-dsl](https://github.com/neo4s/neo4s-cypher-dsl) is forked from [scala-cypher-dsl](https://github.com/manishkkatoch/scala-cypher-dsl)

This project is made possible by [Shapeless](https://github.com/milessabin/shapeless). Special thanks to [Miles Sabin](https://github.com/milessabin)


