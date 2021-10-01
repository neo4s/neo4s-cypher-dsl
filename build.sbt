import Dependencies._

ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "neo4s"
ThisBuild / organizationName := "neo4s"

lazy val root = (project in file("."))
  .settings(
    name := "cypher-dsl",
    libraryDependencies ++= Seq(scalaExtras,shapeless,scalaTest)
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
