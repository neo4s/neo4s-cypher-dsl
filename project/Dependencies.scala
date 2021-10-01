import sbt._

object Dependencies {
  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.7"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.10" % Test
  lazy val scalaExtras = "org.scala-lang" % "scala-reflect" % "2.13.6"
}
