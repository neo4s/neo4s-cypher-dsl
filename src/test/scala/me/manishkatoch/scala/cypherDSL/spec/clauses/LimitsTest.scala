package me.manishkatoch.scala.cypherDSL.spec.clauses

import me.manishkatoch.scala.cypherDSL.spec.DSLResult
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class LimitsTest extends AnyWordSpec with should.Matchers {
  "Limits" should {
    "provide query for a given count" in {
      Limits(10).toQuery() shouldBe DSLResult("LIMIT 10")
    }
  }
}
