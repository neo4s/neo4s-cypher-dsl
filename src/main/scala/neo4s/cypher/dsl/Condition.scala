package neo4s.cypher.dsl

import neo4s.cypher.dsl.clauses.SupportsWhere

private[cypher] class Condition(val parts: (Option[String],SupportsWhere)*) extends SupportsWhere {

  def this(component: SupportsWhere) = this((None,component))

  def toWhereQuery(context: Context = new Context()): DSLResult = {
    parts
      .toList
      .flatMap { case (logicOp, predicate) => {
        logicOp.toList.map(DSLResult(_,Map[String,Any]())) ::: List(predicate.toWhereQuery(context))
      } }
      .reduce(_ ++ _)
  }

  def OR(path: Path): Condition = {
    val condition = new Condition(path)
    OR(condition)
  }

  def OR(condition: Condition): Condition = {
    val otherParts = condition.parts.toList
    val withLogicOperator = otherParts.headOption.map(part => part.copy(Some(" OR "),part._2)).toList ::: condition.parts.tail.toList
    new Condition((parts.toList ::: withLogicOperator):_*)
  }

  def AND(path: Path): Condition = {
    val condition = new Condition(path)
    AND(condition)
  }

  def AND(condition: Condition): Condition = {
    val otherParts = condition.parts.toList
    val withLogicOperator = otherParts.headOption.map(part => part.copy(Some(" AND "),part._2)).toList ::: condition.parts.tail.toList
    new Condition((parts.toList ::: withLogicOperator):_*)
  }

  def XOR(path: Path): Condition = {
    val condition = new Condition(path)
    XOR(condition)
  }

  def XOR(condition: Condition): Condition = {
    val otherParts = condition.parts.toList
    val withLogicOperator = otherParts.headOption.map(part => part.copy(Some(" XOR "),part._2)).toList ::: condition.parts.tail.toList
    new Condition((parts.toList ::: withLogicOperator):_*)
  }
}
