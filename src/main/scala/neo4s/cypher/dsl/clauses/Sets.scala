package neo4s.cypher.dsl.clauses
import neo4s.cypher.dsl.clauses.SetTypes.{EntitySet, ProductSet}
import neo4s.cypher.dsl.entities.CypherEntity
import neo4s.cypher.dsl.{Context, DSLResult}

/**
  *
  *  Sets(person -> List(person('name) -> "jane", person('age) -> 25))
  *  Sets(person('name) -> "jane")
  *  Sets(person -> List.empty)
  *
  */
private[dsl] object SetTypes {

  type ProductSet = (Product, EntitySet)
  type EntitySet  = Map[CypherEntity, Any]
}

private[dsl] class SetSpec(private val either: Either[ProductSet, EntitySet]) {

  def toQuery(context: Context): DSLResult = {
    either.fold(
      productSet => {
        val (element, entitySet) = productSet
        val result = resultFromEntitySet(entitySet, false, context)
        val id = context.get(element).get
        result.copy(query = s"$id = {${result.query}}")
      },
      entitySet => resultFromEntitySet(entitySet, true, context)
    )
  }

  private def resultFromEntitySet(entitySet: Map[CypherEntity, Any], includeNodeId: Boolean, context: Context): DSLResult = {
    if (entitySet.isEmpty) DSLResult.empty
    else {
      val resultSet = entitySet.keySet
        .flatMap(entity => {
          val result = entity.toSetterQuery(context,includeNodeId)
          if (result.isEmpty) None
          else {
            // Map setter value types and handle Options
            val setterTuple = context.mapper(result.queryMap.head._1, entitySet(entity)) match {
              case (key,Some(value)) => Some((key,value))
              case (_,None) => None
              case tuple => Some(tuple)
            }

            val processed = setterTuple.map(t => result.copy(queryMap = result.queryMap ++ Map(t)))
            processed
          }
        })

      if (resultSet.isEmpty) DSLResult.empty
      else resultSet.reduce(_ ++ (_, ","))
    }
  }

}

private[dsl] class Sets(setCommand: Sets.SetCommand, private val setSpec: SetSpec) extends Clause {
  override def toQuery(context: Context = new Context()): DSLResult = {
    val result = setSpec.toQuery(context)
    val mapped = result.queryMap.map(context.mapper)
    result.copy(query = s"${setCommand} ${result.query}", queryMap = mapped)
  }
}

private[dsl] object Sets {
  def apply[T <: Product](setCommand: SetCommand, element: T, set: List[(CypherEntity, Any)]): Sets = new Sets(setCommand, new SetSpec(Left(element -> set.toMap))) // Node
  def apply[T <: CypherEntity](setCommand: SetCommand, set: (CypherEntity, Any)*): Sets = new Sets(setCommand, new SetSpec(Right(set.toMap))) // Fields in Node

  trait SetCommand {
    val command: String
    override def toString() = command
  }

  val setCommand = new SetCommand { override val command = "SET" }
  val onCreateSetCommand = new SetCommand { override val command = "ON CREATE SET" }
  val onMatchSetCommand = new SetCommand { override val command = "ON MATCH SET" }
}
