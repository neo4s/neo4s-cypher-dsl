package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.{Context, DSLResult}
import neo4s.cypher.dsl.entities.{Descending, OrderingProduct}
import neo4s.cypher.dsl.utils.ElementPropertyExtractingAndAliasing

private[cypher] class OrdersBy(elements: OrderingProduct*)
    extends Clause
    with ElementPropertyExtractingAndAliasing {
  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  override def toQuery(context: Context): DSLResult = {
    val ids = elements
      .map(element => {
        val (el, properties) = getElementAndProperties(element.element)
        context
          .get(el)
          .map(identifier => makeAliasedReturnString(identifier, properties, element.descending))
          .getOrElse(throw new NoSuchElementException(errorMessage))
      })
      .mkString(",")
    DSLResult((if (ids.nonEmpty) s"ORDER BY $ids" else "").trim)
  }

  private def makeAliasedReturnString(identifier: String, properties: List[String], descendingOrder: Boolean): String = {
    val identifierString = properties.map(p => s"$identifier.$p${getOrderingString(descendingOrder)}").mkString(",")
    if (identifierString.isEmpty) s"$identifier${getOrderingString(descendingOrder)}" else identifierString
  }

  private def getOrderingString(descendingOrder:Boolean) = if (descendingOrder) " DESC" else ""
}

private[cypher] object OrdersBy {
  val empty = OrdersBy(Seq.empty: _*)

  def apply(elements: Product*): OrdersBy =
    new OrdersBy(makeOrderingProduct(elements.toList): _*)

  private def makeOrderingProduct(list: List[Product]): Seq[OrderingProduct] = list match {
    case Nil                       => List.empty
    case Descending(s: Product) :: remaining => OrderingProduct(s,true) +: makeOrderingProduct(remaining)
    case (s: Product) :: remaining => OrderingProduct(s, false) +: makeOrderingProduct(remaining)
  }
}
