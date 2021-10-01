package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.{Context, DSLResult}
import neo4s.cypher.dsl.spec.entities.OrderingProduct
import neo4s.cypher.dsl.spec.utils.ElementPropertyExtractingAndAliasing

private[dsl] class OrdersBy(descendingOrder: Boolean, elements: OrderingProduct*)
    extends Clause
    with ElementPropertyExtractingAndAliasing {
  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  override def toQuery(context: Context): DSLResult = {
    val ids = elements
      .map(element => {
        val (el, properties) = getElementAndProperties(element.element)
        context
          .get(el)
          .map(identifier => makeAliasedReturnString(identifier, properties))
          .getOrElse(throw new NoSuchElementException(errorMessage))
      })
      .mkString(",")
    DSLResult((if (ids.nonEmpty) s"ORDER BY $ids $getOrderingString" else "").trim)
  }

  private def makeAliasedReturnString(identifier: String, properties: List[String]): String = {
    val identifierString = properties.map(p => s"$identifier.$p").mkString(",")
    if (identifierString.isEmpty) identifier else identifierString
  }

  private def getOrderingString = if (descendingOrder) "DESC" else ""

}
private[dsl] object OrdersBy {
  val empty = OrdersBy(Seq.empty: _*)

  def apply(elements: Product*): OrdersBy = new OrdersBy(false, makeOrderingProduct(elements.toList): _*)

  def apply(descendingOrder: Boolean, elements: Product*): OrdersBy =
    new OrdersBy(descendingOrder, makeOrderingProduct(elements.toList): _*)

  private def makeOrderingProduct(list: List[Product]): Seq[OrderingProduct] = list match {
    case Nil                       => List.empty
    case (s: Product) :: remaining => OrderingProduct(s) +: makeOrderingProduct(remaining)
  }
}
