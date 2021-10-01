package neo4s.cypher.dsl.entities

private[cypher] case class AliasedProduct(node: Product, alias: Option[String])
private[cypher] object AliasedProduct {
  def makeAliasedProduct(list: List[Product]): Seq[AliasedProduct] = list match {
    case Nil                                 => List.empty
    case (s: (Product, String)) :: remaining => AliasedProduct(s._1, Option(s._2)) +: makeAliasedProduct(remaining)
    case (s: Product) :: remaining           => AliasedProduct(s, None) +: makeAliasedProduct(remaining)
    case _ :: _                              => throw new IllegalArgumentException("AliasedProduct only takes product")
  }
  def makeAliasedProduct(product: Product): AliasedProduct = product match {
    case s: (Product, String) => AliasedProduct(s._1, Option(s._2))
    case s                    => AliasedProduct(s, None)
  }
}
