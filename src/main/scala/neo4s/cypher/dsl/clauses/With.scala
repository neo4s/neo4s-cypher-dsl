package neo4s.cypher.dsl.clauses

import neo4s.cypher.dsl.{Context, DSLResult}
import neo4s.cypher.dsl.entities.{AliasedProduct, Node, Relationship}
import neo4s.cypher.dsl.operators.UnaryOperator
import neo4s.cypher.dsl.utils.ElementPropertyExtractingAndAliasing
import neo4s.cypher.dsl.Utils._

private[cypher] class With(elements: Either[AliasedProduct, UnaryOperator]*)
    extends Clause
    with ElementPropertyExtractingAndAliasing {
  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  @throws[NoSuchElementException]
  def toQuery(context: Context = new Context()): DSLResult = {
    val ids = elements
      .map(element => {
        if (element.isRight) element.right.get.toQuery(context).query
        else {
          val aliasedProduct   = element.left.get
          val (el, properties) = getElementAndProperties(aliasedProduct.node)
          context
            .get(el)
            .map(identifier => {
              val hasProperties = (aliasedProduct.node match {
                case s: Node[_, _] => s.properties.toList.length
                case s: Relationship[_, _] => s.properties.toList.length
                case s => 0
              }) > 0
              if (aliasedProduct.alias.isDefined && !hasProperties) context.updateLiteral(el, aliasedProduct.alias.get)
              else {
                context.add(aliasedProduct.node)
                if(aliasedProduct.alias.isDefined) context.updateLiteral(aliasedProduct.node, aliasedProduct.alias.get)
              }
              makeAliasedString(identifier, properties, aliasedProduct.alias)
            })
            .getOrElse(throw new NoSuchElementException(errorMessage))
        }
      })
      .mkString(",")

    DSLResult(if (ids.nonEmpty) s"WITH $ids" else "")
  }
}
private[cypher] object With {
  private def makeEitherList(products: List[Product]): List[Either[AliasedProduct, UnaryOperator]] = products match {
    case Nil                        => List.empty
    case (s: UnaryOperator) :: remaining => Right(s) +: makeEitherList(remaining)
    case s :: remaining             => Left(AliasedProduct.makeAliasedProduct(s)) +: makeEitherList(remaining)
  }
  def apply(elements: Product*): With = new With(makeEitherList(elements.toList): _*)
  val empty                           = With(Seq.empty: _*)
}
