package neo4s.cypher.dsl.spec.clauses

import neo4s.cypher.dsl.spec.{Context, DSLResult}
import neo4s.cypher.dsl.spec.entities.AliasedProduct
import neo4s.cypher.dsl.spec.utils.{ElementPropertyExtracting, ElementPropertyExtractingAndAliasing}

private[dsl] class Removes(element: AliasedProduct)
  extends Clause
  with ElementPropertyExtractingAndAliasing {
  private val errorMessage = "One or more of the elements to be returned are not in Context!"

  override def toQuery(context: Context = new Context()): DSLResult = {
    val id = context.get(element.node)
      .map(id => (id, List.empty[String]))
      .orElse {
        val (el, properties) = getElementAndProperties(element.node)
        Option(context.get(el).getOrElse(throw new NoSuchElementException(errorMessage)), properties)
      }
      .map(s => makeAliasedString(s._1, s._2))
      .getOrElse(throw new NoSuchElementException(errorMessage))
    DSLResult(if (id.nonEmpty) s"REMOVE $id" else "")

  }
}

private[dsl] object Removes {
  def apply(element: Product): Removes = {
    new Removes(AliasedProduct.makeAliasedProduct(element))
  }
}
