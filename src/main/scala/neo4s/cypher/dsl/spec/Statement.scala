package neo4s.cypher.dsl.spec

import neo4s.cypher.dsl.spec.clauses.Clause

/** Statement is the basic building block of Cypher. A Statement contains one or more [[Clause]].
  * The [[Statement]] returns a [[DSLResult]] which is a result of computation of all [[Clause]]s the statement contains
  *
  * @param clauses [[Seq]] of [[Clause]]
  */

private[dsl] case class Statement(private[dsl] val clauses: Seq[Clause]) {
  /**
  * Returns a [[DSLResult]] for this statement
    * @param context any [[Context]] that needs to be reused. Defaults to creating new [[Context]]
    * @return [[DSLResult]]
    */
  def toQuery(context: Context = new Context()): DSLResult = {
    val (queryList, paramMap) = clauses.map(clause => clause.toQuery(context))
    .foldLeft((List.empty[String], Map.empty[String,Any])) {(acc, result) =>
      (acc._1 :+ result.query, acc._2 ++ result.queryMap)
    }
    DSLResult(queryList.mkString(System.lineSeparator()), paramMap)
  }
}

/** Factory for [[neo4s.cypher.dsl.spec.Statement]] instances */
private[dsl] object Statement {

  /** Creates [[Statement]] with no [[Clause]]
    * @return
    */
  def apply(): Statement = new Statement(Seq.empty)
}
