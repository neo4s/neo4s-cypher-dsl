package neo4s.cypher.dsl

import shapeless.ops.record.Keys
import shapeless.{HList, LabelledGeneric}
import Utils._
import neo4s.cypher.dsl.operators.Operator

/** trait [[QueryProvider]] provides methods to get matching properties and values for an instance of type [[T]]
  * @tparam T type for which the [[QueryProvider]] expects an instance
  */
trait QueryProvider[T <: Product] {
  /** returns a [[DSLResult]] for a given element and all its properties
    * @param element instance of type [[T]]
    * @param context the [[Context]] of the [[Statement]]
    * @return [[DSLResult]]
    */
  def getMatchers(element: T)(implicit context: Context): Seq[DSLResult]

  /** returns a [[DSLResult]] for a given element and selected properties
    * @param element instance of type [[T]]
    * @param columns an [[HList]] of [[Symbol]] corresponding to properties selected
    * @param context the [[Context]] of the [[Statement]] which holds the @param element
    * @tparam U type of columns where U <: [[HList]]
    * @return
    */
  def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult]

  def getSetters(element: T, includeNodeId: Boolean)(implicit context: Context): Seq[DSLResult]

  def getSetters[U <: HList](element: T, columns: U, includeNodeId: Boolean)(implicit context: Context): Seq[DSLResult]

  def getOperators[U <: HList](element: T, columns: U, includeNodeId: Boolean, operator: Operator)(implicit context: Context): Seq[DSLResult]

}

/** Factory for [[neo4s.cypher.dsl.QueryProvider]] instances */
object QueryProvider {
  def apply[T <: Product](implicit queryProvider: QueryProvider[T]): QueryProvider[T] =
    queryProvider

  /** Default [[neo4s.cypher.dsl.QueryProvider]] implementation
  * this is served implicitly whenever an implicit instance of [[QueryProvider]] is required but unavailable
   *
   * @param lGen implicit [[LabelledGeneric]] to extract [[HList]] from case class.
    * @param keys implicit [[Keys]] extracted at compile time from @lGen
    * @tparam T type of element instance
    * @tparam H expected type of HList
    * @tparam K expected type of @keys
    * @return [[QueryProvider]] instance
    */
  implicit def makeQueryProvider[T <: Product, H <: HList, K <: HList](implicit
                                                                       lGen: LabelledGeneric.Aux[T, H],
                                                                       keys: Keys.Aux[H, K]): QueryProvider[T] =
    new QueryProvider[T] {
      import CypherBehavior._

      override def getMatchers(element: T)(implicit context: Context): Seq[DSLResult] = {
        val id     = context.get(element).get
        val record = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        recordToDSLResults(id, record)(matchPropertyPattern, defaultMapper, context.mapper)
      }

      override def getMatchers[U <: HList](element: T, columns: U)(implicit context: Context): Seq[DSLResult] = {
        val id             = context.get(element).get
        val record         = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        val columnList     = columns.toList[Symbol].map(_.name)
        val filteredRecord = record.filter(t => columnList.contains(t._1))
        recordToDSLResults(id, filteredRecord)(matchPropertyPattern, defaultMapper, context.mapper)
      }

      override def getSetters(element: T, includeNodeId: Boolean)(implicit context: Context): Seq[DSLResult] = {
        val id     = context.get(element).get
        val record = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        recordToDSLResults(id, record)(setPropertyPattern(includeNodeId,_,_), defaultMapper, context.mapper)
      }

      override def getSetters[U <: HList](element: T, columns: U, includeNodeId: Boolean)(implicit context: Context): Seq[DSLResult] = {
        val id             = context.get(element).get
        val record         = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        val columnList     = columns.toList[Symbol].map(_.name)
        val filteredRecord = record.filter(t => columnList.contains(t._1))
        recordToDSLResults(id, filteredRecord)(setPropertyPattern(includeNodeId,_,_), defaultMapper, context.mapper)
      }

      override def getOperators[U <: HList](element: T, columns: U, includeNodeId: Boolean, operator: Operator)(implicit context: Context): Seq[DSLResult] = {
        // TODO, find away to get this
        assert(columns.toList.size < 2, "Operators require only one attribute is selected...")
        assert(context.get(element).nonEmpty, s"Operator refers to element: $element not in context!")

        val id             = context.get(element).get
        val record         = keys().toList[Symbol].map(_.name) zip lGen.to(element).toList[Any]
        val columnList     = columns.toList[Symbol].map(_.name)
        val filteredRecord = record.filter(t => columnList.contains(t._1))

        val operatorRecord = filteredRecord.map { case (key,_) => (key,operator.operand)}
        val literalQualifier = context.getAndIncrementQualifier()
        recordToDSLResults(id, operatorRecord)(propertyOperatorPattern(includeNodeId,operator.operator,literalQualifier,_,_), literalMapper(literalQualifier,_,_), context.mapper)
      }

      private def recordToDSLResults(id: String, record: Seq[(String, Any)])(
        queryStringGenerator: (String,String) => String,
        mapper: PropertyPlaceholderMapper,
        fieldTypeMapper: FieldTypeMapper
      ): Seq[DSLResult] =
        record.flatMap(tuple => {
          val mapped = fieldTypeMapper(tuple)

          mapped._2 match {
            case Some(v) =>
              val valueMapped = (mapped._1,v)
              Some(DSLResult(queryStringGenerator(id, mapped._1), mapper(id, valueMapped)))
            case None => None
            case _ => Some(DSLResult(queryStringGenerator(id, mapped._1), mapper(id, mapped)))
          }
        })

      private def matchPropertyPattern(identifier: String, propertyName: String) =
        s"$propertyName: $$${propertyValuePlaceHolder(identifier, propertyName)}"

      private def propertyOperatorPattern(includeNodeId: Boolean, opString: String, index: Int, identifier: String, propertyName: String) = {
        if (includeNodeId) s"$identifier.$propertyName ${opString} $$${propertyValuePlaceHolder(identifier, propertyName, index)}"
        else s"$propertyName ${opString} $$${propertyValuePlaceHolder(identifier, propertyName, index)}"
      }

      private def setPropertyPattern(includeNodeId: Boolean, identifier: String, propertyName: String) ={
        if (includeNodeId) s"$identifier.$propertyName = $$${propertyValuePlaceHolder(identifier, propertyName)}"
        else s"$propertyName = $$${propertyValuePlaceHolder(identifier, propertyName)}"
      }

      private def propertyValuePlaceHolder(identifier: String, propertyName: String) = s"${identifier}_$propertyName"
      private def propertyValuePlaceHolder(identifier: String, propertyName: String, index: Int) = s"${identifier}_${propertyName}_$index"

      private def defaultMapper: PropertyPlaceholderMapper = {
        case (id, (key,value)) => Map(propertyValuePlaceHolder(id, key) -> value)
      }

      private def literalMapper(index: Int, id: String, tuple: (String, Any)) = {
        val (key,value) = tuple
        Map(propertyValuePlaceHolder(id,key,index) -> value)
      }

    }

}
