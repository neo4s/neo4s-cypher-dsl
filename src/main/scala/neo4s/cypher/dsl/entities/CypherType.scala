package neo4s.cypher.dsl.entities

import java.util.UUID

import neo4s.cypher.dsl.operators.Operator
import neo4s.cypher.dsl.{Context, DSLResult, QueryProvider}
import neo4s.cypher.dsl.utils.SnakeCasing
import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.reflect.runtime.universe.{Type, typeOf}

private[dsl] sealed abstract class CypherType(tpe: Type, fingerprint: UUID) extends CypherEntity {
  def toQuery(context: Context = new Context()): DSLResult = {
    context
      .map(this) { result =>
        DSLResult(result.trim)
      }
      .getOrElse {
        val typeLabel = label(context.nodeIdTransform)
        val id = context.add(this,typeLabel)
        val labelString = tpe match {
          case s if s =:= typeOf[Any] => ""
          case _ => s":${typeLabel}"
        }
        DSLResult(s"$id$labelString")
      }
  }

  override def toWhereQuery(context: Context, operator: Option[Operator]): DSLResult = {
    context
      .map(this) { result =>
        DSLResult(result.trim)
      }
      .getOrElse {
        val labelString = tpe match {
          case s if s =:= typeOf[Any] => ""
          case _ => s":${label(context.nodeIdTransform)}"
        }
        DSLResult(s"$labelString")
      }

  }

  override def toSetterQuery(context: Context, includeNodeId: Boolean): DSLResult = ???

  def label(transform: (String) => String): String = transform(tpe.typeSymbol.asClass.name.decodedName.toString)

}

private[cypher] case class NodeType(tpe: Type, fingerprint: UUID = UUID.randomUUID()) extends CypherType(tpe, fingerprint) {
  override def toQuery(context: Context): DSLResult = {
    val result = super.toQuery(context)
    result.copy(query = s"(${result.query})")
  }

  override def toWhereQuery(context: Context, operator: Option[Operator]): DSLResult = {
    val result = super.toWhereQuery(context, operator)
    result.copy(query = s"(${result.query})")
  }
}

//work from here.
private[dsl] case class MultiRelationType(types: List[RelationTypeOrInstance], fingerprint: UUID = UUID.randomUUID())
    extends CypherType(typeOf[List[RelationTypeOrInstance]], fingerprint) {

  def or[U <: Product, UH <: HList](rel: U, properties: UH)(
      implicit queryProvider: QueryProvider[U],
      i1: ToTraversable.Aux[UH, List, Symbol]): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(Relationship(rel, properties))
    copy(types :+ relationTypeOrInstance)
  }
  def or(rel: RelationType): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(rel)
    copy(types :+ relationTypeOrInstance)
  }

  override def toQuery(context: Context): DSLResult = {
    context
      .map(this) { result =>
        DSLResult(s"[${result.trim}]")
      }
      .getOrElse {
        val id = context.add(this,"rel")
        val result = types.map(_.toQuery(context)).foldLeft(List.empty[String], Map.empty[String, Any]) {
          (acc, result) =>
            (acc._1 :+ result.query, acc._2 ++ result.queryMap)
        }
        DSLResult(s"[$id:${result._1.mkString("|:")}]", result._2)
      }
  }

}

private[cypher] case class RelationType(tpe: Type,
                                        variableLengthRelation: Option[VariableLengthRelation] = None,
                                        fingerprint: UUID = UUID.randomUUID())
    extends CypherType(tpe, fingerprint)
    with SnakeCasing {

  override def toQuery(context: Context): DSLResult = {
    val varLengthStringIfAny = variableLengthRelation.map(_.toQuery(context)).mkString
    val result               = super.toQuery(context)
    result.copy(query = s"[${result.query}$varLengthStringIfAny]", queryMap = result.queryMap)
  }

  override def toWhereQuery(context: Context, operator: Option[Operator]): DSLResult = {
    val result = super.toWhereQuery(context, operator)
    result.copy(query = s"[${result.query}]")
  }

  def or[U <: Product, UH <: HList](rel: U, properties: UH)(implicit queryProvider: QueryProvider[U],
                                                            i1: ToTraversable.Aux[UH, List, Symbol]): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(Relationship(rel, properties))
    MultiRelationType(List(RelationTypeOrInstance(tpe), relationTypeOrInstance))
  }

  def or(rel: RelationType): MultiRelationType = {
    val relationTypeOrInstance = RelationTypeOrInstance(rel)
    MultiRelationType(List(RelationTypeOrInstance(tpe), relationTypeOrInstance))
  }

  override def label(transform: (String) => String): String = upperSnakeCased(super.label(transform))

  def canEqual(a: Any) = a.isInstanceOf[RelationType]

  override def equals(that: Any): Boolean = {
    that match {
      case that: RelationType => that.canEqual(this) && this.tpe == that.tpe && this.fingerprint == that.fingerprint
      case _ => false
    }
  }

  override def hashCode(): Int = this.tpe.hashCode() + this.fingerprint.hashCode()
}

object RelationType {
  def apply(tpe: Type): RelationType = new RelationType(tpe, None)
  def apply(tpe: Type, variableLengthRelation: VariableLengthRelation): RelationType =
    new RelationType(tpe, Option(variableLengthRelation))
}
