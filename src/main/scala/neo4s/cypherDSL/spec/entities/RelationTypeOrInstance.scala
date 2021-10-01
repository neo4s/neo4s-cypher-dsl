package neo4s.cypherDSL.spec.entities

import neo4s.cypherDSL.spec.{Context, DSLResult}
import neo4s.cypherDSL.spec.utils._
import shapeless.HList

import scala.reflect.runtime.universe._

sealed trait QueryableTypeOrInstance extends CypherEntity

class RelationTypeOrInstance(either: Either[RelationType, Relationship[_, _]]) extends QueryableTypeOrInstance {
  def toQuery(context: Context = new Context()): DSLResult = {
    either.fold(relationType => {
      DSLResult(relationType.label)
    }, relationship => {
      val result = relationship.toQuery(context)
      result.copy(query = result.query.stripSemanticSugar)
    })
  }

  override def toSetterQuery(context: Context = new Context()): DSLResult = {
    either.fold(relationType => {
      DSLResult(relationType.label)
    }, relationship => {
      val result = relationship.toSetterQuery(context)
      result.copy(query = result.query.stripSemanticSugar)
    })
  }
}
object RelationTypeOrInstance {
  def apply(tpe: RelationType): RelationTypeOrInstance =
    new RelationTypeOrInstance(Left(tpe))
  def apply(tpe: Type): RelationTypeOrInstance =
    new RelationTypeOrInstance(Left(RelationType(tpe)))
  def apply[T <: Product, TH <: HList](relationship: Relationship[T, TH]): RelationTypeOrInstance =
    new RelationTypeOrInstance(Right(relationship))
}
