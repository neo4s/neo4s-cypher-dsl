package neo4s.cypher.dsl

import neo4s.cypher.dsl.entities.{NodeType, RelationType}
import neo4s.cypher.dsl.utils.SnakeCasing

import scala.collection.immutable.HashSet
import scala.collection.mutable

/** Context manages state of a [[Statement]] */
class Context(implicit modifiers: CypherBehavior = CypherBehavior()) extends SnakeCasing {
  private val namedReferences = mutable.Map.empty[String,mutable.Seq[Any]]
  private val corollaries = mutable.HashSet.empty[Set[Any]]

  private var literalQualifier = 0

  def getAndIncrementQualifier(): Int = {
    val result = literalQualifier
    literalQualifier += 1
    result
  }

  def correlate(l:Any, r:Any): Unit = {
    val correlation = corollaries.find(set => set.contains(l) || set.contains(r)).getOrElse(HashSet.empty[Any])
    corollaries.remove(correlation)
    corollaries.add(correlation + l + r)
  }

  def add[T](element: T): String = {
    val rawKey = element match {
      case nodeType: NodeType => nodeType.tpe.toString.split('.').last
      case relationType: RelationType => relationType.tpe.toString.split('.').last
      case _ => element.getClass.getSimpleName
    }

    val key = snakeCased(nodeIdTransform(rawKey))
    add(element,key)
  }

  def addLiteral[T](el: T, key: String): String = {
    // Map to corollary set
    val element = corollaries.find(_.contains(el)).getOrElse(el)
    val elements = namedReferences.getOrElseUpdate(key, mutable.ArraySeq())
    val (_,index) = elements
      .zipWithIndex
      .find { case (value,_) => value == element }
      .getOrElse {
        val updatedElements = elements :+ element
        namedReferences += (key -> updatedElements)
        (element, updatedElements.size - 1)
      }

    if (index == 0) key else s"${key}_${index}"
  }

  def add[T](element: T, rawKey: String): String = {
    val key = snakeCased(nodeIdTransform(rawKey))
    addLiteral(element,key)
  }

  def remove[T](element: T): Unit = {
    namedReferences
      .find { case (_,elements) => elements.contains(element) }
      .foreach { case (_,elements) =>
        val index = elements.indexOf(element)
        elements.update(index,None)
      }
  }

  def updateLiteral[T](element: T, key: String): String = {
    remove(element)

    val elements = namedReferences.getOrElseUpdate(key, mutable.ArraySeq())
    elements.indexOf(element) match {
      case -1 => addLiteral(element,key)
      case n => elements.update(n,element)
    }
    key
  }

  def update[T](element: T, rawKey: String): String = {
    val key = snakeCased(nodeIdTransform(rawKey))
    updateLiteral(element,key)
  }

  def clear: Unit = namedReferences.clear()

  def get[T](el: T): Option[String] = {
    val element = corollaries.find(_.contains(el)).getOrElse(el)
    namedReferences
      .find { case (_,elements) => elements.contains(element) }
      .map {
        case (id,elements) =>
          val index = elements.indexOf(element)
          if (index == 0) id else s"${id}_${elements.indexOf(element)}"
      }
  }

  def map[T, B](element: T)(f: String => B): Option[B] = get(element).map(f)

  def nodeIdTransform(nodeId: String): String = modifiers.nodeIdTransform(nodeId)
  def mapper: CypherBehavior.FieldTypeMapper = modifiers.mapper.orElse(CypherBehavior.defaultFieldMapper)

}
