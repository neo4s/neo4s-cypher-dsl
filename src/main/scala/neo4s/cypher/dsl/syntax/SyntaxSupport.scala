package neo4s.cypher.dsl.syntax

import neo4s.cypher.dsl.QueryProvider
import neo4s.cypher.dsl.entities.Node
import shapeless.{HList, HNil, LabelledGeneric, Witness}
import shapeless.ops.record.Selector

import scala.annotation.implicitNotFound

trait SyntaxSupport {
  @implicitNotFound(msg = "${PName} not found in ${T}")
  trait PropertyExists[T, PName, PType]

  object PropertyExists {
    def apply[T, PType](column: Witness)(
      implicit exists: PropertyExists[T, column.T, PType]): PropertyExists[T, column.T, PType] =
      exists

    implicit def implicitProvider[T, H <: HList, PName, PType](
                                                                implicit
                                                                gen: LabelledGeneric.Aux[T, H],
                                                                selector: Selector.Aux[H, PName, PType]
                                                              ): PropertyExists[T, PName, PType] = new PropertyExists[T, PName, PType] {}
  }

  implicit class RichNode[T <: Product](element: T) {

    def apply[TP](p1: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                          sp1: PropertyExists[T, p1.T, TP]) = {
      Node(element, p1.value :: HNil)
    }

    def apply[TP, TP2](p1: Witness.Lt[Symbol], p2: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                       sp1: PropertyExists[T, p1.T, TP],
                                                                       sp2: PropertyExists[T, p2.T, TP2]) = {
      Node(element, p1.value :: p2.value :: HNil)
    }

    def apply[TP, TP2, TP3](p1: Witness.Lt[Symbol], p2: Witness.Lt[Symbol], p3: Witness.Lt[Symbol])(
      implicit queryProvider: QueryProvider[T],
      sp1: PropertyExists[T, p1.T, TP],
      sp2: PropertyExists[T, p2.T, TP2],
      sp3: PropertyExists[T, p3.T, TP3]) = {
      Node(element, p1.value :: p2.value :: p3.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4](p1: Witness.Lt[Symbol],
                                 p2: Witness.Lt[Symbol],
                                 p3: Witness.Lt[Symbol],
                                 p4: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                         sp1: PropertyExists[T, p1.T, TP],
                                                         sp2: PropertyExists[T, p2.T, TP2],
                                                         sp3: PropertyExists[T, p3.T, TP3],
                                                         sp4: PropertyExists[T, p4.T, TP4]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5](p1: Witness.Lt[Symbol],
                                      p2: Witness.Lt[Symbol],
                                      p3: Witness.Lt[Symbol],
                                      p4: Witness.Lt[Symbol],
                                      p5: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                              sp1: PropertyExists[T, p1.T, TP],
                                                              sp2: PropertyExists[T, p2.T, TP2],
                                                              sp3: PropertyExists[T, p3.T, TP3],
                                                              sp4: PropertyExists[T, p4.T, TP4],
                                                              sp5: PropertyExists[T, p5.T, TP5]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6](p1: Witness.Lt[Symbol],
                                           p2: Witness.Lt[Symbol],
                                           p3: Witness.Lt[Symbol],
                                           p4: Witness.Lt[Symbol],
                                           p5: Witness.Lt[Symbol],
                                           p6: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                   sp1: PropertyExists[T, p1.T, TP],
                                                                   sp2: PropertyExists[T, p2.T, TP2],
                                                                   sp3: PropertyExists[T, p3.T, TP3],
                                                                   sp4: PropertyExists[T, p4.T, TP4],
                                                                   sp5: PropertyExists[T, p5.T, TP5],
                                                                   sp6: PropertyExists[T, p6.T, TP6]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7](p1: Witness.Lt[Symbol],
                                                p2: Witness.Lt[Symbol],
                                                p3: Witness.Lt[Symbol],
                                                p4: Witness.Lt[Symbol],
                                                p5: Witness.Lt[Symbol],
                                                p6: Witness.Lt[Symbol],
                                                p7: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                        sp1: PropertyExists[T, p1.T, TP],
                                                                        sp2: PropertyExists[T, p2.T, TP2],
                                                                        sp3: PropertyExists[T, p3.T, TP3],
                                                                        sp4: PropertyExists[T, p4.T, TP4],
                                                                        sp5: PropertyExists[T, p5.T, TP5],
                                                                        sp6: PropertyExists[T, p6.T, TP6],
                                                                        sp7: PropertyExists[T, p7.T, TP7]) = {
      Node(element, p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7, TP8](p1: Witness.Lt[Symbol],
                                                     p2: Witness.Lt[Symbol],
                                                     p3: Witness.Lt[Symbol],
                                                     p4: Witness.Lt[Symbol],
                                                     p5: Witness.Lt[Symbol],
                                                     p6: Witness.Lt[Symbol],
                                                     p7: Witness.Lt[Symbol],
                                                     p8: Witness.Lt[Symbol])(implicit queryProvider: QueryProvider[T],
                                                                             sp1: PropertyExists[T, p1.T, TP],
                                                                             sp2: PropertyExists[T, p2.T, TP2],
                                                                             sp3: PropertyExists[T, p3.T, TP3],
                                                                             sp4: PropertyExists[T, p4.T, TP4],
                                                                             sp5: PropertyExists[T, p5.T, TP5],
                                                                             sp6: PropertyExists[T, p6.T, TP6],
                                                                             sp7: PropertyExists[T, p7.T, TP7],
                                                                             sp8: PropertyExists[T, p8.T, TP8]) = {
      Node(element,
        p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: p8.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7, TP8, TP9](p1: Witness.Lt[Symbol],
                                                          p2: Witness.Lt[Symbol],
                                                          p3: Witness.Lt[Symbol],
                                                          p4: Witness.Lt[Symbol],
                                                          p5: Witness.Lt[Symbol],
                                                          p6: Witness.Lt[Symbol],
                                                          p7: Witness.Lt[Symbol],
                                                          p8: Witness.Lt[Symbol],
                                                          p9: Witness.Lt[Symbol])(
                                                           implicit queryProvider: QueryProvider[T],
                                                           sp1: PropertyExists[T, p1.T, TP],
                                                           sp2: PropertyExists[T, p2.T, TP2],
                                                           sp3: PropertyExists[T, p3.T, TP3],
                                                           sp4: PropertyExists[T, p4.T, TP4],
                                                           sp5: PropertyExists[T, p5.T, TP5],
                                                           sp6: PropertyExists[T, p6.T, TP6],
                                                           sp7: PropertyExists[T, p7.T, TP7],
                                                           sp8: PropertyExists[T, p8.T, TP8],
                                                           sp9: PropertyExists[T, p9.T, TP9]) = {
      Node(
        element,
        p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: p8.value :: p9.value :: HNil)
    }

    def apply[TP, TP2, TP3, TP4, TP5, TP6, TP7, TP8, TP9, TP10](p1: Witness.Lt[Symbol],
                                                                p2: Witness.Lt[Symbol],
                                                                p3: Witness.Lt[Symbol],
                                                                p4: Witness.Lt[Symbol],
                                                                p5: Witness.Lt[Symbol],
                                                                p6: Witness.Lt[Symbol],
                                                                p7: Witness.Lt[Symbol],
                                                                p8: Witness.Lt[Symbol],
                                                                p9: Witness.Lt[Symbol],
                                                                p10: Witness.Lt[Symbol])(
                                                                 implicit queryProvider: QueryProvider[T],
                                                                 sp1: PropertyExists[T, p1.T, TP],
                                                                 sp2: PropertyExists[T, p2.T, TP2],
                                                                 sp3: PropertyExists[T, p3.T, TP3],
                                                                 sp4: PropertyExists[T, p4.T, TP4],
                                                                 sp5: PropertyExists[T, p5.T, TP5],
                                                                 sp6: PropertyExists[T, p6.T, TP6],
                                                                 sp7: PropertyExists[T, p7.T, TP7],
                                                                 sp8: PropertyExists[T, p8.T, TP8],
                                                                 sp9: PropertyExists[T, p9.T, TP9],
                                                                 sp10: PropertyExists[T, p10.T, TP9]) = {
      Node(
        element,
        p1.value :: p2.value :: p3.value :: p4.value :: p5.value :: p6.value :: p7.value :: p8.value :: p9.value :: p10.value :: HNil)
    }

  }

}
