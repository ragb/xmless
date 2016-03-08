package xmless

import scala.annotation.implicitNotFound
import scala.xml._
import cats.Show
import cats.syntax.show._
import cats.std.{ AnyValInstances, StringInstances }
import shapeless._, labelled._

/**
 * Type classto serialise a value to XML
 *
 */
@implicitNotFound(msg = "Implicit ToXml[${T}] not found. Try supplying an implicit instance of ToXml[${T}]")
trait ToXml[-T] {
  def toXml(t: T): NodeSeq
}

object ToXml extends ToXmlLowPriorityInstances

trait ToXmlLowPriorityInstances extends ToXmlLowPriorityInstances1 {
  implicit def optionToXml[T](implicit tx: ToXml[T]): ToXml[Option[T]] = new ToXml[Option[T]] {
    def toXml(ot: Option[T]) = ot map tx.toXml getOrElse NodeSeq.Empty
  }
  implicit val nilToXml: ToXml[HNil] = new ToXml[HNil] {
    def toXml(t: HNil) = NodeSeq.Empty
  }

  implicit def labelledHListToXml[K <: Symbol, V, R <: HList](
    implicit
    key: Witness.Aux[K],
    vx: Lazy[ToXml[V]],
    rx: ToXml[R]
  ): ToXml[FieldType[K, V] :: R] = new ToXml[FieldType[K, V] :: R] {
    def toXml(l: FieldType[K, V] :: R) = {
      val head = Elem(null, key.value.name, Node.NoAttributes: MetaData, TopScope: NamespaceBinding, true, vx.value.toXml(l.head): _*)
      val tail = rx.toXml(l.tail)
      head ++ tail
    }
  }

}

trait ToXmlLowPriorityInstances1 extends AnyValInstances with StringInstances {
  import cats.syntax.show._
  implicit def fromShow[T](implicit show: Show[T]): ToXml[T] = new ToXml[T] {
    def toXml(t: T) = Text(t.show)
  }

}

trait RootToXml[-T] extends ToXml[T] {
  def toXml(t: T): Elem
}

object RootToXml extends RootToXmlLowPriorityInstances {

}

trait RootToXmlLowPriorityInstances {
  implicit def labeledGenericRootToXml[T, Repr](
    implicit
    lgen: LabelledGeneric.Aux[T, Repr],
    reprx: ToXml[Repr],
    tp: Typeable[T]
  ): RootToXml[T] = new RootToXml[T] {
    def toXml(t: T) = {
      val inner = reprx.toXml(lgen.to(t))
      val name = tp.describe
      Elem(null, name, Node.NoAttributes: MetaData, TopScope, true, inner: _*)
    }
  }

}

/**
 * Deserialise a T from XML.
 */
trait FromXml[+T] {
  def fromXml(n: NodeSeq): XmlResult[T]
}
