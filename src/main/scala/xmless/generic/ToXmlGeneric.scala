package xmless.generic

import shapeless._
import labelled.FieldType
import xmless.{ ToXml, ToXmlElement }
import scala.xml._

trait ToXmlGeneric extends ToXmlGenericLowPriorityInstances
trait ToXmlGenericLowPriorityInstances extends ToXmlHints {
  sealed abstract class WrapedToXml[Wraped, Repr]() {
    def toXml(r: Repr): NodeSeq
  }

  implicit def hnilWrapedToXml[Wraped](implicit tp: Typeable[Wraped]): WrapedToXml[Wraped, HNil] = new WrapedToXml[Wraped, HNil] {
    def toXml(h: HNil) = NodeSeq.Empty
  }

  implicit def hlistWrapedToXml[Wraped, Name <: Symbol, Head, Tail <: HList](
    implicit
    tp: Typeable[Wraped],
    name: Witness.Aux[Name],
    headx: Lazy[ToXml[Head]],
    tailx: WrapedToXml[Wraped, Tail],
    fx: Lazy[FieldToXml[Wraped]]
  ): WrapedToXml[Wraped, FieldType[Name, Head] :: Tail] = new WrapedToXml[Wraped, FieldType[Name, Head] :: Tail] {
    def toXml(l: FieldType[Name, Head] :: Tail) = fx.value.fieldToXml(name.value, headx.value.toXml(l.head)) ++ tailx.toXml(l.tail)
  }

  implicit def labelledGenericToXml[T, Repr](
    implicit
    tp: Typeable[T],
    lgen: LabelledGeneric.Aux[T, Repr],
    fx: Lazy[FieldToXml[T]],
    ox: ObjectToXml[T],
    wx: Lazy[WrapedToXml[T, Repr]]
  ): ToXmlElement[T] = new ToXmlElement[T] {
    def toXml(t: T) = {
      val xml = wx.value.toXml(lgen.to(t))
      ox.toXml(xml)
    }
  }

}

trait ToXmlHints {
  trait FieldToXml[Wraped] {
    def fieldToXml[Name <: Symbol](name: Name, xml: NodeSeq): NodeSeq
  }

  trait ObjectToXml[T] {
    def toXml(xml: NodeSeq): Elem
  }

  implicit def objectToXml[T](implicit tp: Typeable[T]): ObjectToXml[T] = new ObjectToXml[T] {
    def toXml(xml: NodeSeq) = Elem(null, tp.describe, Node.NoAttributes, TopScope, true, xml: _*)
  }
  implicit def FieldToXml[T: Typeable]: FieldToXml[T] = new FieldToXml[T] {
    def fieldToXml[Name <: Symbol](name: Name, xml: NodeSeq) = Elem(null, name.name.toString, Node.NoAttributes, TopScope, true, xml: _*)
  }
}
