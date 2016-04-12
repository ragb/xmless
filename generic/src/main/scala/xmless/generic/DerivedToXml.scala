package xmless.generic

import xmless._
import scala.xml._
import export._
import shapeless._, labelled.FieldType

trait DerivedToXmlElement[T] extends ToXmlElement[T]

@exports
object DerivedToXmlElement extends ToXmlHints {
  sealed abstract class WrapedToXml[Wraped, Repr]() {
    type Ret <: NodeSeq
    def toXml(r: Repr): Ret
  }

  object WrapedToXml {
    type Aux[Wraped, Repr, R] = WrapedToXml[Wraped, Repr] { type Ret = R }
  }

  implicit def hnilWrapedToXml[Wraped](implicit tp: Typeable[Wraped]): WrapedToXml.Aux[Wraped, HNil, NodeSeq] = new WrapedToXml[Wraped, HNil] {
    type Ret = NodeSeq
    def toXml(h: HNil) = NodeSeq.Empty
  }

  implicit def hlistWrapedToXml[Wraped, Name <: Symbol, Head, Tail <: HList](
    implicit
    tp: Typeable[Wraped],
    name: Witness.Aux[Name],
    headx: Lazy[ToXml[Head]],
    tailx: WrapedToXml.Aux[Wraped, Tail, NodeSeq],
    fx: FieldToXml[Wraped]
  ): WrapedToXml.Aux[Wraped, FieldType[Name, Head] :: Tail, NodeSeq] = new WrapedToXml[Wraped, FieldType[Name, Head] :: Tail] {
    type Ret = NodeSeq
    def toXml(l: FieldType[Name, Head] :: Tail) = fx.fieldToXml(name.value, headx.value.toXml(l.head)) ++ tailx.toXml(l.tail)
  }

  implicit def cnilWrapedToXml[Wraped](implicit tp: Typeable[Wraped]) = new WrapedToXml[Wraped, CNil] {
    type Ret = Elem
    def toXml(c: CNil) = sys.error("this should never happen")
  }

  implicit def coproductWrapedToXml[Wraped, Name <: Symbol, Head, Remaining <: Coproduct](
    implicit
    tp: Typeable[Wraped],
    name: Witness.Aux[Name],
    headx: Lazy[ToXmlElement[Head]],
    tailx: WrapedToXml.Aux[Wraped, Remaining, Elem],
    cx: CoproductToXml[Wraped]
  ): WrapedToXml.Aux[Wraped, FieldType[Name, Head] :+: Remaining, Elem] = new WrapedToXml[Wraped, FieldType[Name, Head] :+: Remaining] {
    type Ret = Elem
    def toXml(cp: FieldType[Name, Head] :+: Remaining) = cp match {
      case Inl(h) => cx.addHint(name.value, headx.value.toXml(h))
      case Inr(t) => tailx.toXml(t)
    }
  }

  implicit def labelledCoproductToXml[T, Repr <: Coproduct](
    implicit
    tp: Typeable[T],
    lgen: LabelledGeneric.Aux[T, Repr],
    wx: Lazy[WrapedToXml.Aux[T, Repr, Elem]]
  ): ToXmlElement[T] = new ToXmlElement[T] {
    def toXml(t: T) = wx.value.toXml(lgen.to(t))
  }

  implicit def labelledProductToXml[T, Repr <: HList](
    implicit
    tp: Typeable[T],
    lgen: LabelledGeneric.Aux[T, Repr],
    ox: ObjectToXml[T],
    wx: Lazy[WrapedToXml[T, Repr]]
  ): ToXmlElement[T] = new ToXmlElement[T] {
    def toXml(t: T) = {
      val xml = wx.value.toXml(lgen.to(t))
      ox.toXml(xml)
    }
  }

}

trait FieldToXml[Wraped] {
  def fieldToXml[Name <: Symbol](name: Name, xml: NodeSeq): NodeSeq
}

trait ObjectToXml[T] {
  def toXml(xml: NodeSeq): Elem
}

trait CoproductToXml[T] {
  def addHint[Name <: Symbol](name: Name, e: Elem): Elem
}

trait ToXmlHints {
  implicit def objectToXml[T](implicit tp: Typeable[T]): ObjectToXml[T] = new ObjectToXml[T] {
    def toXml(xml: NodeSeq) = Elem(null, tp.describe, Node.NoAttributes, TopScope, true, xml: _*)
  }
  implicit def FieldToXml[T: Typeable]: FieldToXml[T] = new FieldToXml[T] {
    def fieldToXml[Name <: Symbol](name: Name, xml: NodeSeq) = Elem(null, name.name.toString, Node.NoAttributes, TopScope, true, xml: _*)
  }
  implicit def coproductToXml[T: Typeable] = new CoproductToXml[T] {
    def addHint[Name <: Symbol](name: Name, e: Elem) = e
  }
}
