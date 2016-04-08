package xmless.generic

import shapeless._
import labelled.FieldType
import xmless.{ToXml, ToXmlElement}

import scala.xml._

trait ToXmlGeneric extends ToXmlGenericLowPriorityInstances 
private [generic] trait ToXmlGenericLowPriorityInstances extends ToXmlHints {
   abstract class WrapedToXml[Wraped, Repr](implicit tp: Typeable[Wraped]) {
     def toXml(r: Repr) : NodeSeq
   }
   
   implicit def hnilWrapedToXml[Wraped](implicit tp: Typeable[Wraped]): WrapedToXml[Wraped, HNil] = new WrapedToXml[Wraped, HNil] {
     def toXml(h: HNil) = NodeSeq.Empty
   }
   
   implicit def hlistWrapedToXml[Wraped, Name <: Symbol, Head, Tail <: HList](
       implicit tp: Typeable[Wraped],
       name: Witness.Aux[Name],
       headx: ToXml[Head],
       tailx: ToXml[Tail],
       fx: FieldToXml[Wraped]): WrapedToXml[Wraped, FieldType[Name, Head] :: Tail] = new WrapedToXml[Wraped, FieldType[Name, Head] :: Tail] {
     def toXml(l: FieldType[Name, Head] :: Tail) = fx.fieldToXml(name.value, headx.toXml(l.head)) ++ tailx.toXml(l.tail)
   }
   
   implicit def labeledGenericToXml[T, Repr](
       implicit lgen: LabelledGeneric.Aux[T, Repr],
       ox: ObjectToXml[T],
       rx: WrapedToXml[T, Repr],
       tp: Typeable[T]): ToXml[T] = new ToXml[T] {
     def toXml(t: T) = {
         val xml = rx.toXml(lgen.to(t))
    ox.toXml(tp, xml)    
       }
   }
}

trait FieldToXml[Wraped] {
  def fieldToXml[Name <: Symbol](name: Name, xml: NodeSeq): NodeSeq
}

trait ObjectToXml[T] {
  def toXml(tp: Typeable[T], xml: NodeSeq): Elem
}

private [generic] trait ToXmlHints {
  implicit def objectToXml[T] = new ObjectToXml[T] {
    def toXml(tp: Typeable[T], xml: NodeSeq) = Elem(null, tp.describe, Node.NoAttributes, TopScope, true, xml:_*)
  }
  implicit def fieldToXml[T] = new FieldToXml[T] {
    def fieldToXml[Name <: Symbol](name: Name, xml: NodeSeq) = Elem(null, name.name.toString, Node.NoAttributes, TopScope, true, xml:_*)
  }
}
