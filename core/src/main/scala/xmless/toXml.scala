package xmless

import scala.annotation.implicitNotFound
import scala.xml._
import cats.{ Foldable, Show }
import cats.std.{ AnyValInstances, BigDecimalInstances, OptionInstances, ListInstances }
import cats.syntax.show._
import cats.syntax.foldable._
import export._

/**
 * Type classto serialise a value to XML
 *
 */
@implicitNotFound(msg = "Implicit ToXml[${T}] not found. Try supplying an implicit instance of ToXml[${T}]")
trait ToXml[-T] {
  def toXml(t: T): NodeSeq
}

object ToXml extends ToXmlLowPriorityInstances {
  def apply[T](implicit instance: ToXml[T]) = instance
}

@imports[ToXml]
trait ToXmlLowPriorityInstances

trait FromFoldableInstances extends ListInstances with OptionInstances {
  implicit def fromFoldable[F[_]: Foldable, T](implicit tx: ToXml[T]) = new ToXml[F[T]] {
    def toXml(f: F[T]) = f.foldLeft(NodeSeq.Empty)((ac: NodeSeq, elem: T) => ac ++ tx.toXml(elem))
  }
}

trait FromShowInstances extends AnyValInstances with BigDecimalInstances {
  implicit def fromShow[T](implicit show: Show[T]): ToXml[T] = new ToXml[T] {
    def toXml(t: T) = Text(t.show)
  }
}

trait StringInstances {
  // Make a special case for string:
  implicit val stringToXml: ToXml[String] = new ToXml[String] {
    def toXml(s: String) = Text(s)
  }
}

/**
 * Type class representing serialisation of a type to a single XML element
 *
 */
@implicitNotFound(msg = "Implicit ToXmlElement[${T}] not found. Check your implicit scope for problems.")
trait ToXmlElement[-T] extends ToXml[T] {
  def toXml(t: T): Elem
}

object ToXmlElement extends ToXmlElementLowPriorityInstances {
  def apply[T](implicit instance: ToXmlElement[T]) = instance
}

@imports[ToXmlElement]
trait ToXmlElementLowPriorityInstances

trait DefaultToXmlInstances extends FromShowInstances with FromFoldableInstances with StringInstances

object Defaults extends DefaultToXmlInstances

