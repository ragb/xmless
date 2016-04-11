package xmless

import scala.annotation.implicitNotFound
import scala.xml._
import cats.{ Foldable, Show }
import cats.std.{ AnyValInstances, BigDecimalInstances, OptionInstances, ListInstances }
import cats.syntax.show._
import cats.syntax.foldable._

/**
 * Type classto serialise a value to XML
 *
 */
@implicitNotFound(msg = "Implicit ToXml[${T}] not found. Try supplying an implicit instance of ToXml[${T}]")
trait ToXml[-T] {
  def toXml(t: T): NodeSeq
}

object ToXml {
  def apply[T](implicit instance: ToXml[T]) = instance
}

trait FromFoldableInstances extends ListInstances with OptionInstances {
  implicit def foldableToXml[F[_]: Foldable, T](implicit tx: ToXml[T]) = new ToXml[F[T]] {
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

trait DefaultToXmlInstances extends FromShowInstances with FromFoldableInstances with StringInstances

object Defaults extends DefaultToXmlInstances

/**
 * Type class representing serialisation of a type to a single XML element
 *
 */
trait ToXmlElement[-T] extends ToXml[T] {
  def toXml(t: T): Elem
}

object ToXmlElement {
  def apply[T](implicit instance: ToXmlElement[T]) = instance
}

