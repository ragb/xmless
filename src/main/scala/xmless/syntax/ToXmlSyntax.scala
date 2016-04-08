package xmless.syntax

import xmless.{ ToXml, ToXmlElement }

trait ToXmlSyntax {
  implicit class ToXmlOps[T](value: T)(implicit tx: ToXml[T]) {
    def toXml = tx.toXml(value)
  }

  implicit class ToXmlElementOps[T](value: T)(implicit tx: ToXmlElement[T]) {
    def toXml = tx.toXml(value)
  }
}