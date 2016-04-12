package xmless.syntax

import xmless.ToXmlElement

trait ToXmlSyntax {
  implicit class ToXmlElementOps[T](value: T)(implicit tx: ToXmlElement[T]) {
    def toXml = tx.toXml(value)
  }
}