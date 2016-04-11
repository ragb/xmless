package xmless.test

import org.scalatest._
import xmless._, generic.toXml._
import Defaults._

case class Person(name: String)
case class Persons(l: List[Person])

class ToXmlGenericTest extends FlatSpec with Matchers {
  def serialize[T](t: T)(implicit tx: ToXmlElement[T]) = tx.toXml(t).toString

  "xmless.generic" should "Derive ToXml for case classes with primitive fields" in {
    val p = Person("John")
    serialize(p) shouldBe "<Person><name>John</name></Person>"
  }

  it should "Derive ToXml for case classes with lists" in {
    val p1 = Person("John")
    val p2 = Person("Richard")
    val s = Persons(List(p1, p2))
    serialize(s) shouldBe """<Persons><l><Person><name>John</name></Person><Person><name>Richard</name></Person></l></Persons>"""

  }
}