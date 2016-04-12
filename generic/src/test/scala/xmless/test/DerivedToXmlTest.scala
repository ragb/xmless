package xmless.test

import org.scalatest._

import xmless.ToXmlElement
import xmless.generic.DerivedToXmlElement.exports._
import xmless.Defaults._

case class Person(name: String)
case class Persons(l: List[Person])
case class A(l: List[A])

sealed trait C
case class C1(f: String) extends C
case class C2(f: Int) extends C
case class C3(f: Float) extends C

class DerivedToXmlTest extends FlatSpec with Matchers {
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

  it should "Derive toXml on recursive case classes" in {
    val a = A(List(A(List())))
    serialize(a) shouldBe "<A><l><A><l/></A></l></A>"
  }

  it should "Derive ToXml for Coproducts" in {
    val c1 = C1("John")
    val c2 = C2(1)
    val c3 = C3(1.0f)
    serialize(c1: C) shouldBe "<C1><f>John</f></C1>"
  }
}