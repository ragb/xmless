package xmless.test

import org.scalatest._
import scala.xml._
import xmless._
import xmless.syntax.toXml._
import Defaults._
import cats.syntax.option._

class ToXmlTest extends FlatSpec with Matchers {
  def serialize(xml: NodeSeq) = xml.toString

  "ToXml" should "Serialize primitive values to XML" in {
    serialize(1.toXml) shouldBe "1"
    serialize("hello".toXml) shouldBe "hello"
  }

  it should "Serialize option types" in {
    serialize(1.some.toXml) shouldBe "1"
    serialize((None: Option[Int]).toXml) shouldBe ""
  }

  it should "Serialize sequences of strings" in {
    serialize(List(1, 2, 3).toXml) shouldBe "123"
  }
}