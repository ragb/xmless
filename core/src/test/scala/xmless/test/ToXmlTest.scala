package xmless.test

import org.scalatest._
import xmless._, Defaults._
import cats.syntax.option._

class ToXmlTest extends FlatSpec with Matchers {
  def serialize[T](t: T)(implicit tx: ToXml[T]) = tx.toXml(t).toString

  "ToXml" should "Serialize primitive values to XML" in {
    serialize(1) shouldBe "1"
    serialize("hello") shouldBe "hello"
  }

  it should "Serialize option types" in {
    serialize(1.some) shouldBe "1"
    serialize((None: Option[Int])) shouldBe ""
  }

  it should "Serialize sequences of strings" in {
    serialize(List(1, 2, 3)) shouldBe "123"
  }
}