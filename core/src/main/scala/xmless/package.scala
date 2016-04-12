import cats.data.ValidatedNel

package object xmless {
  type XmlResult[T] = ValidatedNel[String, T]

}