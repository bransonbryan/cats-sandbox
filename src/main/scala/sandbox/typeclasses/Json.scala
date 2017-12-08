package sandbox.typeclasses

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

trait JsonWriter[A] {
  def writer(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] = (value: Person) => JsObject(Map(
    "name" -> JsString(value.name),
    "email" -> JsString(value.email)
  ))

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    (option: Option[A]) => option match {
      case Some(aValue) => writer.writer(aValue)
      case None => JsNull
    }
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.writer(value)
  }
}
