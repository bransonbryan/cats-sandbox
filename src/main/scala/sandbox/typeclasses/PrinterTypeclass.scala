package sandbox.typeclasses

final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit =
    println(printable.format(value))
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (value: String) => value

  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString

  implicit val catPrintable: Printable[Cat] = (value: Cat) =>
    value.name + " is a " + value.age.toString + " year-old " + value.color + " cat."
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = println(printable.format(value))
  }
}

