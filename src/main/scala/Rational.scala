package alsiola.Rational

case class Rational(n: Int, d: Int) {
  require(d != 0, "Denominator must be greater than zero")
  val g = gcd(n.abs, d.abs)
  val numerator = n / g
  val denominator = d / g

  override def toString(): String = s"Rational($numerator/$denominator)"
  override def equals(obj: Any): Boolean =
    obj match {
      case r: Rational =>
        numerator == r.numerator && denominator == r.denominator
      case _ => false
    }

  def this(n: Int) = this(n, 1)

  def +(other: Rational): Rational =
    Rational(
      numerator * other.denominator + other.numerator * denominator,
      denominator * other.denominator
    )

  def -(other: Rational): Rational = this + (-other)

  def unary_-(): Rational =
    Rational(
      -numerator,
      denominator
    )

  def *(other: Rational): Rational =
    Rational(
      numerator * other.numerator,
      denominator * other.denominator
    )

  def /(other: Rational): Rational = this * other.reciprocal

  def +(other: Int): Rational = this + Rational(other)
  def -(other: Int): Rational = this - Rational(other)
  def *(other: Int): Rational = this * Rational(other)
  def /(other: Int): Rational = this / Rational(other)
  def +(other: Double): Rational = this + Rational(other)
  def -(other: Double): Rational = this - Rational(other)
  def *(other: Double): Rational = this * Rational(other)
  def /(other: Double): Rational = this / Rational(other)

  def reciprocal = Rational(denominator, numerator)

  private def gcd(a: Int, b: Int): Int =
    b match {
      case 0 => a
      case _ => gcd(b, a % b)
    }
}

object Rational {

  implicit def intToRational(n: Int): Rational = Rational(n)
  implicit def floatToRational(n: Double): Rational = {
    var m = 1
    var a = n

    while (a - a.floor > 0) {
      a *= m
      m *= 10
    }

    Rational(a.toInt, m / 10)
  }
  def apply(n: Int, d: Int): Rational = new Rational(n, d)
  def apply(n: Int): Rational = apply(n, 1)
  def apply = floatToRational _
}

object Program extends App {
  val x = Rational(1, 5)

  println(0.4 + x)
}
