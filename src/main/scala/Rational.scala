case class Rational(n: Int, d: Int) {
  require(d != 0)
  val g = gcd(n.abs, d.abs);
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

  def reciprocal = Rational(denominator, numerator)

  private def gcd(a: Int, b: Int): Int =
    b match {
      case 0 => a
      case _ => gcd(b, a % b)
    }
}

object Rational {
  implicit def intToRational(n: Int): Rational = Rational(n)
  def apply(n: Int, d: Int): Rational = new Rational(n, d)
  def apply(n: Int): Rational = new Rational(n)
}

object Program extends App {
  val x = Rational(1, 2)
  val y = Rational(2, 4);
  val z = Rational(4, 10)

  println(1 + x * y / z)
}
