package alsiola.Rational

case class Rational(n: Int, d: Int) extends Ordered[Rational] {

  override def compare(that: Rational): Int = {
    (this.numerator * that.denominator) - (that.numerator * this.denominator)
  }

  require(d != 0, s"Denominator must be greater than zero, received $n / $d")
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

  private def ~=(a: Double, b: Double): Boolean = {
    Math.abs(a - b) < 0.0001
  }

  private def floatToRational(
      a: Double,
      r: Int
  ): Rational =
    (a - a.floor) match {
      case rem if ~=(rem, 0) => Rational(a.toInt, r)
      case _                 => floatToRational(10 * a, 10 * r)
    }

  implicit def intToRational(n: Int): Rational = Rational(n)
  implicit def floatToRational(n: Double): Rational = floatToRational(n, 1)
  def apply(n: Int, d: Int): Rational = new Rational(n, d)
  def apply(n: Int): Rational = apply(n, 1)
  def apply(n: Double) = floatToRational(n, 1)
}

object Program extends App {
  val y = Rational(13.43)
  println(y)
}
