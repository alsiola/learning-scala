package alsiola.Rational

import alsiola.Precision.Simplify

case class Rational(n: Int, d: Int) extends Ordered[Rational] {
  require(d != 0, s"Denominator must be greater than zero, received $n / $d")
  val (num, den) = simplify(n, d)
  val numerator = num
  val denominator = den

  def simplify(num: Int, den: Int) = (num, den)

  override def toString(): String = s"Rational($numerator/$denominator)"

  override def equals(obj: Any): Boolean =
    obj match {
      case r: Rational =>
        numerator == r.numerator && denominator == r.denominator
      case _ => false
    }

  override def compare(that: Rational): Int = {
    (this.numerator * that.denominator) - (that.numerator * this.denominator)
  }

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

  def +(other: Double): Rational = this + Rational(other)
  def -(other: Double): Rational = this - Rational(other)
  def *(other: Double): Rational = this * Rational(other)
  def /(other: Double): Rational = this / Rational(other)

  def reciprocal = Rational(denominator, numerator)
}

object Rational {
  private implicit class CloseDouble(val n: Double) {
    def ~=(that: Double) = Math.abs(n - that) < 0.0001
  }

  def floatToRational(
      a: Double,
      d: Int
  ): (Int, Int) =
    if (0 ~= a - a.floor)
      (a.toInt, d)
    else
      floatToRational(10 * a, 10 * d)

  implicit def intToRational(n: Int): Rational = Rational(n)
  implicit def floatToRational(n: Double): Rational = apply(n)
  def apply(n: Int, d: Int): Rational = new Rational(n, d)
  def apply(n: Int): Rational = apply(n, 1)
  def apply(n: Double): Rational = {
    val (nu, de) = floatToRational(n, 1)
    apply(nu, de)
  }
}

class SimpleRational(n: Int, d: Int) extends Rational(n, d) with Simplify
object SimpleRational {
  def apply(n: Int, d: Int): SimpleRational = new SimpleRational(n, d)
  def apply(n: Int): SimpleRational = apply(n, 1)
  def apply(n: Double): SimpleRational = {
    val (nu, de) = Rational.floatToRational(n, 1)
    apply(nu, de)
  }
}

object Program extends App {
  val x = Rational(0.2)
  val y = SimpleRational(0.2)
  println(x)
  println(y)
}
