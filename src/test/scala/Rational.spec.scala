package alsiola.Rational

import org.scalatest._
import flatspec._
import matchers._

class RationalSpec extends AnyFlatSpec with should.Matchers {

  "Rational" should "print a nice string" in {
    val r = Rational(1, 2)

    r.toString should include("Rational(1/2)")
  }

  "Rational" should "be normalised" in {
    val r = Rational(4, 8)

    r should equal(Rational(1, 2))
  }

  "Rational" should "throw InvalidArgumentException if denominator is zero" in {
    a[IllegalArgumentException] should be thrownBy {
      Rational(1, 0)
    }
  }
}
