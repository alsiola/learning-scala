package alsiola.Rational

trait Simplify extends Rational {
  abstract override def simplify(n: Int, d: Int) = {
    val div = gcd(n, d)
    (n / div, d / div)
  }

  private val gcd: (Int, Int) => Int = {
    case (a, 0) => a
    case (a, b) => gcd(b, a % b)
  }
}
