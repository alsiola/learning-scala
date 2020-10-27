package alsiola.Rational

trait Simplify extends Rational {
  abstract override def simplify(n: Int, d: Int) = {
    val div = gcd(n, d)
    (n / div, d / div)
  }

  private def gcd(a: Int, b: Int): Int =
    b match {
      case 0 => a
      case _ => gcd(b, a % b)
    }
}
