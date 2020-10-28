package alsiola.Util

object Util {
  def append[T](xs: List[T], ys: List[T]): List[T] = {
    xs match {
      case Nil          => ys
      case head :: tail => head :: append(tail, ys)
    }
  }

  private def rLen(xs: List[Any], p: Int): Int = {
    xs match {
      case Nil    => p
      case h :: t => rLen(t, p + 1)
    }
  }

  val len: (List[Any]) => Int = rLen(_, 0)
}
