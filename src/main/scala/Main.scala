import alsiola.Rational._
import alsiola.Util.Util

object Program extends App {
  val x = List(1, 2)
  val y = List(3, 4)

  val xy = Util.append(x, y)

  println(Util.len(xy))
}
