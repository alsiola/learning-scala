package alsiola.Util

import scala.util.parsing.combinator._

object Util {
  def append[T](xs: List[T], ys: List[T]): List[T] = {
    xs match {
      case Nil          => ys
      case head :: tail => head :: append(tail, ys)
    }
  }

  private def _len(xs: List[Any], p: Int): Int = {
    xs match {
      case Nil    => p
      case h :: t => _len(t, p + 1)
    }
  }

  val len: (List[Any]) => Int = _len(_, 0)

  def parseJSON(s: String) = {
    val j = new JSONParser();
    j.parseAll(j.value, s)
  }
}

class JSONParser extends JavaTokenParsers {
  def value: Parser[Any] =
    (
      obj
        | arr
        | stringLiteral
        | "null" ^^ (x => null)
        | "true" ^^ (x => true)
        | "false" ^^ (x => false)
        | floatingPointNumber ^^ (_.toDouble)
    )

  def obj: Parser[Map[String, Any]] =
    "{" ~ repsep(member, ",") ~ "}" ^^ {
      case "{" ~ ms ~ "}" => Map() ++ ms
    }

  def arr: Parser[List[Any]] =
    "[" ~ repsep(value, ",") ~ "]" ^^ {
      case "[" ~ ms ~ "]" => List() ++ ms
    }

  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^ {
      case s ~ ":" ~ v => (s, v)
    }
}
