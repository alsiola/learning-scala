package com.alsiola.ManagerStrings

import scala.util.parsing.combinator._

class ManagerStringParser(attributes: Map[String, Map[String, String]])
    extends JavaTokenParsers {

  /**
    * context-free grammar:
    * entityName             ::= [a-zA-Z0-9\s,\?\!\\\/&]+
    * quote                  ::= "\""" | "'"
    * attribute              ::= [quote] entityName [quote].
    * segment                ::= quote (entityName | "*") quote.
    * authorizedSegment      ::= attribute ":" segment.
    * authorizedSegmentGroup ::= authorizedSegment {"+", authorizedSegment}
    * authorizedSegments     ::= authorizedSegmentGroup {"," authorizedSegmentGroup}.
    */

  def dQuote = "\""
  def sQuote = "'"
  def quote = dQuote | sQuote
  def optQuote = opt(quote)

  def entityName = """[a-zA-Z0-9\s,\?\!\\\/&]+""".r

  def attribute: Parser[Map[String, String]] =
    optQuote ~> entityName <~ optQuote ^^ { x =>
      attributes(x)
    }

  def segment: Parser[String] = "*" | quote ~> entityName <~ quote

  def authorizedSegment: Parser[List[List[String]]] =
    attribute ~ ":" ~ segment ^^ {
      case a ~ ":" ~ "*" => a.values.toList.map(List(_))
      case a ~ ":" ~ s   => List(List(a(s)))
    }

  /**
    * TODO: Abstract over this length pattern for arbitrary length of x
    */
  def authorizedSegmentGroup: Parser[List[List[List[String]]]] =
    repsep(authorizedSegment, "+") ^^ { x =>
      {
        if (x.length < 2) x
        else if (x.length < 3)
          for {
            elx <- x(0)
            ely <- x(1)
          } yield elx.flatMap(xx => ely.map(yy => List(xx, yy)))
        else {
          for {
            elx <- x(0)
            ely <- x(1)
            elz <- x(2)
          } yield elx.flatMap(xx =>
            ely.flatMap(yy => elz.map(zz => List(xx, yy, zz)))
          )
        }
      }
    }

  def authorizedSegments: Parser[List[List[String]]] =
    repsep(authorizedSegmentGroup, ",") ^^ { x => x.flatten.flatten }
}

object ManagerStringParser {
  def apply(
      in: String
  )(implicit attributes: Map[String, Map[String, String]]) = {
    val p = new ManagerStringParser(attributes)
    p.parseAll(p.authorizedSegments, in)
  }
}
