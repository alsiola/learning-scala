package com.alsiola.ManagerStrings

import scala.util.parsing.combinator._

class ManagerStrings(attributes: Map[String, Map[String, String]])
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

  def authorizedSegmentGroup: Parser[List[List[String]]] =
    repsep(authorizedSegment, "+") ^^ {
      case Nil => Nil
      case x =>
        x.reduce((o, xn) => o.flatMap(e => xn.map(ex => e ++ ex)))
    }

  def authorizedSegments: Parser[List[List[String]]] =
    repsep(authorizedSegmentGroup, ",") ^^ { x => x.flatten }
}

object ManagerStrings {
  case class AttributeMap(val attributes: Map[String, Map[String, String]]) {}

  def parse(
      in: String
  )(implicit attributes: AttributeMap) = {
    val p = new ManagerStrings(attributes.attributes)
    p.parseAll(p.authorizedSegments, in)
  }
}
