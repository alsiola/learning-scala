package com.alsiola.ManagerStrings

import org.scalatest._
import flatspec._
import matchers._

class ManagerStringSpec extends AnyFlatSpec with should.Matchers {
  implicit val attributes = ManagerStrings.AttributeMap(
    Map(
      "Department" -> Map(
        "Sales" -> "salesid",
        "Marketing" -> "marketingid",
        "P, E and D" -> "pedid"
      ),
      "Location" -> Map(
        "Newcastle" -> "newcastleid",
        "London" -> "londonid"
      ),
      "Gender" -> Map(
        "Male" -> "maleid",
        "Female" -> "femaleid"
      )
    )
  )

  "ManagerString parsing" should "return an empty list for empty string input" in {
    val i = ""

    val actual = ManagerStrings.parse(i).get

    val expected = List()

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse a simple att:seg pair" in {
    val i = "Department:'Sales'"

    val actual = ManagerStrings.parse(i).get

    val expected = List(List("salesid"))

    actual shouldEqual expected
  }

  "ManagerString parsing" should "handle segments with commas" in {
    val i = "Department:'P, E and D'"

    val actual = ManagerStrings.parse(i).get

    val expected = List(List("pedid"))

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse multiple att:seg pairs" in {
    val i = "Department:'Sales',Department:'Marketing'"

    val actual = ManagerStrings.parse(i).get

    val expected = List(List("salesid"), List("marketingid"))

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse att:* pairs" in {
    val i = "Department:*"

    val actual = ManagerStrings.parse(i).get

    val expected = List(
      List("salesid"),
      List("marketingid"),
      List("pedid")
    )

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse combined att:seg pairs" in {
    val i = "Department:'Sales'+Location:'London'"

    val actual = ManagerStrings.parse(i).get

    val expected = List(List("salesid", "londonid"))

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse a complex string" in {
    val i = "Department:'Sales'+Location:*"

    val actual = ManagerStrings.parse(i).get

    val expected =
      List(List("salesid", "newcastleid"), List("salesid", "londonid"))

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse another complex string" in {
    val i = "Department:*+Location:*"

    val actual = ManagerStrings.parse(i).get

    val expected =
      List(
        List("salesid", "newcastleid"),
        List("salesid", "londonid"),
        List("marketingid", "newcastleid"),
        List("marketingid", "londonid"),
        List("pedid", "newcastleid"),
        List("pedid", "londonid")
      )

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse ridiculously complex string" in {
    val i = "Department:*+Location:*+Gender:*"

    val actual = ManagerStrings.parse(i).get

    val expected =
      List(
        List("salesid", "newcastleid", "maleid"),
        List("salesid", "newcastleid", "femaleid"),
        List("salesid", "londonid", "maleid"),
        List("salesid", "londonid", "femaleid"),
        List("marketingid", "newcastleid", "maleid"),
        List("marketingid", "newcastleid", "femaleid"),
        List("marketingid", "londonid", "maleid"),
        List("marketingid", "londonid", "femaleid"),
        List("pedid", "newcastleid", "maleid"),
        List("pedid", "newcastleid", "femaleid"),
        List("pedid", "londonid", "maleid"),
        List("pedid", "londonid", "femaleid")
      )

    actual shouldEqual expected
  }

  "ManagerString parsing" should "parse another ridiculously complex string" in {
    val i = "Department:*+Location:*+Gender:'Male'"

    val actual = ManagerStrings.parse(i).get

    val expected =
      List(
        List("salesid", "newcastleid", "maleid"),
        List("salesid", "londonid", "maleid"),
        List("marketingid", "newcastleid", "maleid"),
        List("marketingid", "londonid", "maleid"),
        List("pedid", "newcastleid", "maleid"),
        List("pedid", "londonid", "maleid")
      )

    actual shouldEqual expected
  }
}
