package uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CountrySpec extends AnyWordSpec with Matchers {

  "Country formatter" should {

    "bind a valid country code" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "IN")
      )

      result shouldBe Right(Country("IN"))
    }

    "return error.notFound for an invalid country code" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "XYZ")
      )

      result.left.toOption.get.head.message shouldBe "error.notFound"
    }

    "reject input exceeding 10 characters" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "12345678901")
      )

      result.left.toOption.get.head.message shouldBe "error.notFound"
    }

    "return error.required for empty input" in {
      val result = Country.formatter.bind(
        "country",
        Map("country" -> "")
      )

      result.left.toOption.get.head.message shouldBe "error.required"
    }

    "return error.required when country is missing" in {
      val result = Country.formatter.bind(
        "country",
        Map.empty
      )
      result.left.toOption.get.head.message shouldBe "error.required"
    }
  }
}
