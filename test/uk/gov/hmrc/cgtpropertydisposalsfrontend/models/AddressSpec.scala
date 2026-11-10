/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.Form
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.Address.{UkAddress, addressLineMaxLength}
import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.address.{Address, Postcode}
import play.api.data.Forms.mapping

class AddressSpec extends AnyWordSpec with Matchers {

  private val addressLineForm = Form(
    mapping(
      "addressLine" -> Address.addressLineMapping
    )(identity)(Some(_))
  )

  private def bindAddressLine(value: String) =
    addressLineForm.bind(Map("addressLine" -> value))

  private val postcodeForm = Form(
    mapping(
      "postcode" -> Address.nonUkPostcodeMapping
    )(identity)(Some(_))
  )

  private def bindPostcode(value: String) =
    postcodeForm.bind(Map("postcode" -> value))

  "Address" must {

    "have a country code method on uk addresses which always returns the same value" in {
      val ukAddress = UkAddress("line1", None, None, None, Postcode("postcode"))
      ukAddress.countryCode shouldBe "GB"
    }

    "accept a valid address" in {
      val result = bindAddressLine("123 High Street")

      result.hasErrors shouldBe false
      result.get       shouldBe "123 High Street"
    }

    "accept allowed special characters" in {
      val result = bindAddressLine("Flat 2, King's Road & 12/14-A.")

      result.hasErrors shouldBe false
    }

    "reject an address exceeding the maximum length" in {
      val address = "A" * (Address.addressLineMaxLength + 1)

      val result = bindAddressLine(address)

      result.hasErrors           shouldBe true
      result.errors.head.message shouldBe "error.tooLong"
    }

    "accept an address at the maximum length" in {
      val address = "A" * Address.addressLineMaxLength

      val result = bindAddressLine(address)

      result.hasErrors shouldBe false
    }

    "reject invalid special characters" in {
      val result = bindAddressLine("123 High Street @!")

      result.hasErrors           shouldBe true
      result.errors.head.message shouldBe "error.pattern"
    }

    "reject HTML script input" in {
      val result = bindAddressLine("<script>alert(1)</script>")

      result.hasErrors           shouldBe true
      result.errors.head.message shouldBe "error.pattern"
    }

    "trim leading and trailing spaces" in {
      val result = bindAddressLine("  123 High Street  ")

      result.hasErrors shouldBe false
      result.get       shouldBe "123 High Street"
    }

    "reject an empty address" in {
      val result = bindAddressLine("")

      result.hasErrors shouldBe true
    }

    "reject an address containing only spaces" in {
      val result = bindAddressLine("   ")

      result.hasErrors shouldBe true
    }

    "reject a missing address" in {
      val result = addressLineForm.bind(Map.empty[String, String])

      result.hasErrors shouldBe true
    }

    "reject an empty value" in {
      bindAddressLine("").errors.map(_.message) shouldBe Seq("error.required")
    }

    "reject postcode" in {
      bindAddressLine("").errors.map(_.message) shouldBe Seq("error.required")
    }

    "nonUkPostcodeMapping" should {

      "accept a valid postcode" in {
        bindPostcode("12345-ABC").hasErrors shouldBe false
      }

      "trim leading and trailing spaces before validation" in {
        bindPostcode("  12345-ABC  ").value shouldBe Some("12345-ABC")
      }

      "accept a postcode of exactly the maximum length" in {
        val postcode = "a" * addressLineMaxLength

        bindPostcode(postcode).hasErrors shouldBe false
      }

      "reject a postcode longer than the maximum length" in {
        val postcode = "a" * (addressLineMaxLength + 1)

        bindPostcode(postcode).hasErrors           shouldBe true
        bindPostcode(postcode).errors.head.message shouldBe "error.tooLong"
      }

      "reject a postcode containing invalid characters" in {
        val result = bindPostcode("ABC@123")

        result.hasErrors           shouldBe true
        result.errors.head.message shouldBe "error.pattern"
      }

      "reject an empty postcode" in {
        bindPostcode("").hasErrors shouldBe true
      }

      "reject a postcode containing only spaces" in {
        bindPostcode("   ").hasErrors shouldBe true
      }

      "reject characters that are not allowed" in {
        val result = bindPostcode("<script>alert(1)</script>")

        result.hasErrors           shouldBe true
        result.errors.head.message shouldBe "error.pattern"
      }
    }
  }
}
