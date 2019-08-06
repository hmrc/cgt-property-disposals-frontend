package uk.gov.hmrc.cgtpropertydisposalsfrontend.models

import cats.instances.char._
import cats.syntax.eq._

import play.api.data.Form
import play.api.data.Forms.{mapping, text}

final case class Postcode(value: String) extends AnyVal

object Postcode {

  val form: Form[Postcode] = Form(
    mapping(
      "postcode" -> text.verifying(
        "invalid",
        s => s.forall(c => c.isLetterOrDigit || c === ' ') && s.exists(_.isLetterOrDigit)
      )
    )(Postcode.apply)(Postcode.unapply)
  )

}
