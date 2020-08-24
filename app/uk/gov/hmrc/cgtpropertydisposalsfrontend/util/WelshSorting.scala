package uk.gov.hmrc.cgtpropertydisposalsfrontend.util

object WelshSorting {

  implicit class WelshStringOps(private val s: String) extends AnyVal {

    def isBeforeInWelsh(other: String): Boolean = s.compare(other) < 0

  }

}
