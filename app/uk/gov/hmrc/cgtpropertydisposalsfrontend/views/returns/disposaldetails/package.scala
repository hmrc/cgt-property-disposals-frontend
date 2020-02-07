/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.cgtpropertydisposalsfrontend.views.returns

import uk.gov.hmrc.cgtpropertydisposalsfrontend.models.returns.{DisposalMethod, ShareOfProperty}

package object disposaldetails {

  def disposalFeesTitleKey(disposalMethod: DisposalMethod, shareOfProperty: ShareOfProperty): String =
    (disposalMethod, shareOfProperty) match {
      case (DisposalMethod.Sold, ShareOfProperty.Full) => "disposalFees.full-share.sold-it.title"
      case (DisposalMethod.Sold, _)                    => "disposalFees.not-full-share.sold-it.title"
      case (_, ShareOfProperty.Full)                   => "disposalFees.full-share.gifted-it.title"
      case (_, _)                                      => "disposalFees.not-full-share.gifted-it.title"
    }

  def disposalPriceTitleKey(disposalMethod: DisposalMethod, shareOfProperty: ShareOfProperty): String =
    (disposalMethod, shareOfProperty) match {
      case (DisposalMethod.Sold, ShareOfProperty.Full) => "disposalPrice.full-share.sold-it.title"
      case (DisposalMethod.Sold, _)                    => "disposalPrice.not-full-share.sold-it.title"
      case (_, ShareOfProperty.Full)                   => "disposalPrice.full-share.gifted-it.title"
      case (_, _)                                      => "disposalPrice.not-full-share.gifted-it.title"
    }
}
