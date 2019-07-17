package jp.co.cinemacity.domain

import jp.co.cinemacity.domain.Discount.NotBorrowingGlassesFor3D
import jp.co.cinemacity.domain.TicketType._

/*
 チケット販売担当者。
 顧客が提示したものをもとに、販売可能なチケット種別や、適用可能な割引を判定する。

 TODO: チケット種別や割引の、人数制約バリデーションの実装
 */
trait TicketConcierge {
  def validate(customer: Customer): Set[TicketType]

  def proposeDiscountsTo(customer: Customer): Seq[DiscountPricing]
}

object CinemaCityTicketConcierge extends TicketConcierge {
  def validate(customer: Customer): Set[TicketType] = {
    def check(benefit: Benefit): TicketType = benefit match {
      case Benefit.CinemaCitizenID =>
        if(customer.has{case Benefit.ID(age) => age >= Age(60)})
          CinemaCitizenSenior
        else
          CinemaCitizen
      case Benefit.MiCard => MiCardHolder
      case Benefit.ParkingAreaPark80 => ParkingAreaPark80Holder
      case Benefit.DisabilityHandbook | Benefit.AccompanyOfDisabled =>
        if(customer.has{
          case Benefit.Child => true
          case Benefit.StudentHandbook => true
          case _ => false
        })
          DisabledStudent
        else
          Disabled
      case Benefit.UniversityStudentID => UniversityStudent
      case Benefit.StudentHandbook => Student
      case Benefit.Child => Child
      case Benefit.Baby => Baby
      case Benefit.ID(age) if age >= Age(70) => Senior
      case _ => Regular
    }

    customer.benefits.map(check) + Regular
  }

  def proposeDiscountsTo(customer: Customer): Seq[DiscountPricing] = {
    def check: PartialFunction[Benefit, DiscountPricing] = {
      case Benefit.GlassesFor3D =>
        NotBorrowingGlassesFor3D
    }

    customer.benefits.toSeq.flatMap(check.lift)
  }
}

