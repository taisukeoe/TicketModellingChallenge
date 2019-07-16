package jp.co.cinemacity.domain

sealed trait TicketType
sealed trait NonDiscountableTicketType extends TicketType

object TicketType {
  def of(customer: Customer): Set[TicketType] = {
    def check(benefit: Benefit): TicketType = benefit match {
      case Benefit.CinemaCitizenID =>
        if(customer.has{case Benefit.ID(age) => age >= Age(60)})
          CinemaCitizenAged
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
          ChallengedStudent
        else
          Challenged
      case Benefit.UniversityStudentID => UniversityStudent
      case Benefit.StudentHandbook => Student
      case Benefit.Child => Child
      case Benefit.Baby => Baby
      case Benefit.ID(age) if age >= Age(70) => Senior
      case _ => Regular
    }

    customer.benefits.map(check) + Regular
  }

  case object Regular extends TicketType
  case object Senior extends TicketType
  case object CinemaCitizen extends TicketType
  case object CinemaCitizenAged extends TicketType
  case object UniversityStudent extends TicketType
  case object Student extends TicketType
  case object Child extends TicketType
  case object Baby extends TicketType
  case object Challenged extends TicketType
  case object ChallengedStudent extends TicketType
  case object MiCardHolder extends NonDiscountableTicketType
  case object ParkingAreaPark80Holder extends NonDiscountableTicketType
}
