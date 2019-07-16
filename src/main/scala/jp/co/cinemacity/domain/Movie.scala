package jp.co.cinemacity.domain

import java.time.LocalDateTime

case class Movie(id: MovieId, name: String)

case class Show(id: ShowId, movie: Movie, typ: ShowType, at: LocalDateTime) {
  lazy val time: TimeType = if (at.getHour <= 20) TimeType.Late else TimeType.Daytime
  lazy val date: DateType = ???

  private def priceTable: Set[TicketPricing] = typ match {
    case ShowType.SpecialShow(price) => Set(TicketPricing.always(price))

    case ShowType.GokuBaku =>
      TicketPricing.of(date).map(TicketPricing.alwaysDaytimePriceAt)

    case ShowType.ThreeD =>
      TicketPricing.of(date).map(_.andThen(_.andThen(_ + Price(400))))

    case ShowType.Standard =>
      TicketPricing.of(date)
  }

  def priceOf(ticketType: TicketType): Ticket = Ticket(this, ticketType, priceTable.flatMap(_.apply(time).lift(ticketType)).min)
}

case class Ticket(show:Show, typ: TicketType, price: Price)

sealed trait ShowType

object ShowType {

  case class SpecialShow(price: Price) extends ShowType

  case object GokuBaku extends ShowType

  case object ThreeD extends ShowType

  case object Standard extends ShowType

}


sealed trait DateType

object DateType {

  case class MovieDay(dateType: DateType) extends DateType

  case object Holiday extends DateType

  case object Weekday extends DateType

}

sealed trait TimeType

object TimeType {

  case object Late extends TimeType

  case object Daytime extends TimeType

}

object Price {
  implicit val ordering: Ordering[Price] = Ordering.by(_.value)
}

case class Price(private val value: Int) {
  def +(price: Price) = Price(value + price.value)

  def -(price: Price) = Price(value - price.value)
}

object TicketPricing {
  def always(price: Price): TicketPricing = _ => {case _ => price}

  def alwaysDaytimePriceAt(pricing: TicketPricing): TicketPricing = (_: TimeType) => pricing.apply(TimeType.Daytime)

  def of(dateType: DateType): Set[TicketPricing] = dateType match {
    case DateType.MovieDay(dt) => of(dt) + MoviedayPricing
    case DateType.Weekday => Set(WeekdayPricing)
    case DateType.Holiday => Set(HolidayPricing)
  }
}

object WeekdayPricing extends TicketPricing {
  import TicketType._, TimeType._
  override def apply(time: TimeType): PartialFunction[TicketType, Price] = {
    case ticket =>
      (ticket, time) match {
        case (CinemaCitizen, _) => Price(1000)
        case (CinemaCitizenAged, _) => Price(1000)
        case (Regular, Daytime) => Price(1800)
        case (Regular, Late) => Price(1300)
        case (Senior, _) => Price(1100)
        case (UniversityStudent, Daytime) => Price(1500)
        case (UniversityStudent, Late) => Price(1300)
        case (Student, _) => Price(1000)
        case (Child, _) => Price(1000)
        case (Baby, _) => Price(0)
        case (Challenged, _) => Price(1000)
        case (ChallengedStudent, _) => Price(900)
        case (MiCardHolder, Daytime) => Price(1600)
        case (MiCardHolder, Late) => Price(1300)
        case (ParkingAreaPark80Holder, Daytime) => Price(1400)
        case (ParkingAreaPark80Holder, Late) => Price(1100)
      }
  }
}

object HolidayPricing extends TicketPricing {
  import TicketType._, TimeType._
  override def apply(time: TimeType): PartialFunction[TicketType, Price] = {
    case ticket =>
      (ticket, time) match {
        case (CinemaCitizen, Daytime) => Price(1300)
        case (CinemaCitizen, Late) => Price(1000)
        case (CinemaCitizenAged, _) => Price(1000)
        case (Regular, Daytime) => Price(1800)
        case (Regular, Late) => Price(1300)
        case (Senior, _) => Price(1100)
        case (UniversityStudent, Daytime) => Price(1500)
        case (UniversityStudent, Late) => Price(1300)
        case (Student, _) => Price(1000)
        case (Child, _) => Price(1000)
        case (Baby, _) => Price(0)
        case (Challenged, _) => Price(1000)
        case (ChallengedStudent, _) => Price(900)
        case (MiCardHolder, Daytime) => Price(1600)
        case (MiCardHolder, Late) => Price(1300)
        case (ParkingAreaPark80Holder, Daytime) => Price(1400)
        case (ParkingAreaPark80Holder, Late) => Price(1100)
      }
  }
}

object MoviedayPricing extends TicketPricing {

  import TicketType._

  override def apply(v1: TimeType): PartialFunction[TicketType, Price] = {
    case Regular | CinemaCitizen | Senior | UniversityStudent => Price(1100)
    case CinemaCitizenAged | Student | Child | Challenged => Price(1000)
    case ChallengedStudent => Price(900)
  }
}
