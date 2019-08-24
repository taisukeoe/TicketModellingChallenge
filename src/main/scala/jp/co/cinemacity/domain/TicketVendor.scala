package jp.co.cinemacity.domain

 /*
  チケット発券機。
  上映回を選んでチケット種別を選ぶと、チケット価格が決まる。
 */
trait TicketVendor {
  def issue(show: Showing, ticketType: TicketType): Ticket
}

object CinemaCityTicketVendor extends TicketVendor {
  def issue(show: Showing, ticketType: TicketType): Ticket =
    Ticket(show, ticketType, ticketPricingOf(show).flatMap(_.apply(show.time).lift(ticketType)).min)

  private def ticketPricingAt(dateType: DateType): Set[TicketPricing] = dateType match {
    case DateType.MovieDay(DateType.Weekday) => Set(WeekdayPricing, MoviedayPricing)
    case DateType.MovieDay(DateType.Holiday) => Set(HolidayPricing, MoviedayPricing)
    case DateType.Weekday => Set(WeekdayPricing)
    case DateType.Holiday => Set(HolidayPricing)
  }

  /*
   特別上映が固定額ではなく、特別なチケット価格表だったとしたら `ticketPricingOf` メソッドを拡張する。
  */
  private def ticketPricingOf(show: Showing): Set[TicketPricing] = {
    def always(price: Price): TicketPricing = _ => {
      case _ => price
    }

    def alwaysDaytimePriceAt(pricing: TicketPricing): TicketPricing = (_: TimeType) => pricing.apply(TimeType.Daytime)

    show.typ match {
      case ShowType.SpecialShow(price) => Set(always(price))

      case ShowType.GokuBaku =>
        ticketPricingAt(show.date).map(alwaysDaytimePriceAt)

      case ShowType.ThreeD =>
        ticketPricingAt(show.date).map(_.andThen(_.andThen(_ + Price(400))))

      case ShowType.Standard =>
        ticketPricingAt(show.date)
    }
  }
}

object WeekdayPricing extends TicketPricing {

  import TicketType._, TimeType._

  override def apply(time: TimeType): PartialFunction[TicketType, Price] = {
    case ticket =>
      (ticket, time) match {
        case (CinemaCitizen, _) => Price(1000)
        case (CinemaCitizenSenior, _) => Price(1000)
        case (Regular, Daytime) => Price(1800)
        case (Regular, Late) => Price(1300)
        case (Senior, _) => Price(1100)
        case (UniversityStudent, Daytime) => Price(1500)
        case (UniversityStudent, Late) => Price(1300)
        case (Student, _) => Price(1000)
        case (Child, _) => Price(1000)
        case (Baby, _) => Price(0)
        case (Disabled, _) => Price(1000)
        case (DisabledStudent, _) => Price(900)
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
        case (CinemaCitizenSenior, _) => Price(1000)
        case (Regular, Daytime) => Price(1800)
        case (Regular, Late) => Price(1300)
        case (Senior, _) => Price(1100)
        case (UniversityStudent, Daytime) => Price(1500)
        case (UniversityStudent, Late) => Price(1300)
        case (Student, _) => Price(1000)
        case (Child, _) => Price(1000)
        case (Baby, _) => Price(0)
        case (Disabled, _) => Price(1000)
        case (DisabledStudent, _) => Price(900)
        case (MiCardHolder, Daytime) => Price(1600)
        case (MiCardHolder, Late) => Price(1300)
        case (ParkingAreaPark80Holder, Daytime) => Price(1400)
        case (ParkingAreaPark80Holder, Late) => Price(1100)
      }
  }
}

object MoviedayPricing extends TicketPricing {

  import TicketType._

  override def apply(time: TimeType): PartialFunction[TicketType, Price] = {
    case Regular | CinemaCitizen | Senior | UniversityStudent => Price(1100)
    case CinemaCitizenSenior | Student | Child | Disabled => Price(1000)
    case DisabledStudent => Price(900)
  }
}