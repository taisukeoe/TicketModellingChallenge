package jp.co.cinemacity.domain

/*
  チケット。
  上映会、チケット種別、そして割引なしの定価。
 */
case class Ticket(show: Showing, typ: TicketType, price: Price)


/*
  チケット種別。
  割引可能なものと、割引不可なものがある。
 */
sealed trait TicketType

sealed trait NonDiscountableTicketType extends TicketType

object TicketType {

  case object Regular extends TicketType

  case object Senior extends TicketType

  case object CinemaCitizen extends TicketType

  case object CinemaCitizenSenior extends TicketType

  case object UniversityStudent extends TicketType

  case object Student extends TicketType

  case object Child extends TicketType

  case object Baby extends TicketType

  case object Disabled extends TicketType

  case object DisabledStudent extends TicketType

  case object MiCardHolder extends NonDiscountableTicketType

  case object ParkingAreaPark80Holder extends NonDiscountableTicketType
}

object Price {
  implicit val ordering: Ordering[Price] = Ordering.by(_.value)
}

case class Price(private val value: Int) {
  def +(price: Price) = Price(value + price.value)

  def -(price: Price) = Price(value - price.value)
}

