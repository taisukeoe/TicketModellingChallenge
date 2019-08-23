package jp.co.cinemacity.domain


/*
  チケット購入者。
  映画館に足を運んで購入する人を想定しているため、IDはない。
 */
case class Customer(private val personality: Personality, private val benefits: Set[Benefit], discounts: Set[Discount] ) {
  def availableTicketTypes: Set[TicketType] = benefits.map(_.ticketTypeOf(personality)) + personality.ticketType
}

/*
  チケット購入者が提示する特典。
  能動的に提示するもの（IDなど）の他、自然と見て判断されるもの（幼児か否か）や、自己申告特典（障がい者の同伴者）も含まれる。
 */
sealed trait Benefit {
  def ticketTypeOf(personality: Personality): TicketType
}

object Benefit {

  import Personality._
  import TicketType._

  case object CinemaCitizenID extends Benefit {
    override def ticketTypeOf(personality: Personality): TicketType = personality match {
      case Adult(age) if age >= Age(60) =>
        CinemaCitizenSenior
      case _ =>
        CinemaCitizen
    }
  }

  case object MiCard extends Benefit {
    override def ticketTypeOf(personality: Personality): TicketType = MiCardHolder
  }

  case object ParkingAreaPark80 extends Benefit {
    override def ticketTypeOf(personality: Personality): TicketType = ParkingAreaPark80Holder
  }

  case object DisabilityHandbook extends Benefit {
    override def ticketTypeOf(personality: Personality): TicketType = personality match {
      case Personality.Child | Personality.Student => DisabledStudent
      case _ => Disabled
    }
  }

  case object AccompanyOfDisabled extends Benefit {
    override def ticketTypeOf(personality: Personality): TicketType = DisabilityHandbook.ticketTypeOf(personality)
  }

}

sealed trait Personality {
  def ticketType: TicketType
}

object Personality {

  case class Adult(age: Age) extends Personality {
    override def ticketType: TicketType = if(age >= Age(70)) TicketType.Senior else TicketType.Regular
  }

  case object Child extends Personality {
    def ticketType: TicketType = TicketType.Child
  }

  case object Baby extends Personality {
    def ticketType: TicketType = TicketType.Baby
  }

  case object Student extends Personality {
    override def ticketType: TicketType = TicketType.Student
  }

  case object UniversityStudent extends Personality {
    override def ticketType: TicketType = TicketType.UniversityStudent
  }

}

case class Age(private val value: Int) {
  def >(age: Age): Boolean = value > age.value

  def <(age: Age): Boolean = value < age.value

  def ==(age: Age): Boolean = value == age.value

  def >=(age: Age): Boolean = value >= age.value

  def <=(age: Age): Boolean = value <= age.value
}

