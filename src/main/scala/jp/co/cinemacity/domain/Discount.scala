package jp.co.cinemacity.domain


/*
  割引を提供する何か。
  チケット種や上映回ごとに割り引ける額が決まっている。
 */
trait Discount {
  def pricing: DiscountPricing
}

object Discount {
  case object NotBorrowingGlassesFor3D extends Discount {

    override def pricing: DiscountPricing = {
      case ticket if ticket.show.typ == ShowType.ThreeD && !ticket.typ.isInstanceOf[NonDiscountableTicketType] => Price(100)
    }
  }
}

object DiscountPricing {
  lazy val noDiscount: DiscountPricing = {case _ => Price(0)}
}
