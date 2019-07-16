package jp.co.cinemacity.domain


/*
  割引を提供する何か。
  チケット種や上映回ごとに割り引ける額が決まっている。
 */
object Discount {
  case object NotBorrowingGlassesFor3D extends DiscountPricing {

    override def isDefinedAt(ticket: Ticket): Boolean = ticket.show.typ == ShowType.ThreeD && !ticket.typ.isInstanceOf[NonDiscountableTicketType]

    override def apply(ticket: Ticket): Price =  if(isDefinedAt(ticket)) Price(100) else Price(0)
  }
}

object DiscountPricing {
  lazy val noDiscount: DiscountPricing = {case _ => Price(0)}
}
