package jp.co.cinemacity.domain

trait Discount

object Discount {
  def of(customer: Customer): Seq[DiscountPricing] = {
    def check: PartialFunction[Benefit, DiscountPricing] = {
      case Benefit.GlassesFor3D =>
        NotBorrowingGlassesFor3D
    }

    customer.benefits.toSeq.flatMap(check.lift)
  }

  case object NotBorrowingGlassesFor3D extends DiscountPricing {

    override def isDefinedAt(ticket: Ticket): Boolean = ticket.show.typ == ShowType.ThreeD && !ticket.typ.isInstanceOf[NonDiscountableTicketType]

    override def apply(ticket: Ticket): Price =  if(isDefinedAt(ticket)) Price(100) else Price(0)
  }
}

object DiscountPricing {
  lazy val nodiscount: DiscountPricing = {case _ => Price(0)}
}
