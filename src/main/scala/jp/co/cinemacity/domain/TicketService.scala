package jp.co.cinemacity.domain

trait TicketCounter {
  def sell(customer: Customer, show: Show): Price = {
    val ticketTypes = TicketType.of(customer)
    val discounts = Discount.of(customer)

    val tickets = ticketTypes.map(show.priceOf)

    tickets.map(discounts.foldLeft(_)((t, d) => t.copy(price  = t.price + d.applyOrElse(t, DiscountPricing.nodiscount)))).minBy(_.price).price
  }
}

