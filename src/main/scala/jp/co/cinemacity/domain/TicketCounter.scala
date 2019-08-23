package jp.co.cinemacity.domain

/*
 チケットカウンター。
 顧客が求めるチケットを、最も安い価格で案内し、販売する。
 */
class TicketCounter(vendor: TicketVendor) {
  def sell(customer: Customer, show: Showing): Ticket = {
    val ticketTypes = customer.availableTicketTypes
    val discounts = customer.discounts

    val tickets = ticketTypes.map(vendor.issue(show, _))

    tickets.map(t => t.copy( price = discounts.foldLeft(t.price)((price, disc) => price - disc.pricing.applyOrElse(t, DiscountPricing.noDiscount)))).minBy(_.price)
  }
}

