package jp.co.cinemacity.domain

/*
 チケットカウンター。
 顧客が求めるチケットを、最も安い価格で案内し、販売する。
 */
class TicketCounter(vendor: TicketVendor, concierge: TicketConcierge) {
  def sell(customer: Customer, show: Show): Price = {
    val ticketTypes = concierge.validate(customer)
    val discounts = concierge.proposeDiscountsTo(customer)

    val tickets = ticketTypes.map(vendor.issue(show, _))

    tickets.map(t => discounts.foldLeft(t.listPrice)((price, disc) => price - disc.applyOrElse(t, DiscountPricing.noDiscount))).min
  }
}

