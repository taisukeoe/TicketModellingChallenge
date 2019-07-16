package jp.co.cinemacity

package object domain {
  type TicketPricing = TimeType => PartialFunction[TicketType, Price]

  type DiscountPricing = PartialFunction[Ticket, Price]
}
