package jp.co.cinemacity

package object domain {
  case class MovieId(private val value: Long) extends AnyVal
  case class ShowId(private val value: Long) extends AnyVal

  type TicketPricing = TimeType => PartialFunction[TicketType, Price]

  type DiscountPricing = PartialFunction[Ticket, Price]

  implicit class Chainable[A,B,C](original: Function2[A,B,C]) {
    def andThen[T](f: Function1[C, T]): Function2[A,B,T] = (a:A, b:B) => f(original(a, b))
  }
}
