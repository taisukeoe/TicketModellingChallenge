package jp.co.cinemacity.domain.test

import java.time.LocalDateTime

import jp.co.cinemacity.domain.Benefit.CinemaCitizenID
import jp.co.cinemacity.domain.Discount.NotBorrowingGlassesFor3D
import jp.co.cinemacity.domain._
import org.scalatest.FlatSpec


class TicketCounterSpec extends FlatSpec {

  lazy val vendor: TicketVendor = CinemaCityTicketVendor
//  lazy val concierge: TicketConcierge = CinemaCityTicketConcierge
  lazy val counter = new TicketCounter(vendor)
  lazy val movie = Movie(MovieId(1L), "スパイダーマン")

  "TicketCounter" should "sell a cheaper ticket" in {

    val customer = Customer(Personality.Student, Set(Benefit.DisabilityHandbook), Set.empty)

    val ticketTypes = customer.availableTicketTypes

    assert(ticketTypes.contains(TicketType.Student))

    assert(ticketTypes.contains(TicketType.DisabledStudent))

    val show = Showing(ShowId(11L), movie, ShowType.Standard, LocalDateTime.of(2019, 7, 2, 10, 0))

    val tickets = ticketTypes.map(vendor.issue(show, _))

    assert(tickets.exists(_.price == Price(1000)))

    assert(tickets.exists(_.price == Price(900)))

    val ticket = counter.sell(customer, show)

    assert(ticket.price == Price(900))
  }


  it should "sell weekday ticket to CineCitizen what if it's movie day" in {

    val customer = Customer(Personality.Adult(Age(20)), Set(CinemaCitizenID), Set(NotBorrowingGlassesFor3D))

    val ticketTypes = customer.availableTicketTypes

    assert(ticketTypes.contains(TicketType.CinemaCitizen))

    val show = Showing(ShowId(11L), movie, ShowType.Standard, LocalDateTime.of(2019, 7, 1, 10, 0))

    val tickets = ticketTypes.map(vendor.issue(show, _))

    assert(tickets.exists(_.price == Price(1000)),tickets)

    assert(tickets.exists(_.price == Price(1100)),tickets)

    val ticket = counter.sell(customer, show)

    assert(ticket.price == Price(1000))
  }

  it should "sell 3D ticket with glasses discount" in {
    val customer = Customer(Personality.Adult(Age(20)), Set(CinemaCitizenID), Set(NotBorrowingGlassesFor3D))

    val ticketTypes = customer.availableTicketTypes

    assert(ticketTypes.contains(TicketType.CinemaCitizen))

    val show = Showing(ShowId(11L), movie, ShowType.ThreeD, LocalDateTime.of(2019, 7, 6, 23, 0))

    val tickets = ticketTypes.map(vendor.issue(show, _))

    assert(tickets.exists(_.price == Price(1400)), tickets)

    val ticket = counter.sell(customer, show)

    assert(ticket.price == Price(1300))
  }
}
