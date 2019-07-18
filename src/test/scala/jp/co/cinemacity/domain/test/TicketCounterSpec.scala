package jp.co.cinemacity.domain.test

import java.time.LocalDateTime

import jp.co.cinemacity.domain.Benefit.CinemaCitizenID
import jp.co.cinemacity.domain.TicketType.CinemaCitizen
import jp.co.cinemacity.domain._
import org.scalatest.FlatSpec


class TicketCounterSpec extends FlatSpec {

  lazy val vendor: TicketVendor = CinemaCityTicketVendor
  lazy val concierge: TicketConcierge = CinemaCityTicketConcierge
  lazy val counter = new TicketCounter(vendor, concierge)
  lazy val movie = Movie(MovieId(1L), "スパイダーマン")

  "TicketCounter" should "sell a cheaper ticket" in {

    val customer = Customer(Set(Benefit.StudentHandbook, Benefit.DisabilityHandbook))

    val ticketTypes = concierge.validate(customer)

    assert(ticketTypes.contains(TicketType.Student))

    assert(ticketTypes.contains(TicketType.DisabledStudent))

    val show = Show(ShowId(11L), movie, ShowType.Standard, LocalDateTime.of(2019, 7, 2, 10, 0))

    val tickets = ticketTypes.map(vendor.issue(show, _))

    assert(tickets.exists(_.listPrice == Price(1000)))

    assert(tickets.exists(_.listPrice == Price(900)))

    val price = counter.sell(customer, show)

    assert(price == Price(900))
  }


  it should "sell weekday ticket to CineCitizen what if it's movie day" in {

    val customer = Customer(Set(CinemaCitizenID))

    val ticketTypes = concierge.validate(customer)

    assert(ticketTypes.contains(TicketType.CinemaCitizen))

    val show = Show(ShowId(11L), movie, ShowType.Standard, LocalDateTime.of(2019, 7, 1, 10, 0))

    val tickets = ticketTypes.map(vendor.issue(show, _))

    assert(tickets.exists(_.listPrice == Price(1000)))

    assert(tickets.exists(_.listPrice == Price(1100)))

    val price = counter.sell(customer, show)

    assert(price == Price(1000))
  }
}
