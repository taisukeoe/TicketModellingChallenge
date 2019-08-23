package jp.co.cinemacity.domain

import java.time.{DayOfWeek, LocalDateTime}

import jp.t2v.util.locale.Holidays

/*
  映画。
 */
case class Movie(id: MovieId, name: String)

case class MovieId(private val value: Long) extends AnyVal

/*
  上映回。
 */
case class Showing(id: ShowId, movie: Movie, typ: ShowType, at: LocalDateTime) {
  lazy val time: TimeType = if (at.getHour <= 20) TimeType.Daytime else TimeType.Late
  lazy val date: DateType = (at.getDayOfWeek, at) match {
    case (DayOfWeek.SATURDAY | DayOfWeek.SUNDAY, _) => if (at.getDayOfMonth == 1) DateType.MovieDay(DateType.Holiday) else DateType.Holiday
    case (_, Holidays(_)) => if (at.getDayOfMonth == 1) DateType.MovieDay(DateType.Holiday) else DateType.Holiday
    case _ => if (at.getDayOfMonth == 1) DateType.MovieDay(DateType.Weekday) else DateType.Weekday
  }
}

case class ShowId(private val value: Long) extends AnyVal

sealed trait ShowType

object ShowType {

  case class SpecialShow(price: Price) extends ShowType

  case object GokuBaku extends ShowType

  case object ThreeD extends ShowType

  case object Standard extends ShowType

}


sealed trait DateType

object DateType {

  case class MovieDay(dateType: DateType) extends DateType

  case object Holiday extends DateType

  case object Weekday extends DateType

}

sealed trait TimeType

object TimeType {

  case object Late extends TimeType

  case object Daytime extends TimeType

}