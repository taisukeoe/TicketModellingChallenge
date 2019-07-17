package jp.co.cinemacity.domain

import java.time.LocalDateTime

/*
  映画。
 */
case class Movie(id: MovieId, name: String)

case class MovieId(private val value: Long) extends AnyVal

/*
  上映回。
 */
case class Show(id: ShowId, movie: Movie, typ: ShowType, at: LocalDateTime) {
  lazy val time: TimeType = if (at.getHour <= 20) TimeType.Late else TimeType.Daytime
  lazy val date: DateType = ??? //TODO: 土日祝日と日付の判定
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