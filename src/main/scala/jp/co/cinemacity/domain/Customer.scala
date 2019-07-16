package jp.co.cinemacity.domain

case class BuyerId(value: Long)

case class Customer(benefits: Set[Benefit]){
  def has(benefit: PartialFunction[Benefit, Boolean]): Boolean = benefits.exists(benefit)
}

sealed trait Benefit

object Benefit {
  case class ID(age: Age) extends Benefit
  case object CinemaCitizenID extends Benefit
  case object MiCard extends Benefit
  case object ParkingAreaPark80 extends Benefit
  case object UniversityStudentID extends Benefit
  case object StudentHandbook extends Benefit
  case object DisabilityHandbook extends Benefit
  case object AccompanyOfDisabled extends Benefit
  case object GlassesFor3D extends Benefit
  case object Child extends Benefit
  case object Baby extends Benefit
}

case class Age(private val value: Int) {
  def > (age: Age): Boolean = value > age.value
  def < (age: Age): Boolean = value < age.value
  def == (age: Age): Boolean = value == age.value
  def >= (age: Age): Boolean = value >= age.value
  def <= (age: Age): Boolean = value <= age.value
}

