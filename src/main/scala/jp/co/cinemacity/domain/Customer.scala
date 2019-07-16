package jp.co.cinemacity.domain


/*
  チケット購入者。
  映画館に足を運んで購入する人を想定しているため、IDはない。
 */
case class Customer(benefits: Set[Benefit]) {
  def has(benefit: PartialFunction[Benefit, Boolean]): Boolean = benefits.exists(benefit)
}

/*
  チケット購入者が提示する特典。
  能動的に提示するもの（IDなど）の他、自然と見て判断されるもの（幼児か否か）や、自己申告特典（障がい者の同伴者）も含まれる。
 */
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
  def >(age: Age): Boolean = value > age.value

  def <(age: Age): Boolean = value < age.value

  def ==(age: Age): Boolean = value == age.value

  def >=(age: Age): Boolean = value >= age.value

  def <=(age: Age): Boolean = value <= age.value
}

