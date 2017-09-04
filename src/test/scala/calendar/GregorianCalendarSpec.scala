package calendar

import org.scalatest._
import scala.calendar.GregorianCalendar._;
import scala.calendar.GregorianCalendar.GregorianDate; 

class GregorianCalendarSpec extends FlatSpec with Matchers {
  "The gregorian date " should "work correctly" in {
  		val aDate = new GregorianDate(1,1,1)
    	(gDateToDay(aDate)) == 1
  }
}
