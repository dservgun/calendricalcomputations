package calendar

import org.scalatest._
import scala.calendar.GregorianCalendar._;
import scala.calendar.GregorianCalendar.GregorianDate; 


class GregorianCalendarSpec extends FlatSpec with Matchers {
  "The gregorian date " should "work correctly" in {
  		val aDate = new GregorianDate(1,1,2017)
    	toGregorianDate(gDateToDay(aDate)) == (aDate)
  }

  "Labor day this year " should "work correctly " in {
  		laborDay(2017) shouldEqual (new GregorianDate(9, 4, 2017))
  }
  "Months " should "work" in {
  	val aDate = toGregorianDateF(32);
  	val dString = printDate(aDate) 
  	val cmpString = printDate(new GregorianDate(2, 1, 1))
  	dString shouldEqual cmpString
  }

}
