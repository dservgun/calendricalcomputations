package scala.calendar;

/**
;; The following Lisp code is from ``Calendrical
;; Calculations'' by Nachum Dershowitz and Edward
;; M. Reingold, Software---Practice & Experience, vol. 20,
;; no. 9 (September, 1990), pp. 899--928 and from
;; ``Calendrical Calculations, II: Three Historical
;; Calendars'' by Edward M.  Reingold, Nachum Dershowitz,
;; and Stewart M. Clamen, Software---Practice & Experience,
;; vol. 23, no. 4 (April, 1993), pp. 383--404.

;; This code is in the public domain, but any use of it
;; should publically acknowledge its source.

*/



object GregorianCalendar {
  type Month = Int 
  type Day = Int 
  type Year = Int
  val days =Map(
      (0 -> "Sunday"), (1 ->  "Monday") ,
      (2 -> "Tuesday") , (3 -> "Wednesday"),
      (4 -> "Thursday") , (5 -> "Friday") ,
      (6 -> "Saturday")     
      )
  class Date(val m : Month, val d : Day, val y : Year);
  class GregorianDate(val m : Month, val d : Day, val y : Year);

  private def divPair (num : Int, den : Int) : (Int, Int) =
    (num / den, num % den) 
  def first  (input : (Int,Int)) : Int = input._1
  def second (input : (Int, Int)) : Int = input._2
  def quotient (num : Int, den : Int) : Int  =  first(divPair(num, den))
  def remainder (num : Int, den : Int) : Int = second(divPair(num, den))
  def extractMonth (aDate : Date) : Month = aDate.m
  def extractDay (aDate : Date) : Month = aDate.d  
  def extractYear (aDate : Date) : Year = aDate.y
  def xdayOnOrBefore(d : Int , x : Int) : Int = (d - ((d - x) %7))
  //TODO: Can be written better, though this seems to do the job.
  def lastDayOfGregorianMonth(month : Int, year : Int) : Int = 
    month match {
      case 2 => 
        if (((year % 4 == 0) && ((year % 100) != 0 ))
          || ((year % 400) == 0))
          29
        else
          28
      case 4 => 30
      case 6 => 30 
      case 9 => 30
      case 11 => 30 
      case _ => 31
    }

  def toGregorianDate(d : Int) : GregorianDate =  new GregorianDate(1,1,1)


  def gDateToDay(d : GregorianDate) : Int = {
    val (months : Seq[Int]) = for(i <- d.m - 1 to 1) yield i
    val mDays = 
      months.foldLeft(d.d) {
          (z, f) => 
            z + (lastDayOfGregorianMonth(f, d.y))
          }
    (mDays 
    + (365 * (d.y - 1))
    + (d.y -1) / 4 
    - (d.y -1) / 100
    + (d.y - 1) /400)
  }
}