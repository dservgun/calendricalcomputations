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

  
  def xdayOnOrBefore(d : Int , x : Int) : Int = (d - ((d - x) % 7))
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

/**
  GregorianDate(int d) { // Computes the Gregorian date from the absolute date.
    
    // Search forward year by year from approximate year
    year = d/366;
    while (d >= GregorianDate(1,1,year+1))
      year++;
    // Search forward month by month from January
    month = 1;
    while (d > GregorianDate(month, LastDayOfGregorianMonth(month,year), year))
      month++;
    day = d - GregorianDate(month,1,year) + 1;
  }

*/
  def toGregorianDate(d : Int) : GregorianDate = {
    val year : Int = d / 366 
    def getYear (aYear : Int) : Int = {
        val date = gDateToDay(new GregorianDate(1, 1, aYear + 1))
        if (d >= date)
          getYear(aYear + 1) 
        else
          aYear
        }
    //search forward for the month.
    def getMonth(month : Int, year : Int) : Int = {
        val lday = lastDayOfGregorianMonth(month, year)
        val date = gDateToDay(new GregorianDate(month, lday, year))
        if (d >= date)
          getMonth(month + 1, year)
        else
          month    
    }
    val approxDate = {
      val aY = new GregorianDate(1, 1, getYear(year))
      new GregorianDate(getMonth(1, aY.y), 1, aY.y)
    }
    val day = d - (gDateToDay(new GregorianDate(approxDate.m, 1, approxDate.y))) + 1;
    new GregorianDate(approxDate.m, day, approxDate.y)
    
  }


  /*
  operator int() { // Computes the absolute date from the Gregorian date.
    int N = day;           // days this month
    for (int m = month - 1;  m > 0; m--) // days in prior months this year
      N = N + LastDayOfGregorianMonth(m, year);
    return
      (N                    // days this year
       + 365 * (year - 1)   // days in previous years ignoring leap days
       + (year - 1)/4       // Julian leap days before this year...
       - (year - 1)/100     // ...minus prior century years...
       + (year - 1)/400);   // ...plus prior years divisible by 400
  }  
  */

  def gDateToDay(d : GregorianDate) : Int = {
    def accum(s : Int, m : Int, result : Int) : Int = {
      if (m > 0) {
        accum(s, m - 1, result + lastDayOfGregorianMonth(m, d.y))
      }else
        result;
    }
    val n = accum(d.d, d.m - 1, d.d); 
    (n 
      + (365 * (d.y - 1))
      + (d.y - 1) / 4 
      - (d.y - 1) / 100
      + (d.y - 1) / 400
      )
  }

/*
GregorianDate NthXday(int n, int x, int month, int year, int day = 0)
// The Gregorian date of nth x-day in month, year before/after optional day.
// x = 0 means Sunday, x = 1 means Monday, and so on.  If n<0, return the nth
// x-day before month day, year (inclusive).  If n>0, return the nth x-day
// after month day, year (inclusive).  If day is omitted or 0, it defaults
// to 1 if n>0, and month's last day otherwise.
{
  if (n > 0) {
    if (day == 0)
      day = 1;  // default for positive n
    return GregorianDate
      ((7 * (n - 1)) + XdayOnOrBefore(6 + GregorianDate(month, day, year), x));
  }
  else {
    if (day == 0)
      day = LastDayOfGregorianMonth(month, year);;  // default for negative n
    return GregorianDate
      ((7 * (n + 1)) + XdayOnOrBefore(GregorianDate(month, day, year), x));
  }
}
*/
  /**
  * The gregorian date of nth x-day in month, year, before/after optional day
  */
  def NthXDay(n : Int, x : Int , month : Int, year : Int, day : Option[Int]) : GregorianDate = 
    if (n > 0) {
        val referenceDate : Int = 
          day match {
            case None =>  gDateToDay(new GregorianDate(month, 1, year))
            case Some(d) => gDateToDay(new GregorianDate(month, d, year))
        }
        toGregorianDate((7 * (n - 1)) + xdayOnOrBefore(6 + referenceDate, x))
      } else {          
          val referenceDate = 
            day match {
              case None => 
                gDateToDay(new GregorianDate(month, lastDayOfGregorianMonth(month, year), year))
              case Some(d) => 
                gDateToDay(new GregorianDate(month, d, year))
            } 
        toGregorianDate((7 * (n + 1)) + xdayOnOrBefore(referenceDate, x))
      } 

  // First monday of september
  def laborDay(year : Int) = NthXDay(1, 1, 9, year, None)
  //  (Nth-Kday -1 1 5 year));; Last Monday in May.
  def memorialDay(year : Int) = NthXDay(-1, 1, 5, year, None)
/*(defun daylight-savings-start (year)
;; Absolute date of the start of American daylight savings time
;; in Gregorian $year$.
  (Nth-Kday 1 0 4 year));; First Sunday in April.
*/
  def daylightSavingsStart(year : Int) = NthXDay(2, 0 , 3, year, None)
/*
(defun daylight-savings-end (year)
;; Absolute date of the end of American daylight savings time
;; in Gregorian $year$.
  (Nth-Kday -1 0 10 year));; Last Sunday in October.
*/
  def daylightSavingsEnd(year : Int) = NthXDay(1, 0, 11, year, None);
  /*
  (defun christmas (year)
  ;; Absolute date of Christmas in Gregorian $year$.
    (absolute-from-gregorian (list 12 25 year)))
  */
  def christmas(year : Int) = gDateToDay(new GregorianDate(12, 25, year))
  def christmasDay(year: Int) = toGregorianDate(christmas(year))
  def printDate(aGregorianDate : GregorianDate) = 
    aGregorianDate.m + "/" + aGregorianDate.d + "/" + aGregorianDate.y
}