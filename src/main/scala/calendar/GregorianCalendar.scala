package scala.calenndar;

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
	class Date(val m : Month, val d : Day, val y : Year);
	private def divPair (num : Int, den : Int) : (Int, Int) =
		(num / den, num % den) 
	def first  (input : (Int,Int)) : Int = input._1
	def second (input : (Int, Int)) : Int = input._2
	def quotient (num : Int, den : Int) : Int  =	first(divPair(num, den))
	def remainder (num : Int, den : Int) : Int = second(divPair(num, den))
	def extractMonth (aDate : Date) : Month = aDate.m
	def extractDay (aDate : Date) : Month = aDate.d  
	def extractYear (aDate : Date) : Year = aDate.y
}