package util

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.concurrent.stm._
import scala.io.Source


object SimpleCacheTest {
  type CustomType = (String, String)
  def splitter (a : String) : (String, CustomType) = 
    {
      val elems = a.split('|')
      return (elems(0), (elems(0), elems(1)))
    }
   def loadTest(aFile : String) = {
   		cache.SimpleCache.loadPeriodically (TMap[String, CustomType]TMap()) (splitter) (1000)

   }

}