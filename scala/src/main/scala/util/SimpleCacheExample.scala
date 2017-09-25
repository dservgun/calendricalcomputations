package cache;
object SimpleCacheExample {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source
  import cache.SimpleCache.CommandAndControl._
  import cache.SimpleCache.NetworkServer.{Port, PoolSize}
  import java.util.Date
  type CustomType = (String, String)
  def splitter (a : String) : Either[String, (String, CustomType)] = 
    {
      val elems = a.split('|')
      if (elems.size == 2)
        Right(elems(0).trim(), (elems(0).trim(), elems(1).trim()))
      else
        Left ("Unable to parse line " + a)
    }
   def loadTest(aFile : String, intervalMs : Int) = {
      val cache : TMap[String, CustomType] = TMap()
      val keyArray : Ref[List[(String, Date)]] = Ref(List())
      SimpleCache.loadPeriodically (splitter) (intervalMs) (aFile) (cache, keyArray)
   }
   def simpleLoad(aFile : String) = {
      val cache : TMap[String, CustomType] = TMap()
      val keyArray : Ref[List[(String, Date)]] = Ref(List())
      SimpleCache.load (splitter) (aFile) (cache, keyArray)
      //Thread.sleep(1000)
      println("Value got " + SimpleCache.get(cache, keyArray) ("key1"))
      SimpleCache.evictOne(cache, keyArray)
      SimpleCache.size(cache, keyArray)
      Thread.sleep(5500)
      SimpleCache.evictOne(cache, keyArray)
      SimpleCache.evictOne(cache, keyArray)
      SimpleCache.evictOne(cache, keyArray)
      SimpleCache.evictOne(cache, keyArray)
      SimpleCache.evictOne(cache, keyArray)

   }
   def start(aFile : String, anInterval : IntervalMs,  portNumber : Port, poolSize : PoolSize) = {
      loadTest(aFile, anInterval)
      SimpleCache.NetworkServer.start(portNumber, poolSize)
   }

}