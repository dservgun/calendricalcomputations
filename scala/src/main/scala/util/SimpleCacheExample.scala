package cache;
object SimpleCacheExample {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source
  import cache.SimpleCache.CommandAndControl._
  import cache.SimpleCache.NetworkServer.{Port, PoolSize}
  type CustomType = (String, String)
  def splitter (a : String) : Either[String, (String, CustomType)] = 
    {
      val elems = a.split('|')
      if (elems.size == 2)
        Right(elems(0), (elems(0), elems(1)))
      else
        Left ("Unable to parse line " + a)
    }
   def loadTest(aFile : String, intervalMs : Int) = {
      val cache : TMap[String, CustomType] = TMap()
      SimpleCache.loadPeriodically (splitter) (intervalMs) (aFile) (cache)
   }

   def start(aFile : String, anInterval : IntervalMs,  portNumber : Port, poolSize : PoolSize) = {
      loadTest(aFile, anInterval)
      SimpleCache.NetworkServer.start(portNumber, poolSize)
   }

}