package cache;
object SimpleCache {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source
  def log(msg: String) {
    println(s"${Thread.currentThread.getName}: $msg")
  }

  def add[K, V](cacheMap : TMap[K, V]) (key : K, value : V) : Unit = atomic {
    implicit txn => 
      cacheMap.put(key, value)
      Txn.afterCommit {
        x => log("After adding ( "  + key + " , (" + value + ")" + " " + x)
      }
      return ()

  }
  def remove[K, V] (cacheMap : TMap [K, V]) (key : K) : Unit = atomic {
    implicit txn =>
      cacheMap.remove(key)
      Txn.afterCommit {
        x => log("After removing " + key + " " + x)
      }
  }
  def size[K, V] (cacheMap : TMap[K, V]) : Int = atomic {
      implicit txn => 
        Txn.afterCommit {
          x => log("After returning size " + x)
        }
        cacheMap.size

  }

  type FileName = String

  def load[K, V] (splitter : String => Either[String, (K, V)]) (aFileName : FileName) (cacheMap : TMap[K, V]) : List[Either[String, (K,V)]] = {
      val source = Source.fromFile(aFileName);
      try { 
        val lines = source.getLines.toList
        val _ = 
          lines.map(x => {
              val splitValues : Either[String, (K, V)] = splitter(x)
              splitValues match {
                case Right((k, v)) => add (cacheMap) (k, v)
                case _                  => log("Error adding line " + x)
              }     
              })
        List.empty
      } catch {
        case e: Exception => 
          log("Error " + e)
          List.empty
      } finally {
        source.close
      }
    }

  //TODO: Change this to Either.
  def refresh[K, V] (interval : Int)    
    (splitter : (String => Either[String, (K, V)]))
    (aFileName : FileName) 
    (cacheMap : TMap[K,V]) : Unit = 
  {
    log("Loading " + aFileName)
    load (splitter) (aFileName) (cacheMap)
    Thread.sleep(interval)
    refresh (interval)(splitter)(aFileName) (cacheMap)
  }


  def loadPeriodically[K, V] (spl : String => Either[String, (K, V)]) (interval : Int) 
      (aFileName : FileName) (cacheMap : TMap[K,V]): Unit = 
  {
    refresh(interval)(spl)(aFileName)(cacheMap)
  }
}

object SimpleCacheTest {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source

  type CustomType = (String, String)
  def splitter (a : String) : Either[String, (String, CustomType)] = 
    {
      val elems = a.split('|')
      if (elems.size == 2)
        Right(elems(0), (elems(0), elems(1)))
      else
        Left ("Unable to parse line " + a)
    }
   def loadTest(aFile : String) = {
      val cache : TMap[String, CustomType] = TMap()
      SimpleCache.loadPeriodically (splitter) (1000) (aFile) (cache)

   }

}
