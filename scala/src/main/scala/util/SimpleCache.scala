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
  }
  def remove[K, V] (cacheMap : TMap [K, V]) (key : K) : Unit = atomic {
    implicit txn =>
      cacheMap.remove(key)
  }
  def size[K, V] (cacheMap : TMap[K, V]) : Int = atomic {
      implicit txn => cacheMap.size
  }

  type FileName = String

  def load[K, V] (splitter : String => (K, V)) (aFileName : FileName) (cacheMap : TMap[K, V]) : Unit = {
      val source = Source.fromFile(aFileName);
      try { 
        val lines = source.getLines.toList
        lines.map(x => {
              val (k, v) = splitter(x)
              add(cacheMap)(k,v)
              })
      } catch {
        case e: Exception => 
          log("Error " + e)
      } finally {
        source.close
      }
    }

  //TODO: Change this to Either.
  def refresh[K, V] (interval : Int)
    (splitter : (String => (K, V)))
    (aFileName : FileName) 
    (cacheMap : TMap[K,V]) : Unit = 
  {
    load (splitter) (aFileName) (cacheMap)
    Thread.sleep(interval)
    refresh (interval)(splitter)(aFileName) (cacheMap)
  }

  def loadPeriodically[K, V] (cacheMap : TMap[K,V]) (spl : String => (K, V)) (interval : Int) (aFileName : FileName) : Unit = 
  {
    refresh(interval)(spl)(aFileName)(cacheMap)
  }
}
