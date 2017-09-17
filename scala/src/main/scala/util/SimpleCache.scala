package cache;
object SimpleCache {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source
  def log(msg: String) {
    println(s"${Thread.currentThread.getName}: $msg")
  }

  val myVal : TMap[String, String] = TMap()
  def add(key : String, value : String) : Unit = atomic {
    implicit txn => 
      myVal.put(key, value)
  }
  def remove(key : String) : Unit = atomic {
    implicit txn =>
      myVal.remove(key)
  }
  def size() : Int = atomic {
      implicit txn => myVal.size
  }

  type FileName = String
  def load(aFileName : FileName) = {
      val source = Source.fromFile(aFileName);
      try { 
        val lines = source.getLines.toList
        lines.map(x => {
              val elems = x.split('|')
              log("Adding elems " + elems(0) + " -> " + elems(1))
              add(elems(0), elems(1))
              })
      } catch {
        case e: Exception => 
          log("Error " + e)
      } finally {
        source.close
      }
      Future {
        log("Removing elems " + "key1")
        remove("key1")
      }
      Thread.sleep(4000)
    }

  def refresh (interval : Int)(f : FileName => Unit) (aFileName : FileName)  : Unit = 
  {
    f (aFileName)
    Thread.sleep(interval)
    refresh (interval)(f)(aFileName)
  }
  def loadPeriodically(interval : Int) (aFileName : FileName) : Unit = 
  {
    refresh(interval) (load) (aFileName)
  }
}
