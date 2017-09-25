package cache;
import java.util.Date;
object SimpleCache {
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source

  type LRUCache[K, V] = (TMap[K, V], Ref[List[(K, Date)]])

  object CacheRefreshParameters {
    val defaultExpirationInMillis = 5000;
    val defaultCacheSize = 10;
  }


  def evictKey [K, V] (key : K) (cacheMap : LRUCache[K, V]) : Unit = atomic {
    implicit txn => 
      val list = cacheMap._2() 
      val saveMe = list.filter(_._1 != key)
      cacheMap._1.remove(key)
      cacheMap._2() = saveMe
      Txn.afterCommit {
        x => log("After evicting key " + key)
      }

  }
  // Evict the first key greater than the expiration time.
  def evictOne[K, V] (cacheMap : LRUCache[K, V]) : Unit = atomic {
    implicit txn => 
      val list = cacheMap._2() 
      val currentTime = (new Date()).getTime
      val removeMe = list.find(currentTime - _._2.getTime > CacheRefreshParameters.defaultExpirationInMillis)
      removeMe match {
        case Some(l) => 
            val newList = list.filter(_._1 != l._1)
            cacheMap._2 () = newList
            removeMe.map(x => cacheMap._1.remove(x._1))      
        case None => ()
      }
      Txn.afterCommit {
        x => log("Eviction complete. " + removeMe)
      }
  }


  def log(msg: String) {
    val dateTime = new java.util.Date()
    println(s"${Thread.currentThread.getName}: $dateTime : $msg")
  }

  // Cache operations.
  def clear[K, V] (cacheMap : LRUCache[K, V]) : Unit = atomic {
    implicit txn => 
      cacheMap._1.empty
      cacheMap._2() = List()
      Txn.afterCommit {
        x => log("Clearing contents for map " + x)
      } 
  }
  def removeFromKeys[K, V] (key : K) (keyArray : Ref[List[(K, Date)]]) : K = atomic {
    // need to remove from the key.
    implicit txn => 
      val list = keyArray() 
      val newList = list.filter(_._1 != key)
      keyArray() = newList
      Txn.afterCommit {
        x => log("Removing the key from the list")
      }
      key
  }
  def addToKeys[K, V] (key : K) (keyArray : Ref[List[(K, Date)]]): Unit = atomic {
    implicit txn => 
      val n : (K, Date) = (key, new Date())
      keyArray () = n :: keyArray ()
      Txn.afterCommit {
        x => log("Adding key to the key array " + key)
    }
  }
  
  def get[K,V] (cacheMap : LRUCache[K, V]) (key : K) : Option[V] = atomic {
    implicit txn => 
      addToKeys(key)(cacheMap._2)
      cacheMap._1.get(key)
  }
  def add[K, V](cacheMap : LRUCache[K, V]) (key : K, value : V) : Unit = atomic {
    implicit txn => 
      addToKeys(key)(cacheMap._2)
      cacheMap._1.put(key, value)
      Txn.afterCommit {
        x => log("After adding ( "  + key + "->(" + value + ")" + " " + x + " ")
      }
      return ()
  }
  def remove[K, V] (cacheMap : LRUCache [K, V]) (key : K) : Unit = atomic {
    implicit txn =>
      removeFromKeys(key)(cacheMap._2)
      cacheMap._1.remove(key)
      Txn.afterCommit {
        x => log("After removing " + key + " " + x)
      }
  }

  def size[K, V] (cacheMap : LRUCache[K, V]) : Int = atomic {
      implicit txn => 
        Txn.afterCommit {
          x => log("After returning size " + x)
        }
        //assert that the key size 
        //the list size are equal.
        cacheMap._1.size

  }

  type FileName = String

  def load[K, V] (splitter : String => Either[String, (K, V)]) (aFileName : FileName) (cacheMap : LRUCache[K, V]) : List[Either[String, (K,V)]] = {
      val source = Source.fromFile(aFileName);
      try { 
        val lines = source.getLines.toList
//        val clMap = clear(cacheMap)
        val _ =         
          lines.map(x => {
              val splitValues : Either[String, (K, V)] = splitter(x)
              splitValues match {
                case Right((k, v)) => 
                  add (cacheMap) (k, v)
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


  def evictor[K, V] (interval : Int) (cacheMap : LRUCache[K,V]) : Unit = {
    log("Evicting...") 
    evictOne(cacheMap)
    Thread.sleep(interval) 
    evictor (interval)(cacheMap)
  }
  def refresh[K, V] (interval : Int)    
    (splitter : (String => Either[String, (K, V)]))
    (aFileName : FileName) 
    (cacheMap : LRUCache[K,V]) : Unit = 
  {
    log("Loading " + aFileName)
    load (splitter) (aFileName) (cacheMap)
    Thread.sleep(interval)
    refresh (interval)(splitter)(aFileName) (cacheMap)
  }

  object NetworkServer {
    import java.net.{Socket, ServerSocket}
    import java.util.concurrent.{Executors, ExecutorService}
    import java.util.Date
    import java.io._
    type Port = Int 
    type PoolSize = Int
    class NetworkService(port: Int, poolSize: Int) extends Runnable {
      val serverSocket = new ServerSocket(port)
      val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)

      def run() {
        try {
          while (true) {
            // This will block until a connection comes in.
            log("Waiting for connections...")
            val socket = serverSocket.accept()
            pool.execute(new Handler(socket))
          }
        } finally {
          pool.shutdown()
        }
      }
    }

    class Handler(socket: Socket) extends Runnable {
      def message = (Thread.currentThread.getName() + "\n").getBytes

      def run() {
          val buf = new BufferedReader(new InputStreamReader(socket.getInputStream))
          try{ 
            val currentMessage = buf.readLine + "\n"
            socket.getOutputStream.write(currentMessage.getBytes)            
          }
          finally            
            socket.getOutputStream.close()
      }
    }
    def start(port : Port, poolSize : PoolSize) = (new NetworkService(port, poolSize)).run
  }
  object CommandAndControl {
    type IntervalMs = Int 

    sealed trait Command
    case object Start extends Command 
    case object Stop extends Command

    val command : Ref[Command] = Ref(Stop)
    def start () : Unit = atomic {
        implicit txn => 
          command () = Start
          Txn.afterCommit {
            x => log("Starting the server");
          }
    }
    def stop () : Unit = atomic {
      implicit txn => 
        command () = Stop
        Txn.afterCommit {
          x => log("Stopping the server")
        }
    }
    def currentState () : Command = atomic {
      implicit txn => 
        val readVal = command () 
        Txn.afterCommit {
          x => log("Reading current state")
        }
        readVal
    }
  }


  import CommandAndControl._
  def loadPeriodically[K, V] (spl : String => Either[String, (K, V)]) (interval : Int) 
      (aFileName : FileName) (cacheMap : LRUCache[K, V]): Unit =  {
        val f = Future { refresh(interval)(spl)(aFileName)(cacheMap)}
        val e = Future { evictor(100)(cacheMap) }
        Await.result(f, Duration.Inf)
        Await.result(e, Duration.Inf)
      }
}


