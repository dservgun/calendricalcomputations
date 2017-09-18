package cache;
object SimpleCache {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.io.Source

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

  def log(msg: String) {
    val dateTime = new java.util.Date()
    println(s"${Thread.currentThread.getName}: $dateTime : $msg")
  }

  // Cache operations.
  def clear[K, V] (cacheMap : TMap[K, V]) : Unit = atomic {
    implicit txn => 
      cacheMap.empty
      Txn.afterCommit {
        x => log("Clearing contents for map " + x)
      } 
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
        val clMap = clear(cacheMap)
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

  import CommandAndControl._
  def loadPeriodically[K, V] (spl : String => Either[String, (K, V)]) (interval : Int) 
      (aFileName : FileName) (cacheMap : TMap[K,V]): Unit =  
        refresh(interval)(spl)(aFileName)(cacheMap) 
}


