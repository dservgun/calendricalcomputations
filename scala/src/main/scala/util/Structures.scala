package util.Structures
import java.util.concurrent.{ExecutorService, Future, Callable, TimeUnit, Executors}
import util.Actor._

object Par {

  type Par[A] = ExecutorService => Future[A]
  def unit[A] (a : A) : Par[A] = (es : ExecutorService) => UnitFuture(a)
  def lazyUnit[A] (a : => A) : Par[A] = fork(unit(a))
  def run[A] (es : ExecutorService)(a : Par[A]) : Future[A] = a(es)
  def fork[A] (a: => Par[A]) : Par[A] = 
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  private case class UnitFuture[A] (get : A) extends Future[A] {
    println("Inside future...")
    def isDone = true
    def get(timeout : Long, units : TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning : Boolean) : Boolean = false
  }
  def map2[A, B, C](a : Par[A], b : Par[B]) (f: (A, B) => C) : Par[C] = 
    (es : ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      val afV = af.get 
      val bfV = bf.get
      println(s"$afV : $bfV")
      UnitFuture(f(afV, bfV))
    }
  def asyncF[A, B](f : A => B) : A => Par[B] = {
    a => fork(unit(f(a)))
  }
}

object SomeFunctions {
  import Par._
  def sum(ints : IndexedSeq[Int]) : Par[Int] = 
    if (ints.size <= 1) 
        unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2) 
      Par.map2[Int, Int, Int] (Par.fork(sum(l)), Par.fork(sum(r))) ((a : Int, b : Int) => a + b)
    }
}



object StructuresMain extends scala.App {
  import SomeFunctions._
  val es : ExecutorService = Executors.newFixedThreadPool(40)
  try {
  val pSum = Par.run(es)(sum(IndexedSeq.range(0, 10)))
  println(pSum.get)
  }finally {
    es.shutdown()  
  }
  
}