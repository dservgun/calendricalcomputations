package scala.util;

trait Monoid[A] {
  def op(a : A, b : A) : A 
  def zero : A
}

object MonoidInstances {
val stringMonoid = new Monoid[String] {
  def op(a1 : String, a2 : String) = a1 + a2
  val zero = "";
}


val intAddition = new Monoid[Int] {
    def op(a1 : Int, a2 : Int) = a1 + a2
    val zero = 0
}
val intMultiplication = new Monoid[Int] {
    def op(a1 : Int, a2 : Int) = a1 * a2 
    val zero = 1 
}
}

class GenericMonoids[A] {
  def listMonoid = new Monoid[List[A]] {
    def op(a1 : List[A] , a2 : List[A]) = a1 ++ a2
    val zero = Nil
  }
  def concatenate[A](as : List[A], m : Monoid[A]) : A = as.foldLeft (m.zero) (m.op)
  def foldMap[A, B] (as : List[A], m : Monoid[B]) (f : A => B) : B = {
    val n = as.map (x => f(x))
    concatenate(n, m)
  }
}


