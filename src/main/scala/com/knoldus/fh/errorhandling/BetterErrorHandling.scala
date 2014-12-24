package com.knoldus.fh.errorhandling

import scala.util.Try

object BetterErrorHandling {

  def fetchEmployeeName(id: Int) = {
    try {
      if (id > 0) "Jack" else throw new Exception
    } catch {
      case ex: Exception => "John"
    }
  }

  // Exceptions are not type-safe
  def divide(divident: Int, divisor: Int): Int = divident / divisor
 
  def divideWithError(divident: Int, divisor: Int): Either[Exception, Int] = {
    try {
      Right(divide(divident, divisor))
    } catch { case ex: Exception => Left(ex) }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    try {
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)
    } catch { case ex: Exception => None }

}
object BetterErrorHandlingApp extends App {
  import BetterErrorHandling._

  println(fetchEmployeeName(-1))
  println(divide(10, 1))
  val k = lift(math.abs)
  println(k(Some(-10)))
  val liftedDivide = map2(Some(10), Some(0))(divide)
  println("ff " + liftedDivide)
 
  println(divideWithError(10,1).fold(a=>a,b=>b+1))
  
  //println(divide(10, 0))
}