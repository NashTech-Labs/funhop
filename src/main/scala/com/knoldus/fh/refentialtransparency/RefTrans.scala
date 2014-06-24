package com.knoldus.fh.refentialtransparency

import com.knoldus.fh.start.Money

object RefTrans extends App {
  // val money1 = new Money(10)
  val money1 = new GoodMoney(10)
  val money2 = money1.add(20)

  //val sum1 = money2.add(10)
  val sum1 = money1.add(20).add(10)
  println(s"Your sum1 is $sum1")
  //val sum2 = money2.add(10)
  val sum2 = money1.add(20).add(10)
  println(s"Your sum2 is $sum2")
}

case class GoodMoney(val amt: Int) {
  def add(anotherAmt: Int): GoodMoney = GoodMoney(this.amt + anotherAmt)
}