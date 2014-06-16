package com.knoldus.fh.start

object StepOne extends App {

  val money1 = new Money(10)
  showMeTheMoney(money1)
  showMeTheMoney(money1.add(0))

  
  private def showMeTheMoney(m:Money)= println(s"Your money is $m")
  
}

case class Money(var amt: Int) {
  def add(anotherAmt: Int): Money = {
    this.amt += anotherAmt
    updateBankRecords
    this
  }

  def updateBankRecords = println("Connect to Bank & Update records")
}