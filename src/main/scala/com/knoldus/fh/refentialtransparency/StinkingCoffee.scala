package com.knoldus.fh.refentialtransparency

class Cafe {
  def buyCoffee(name: String, cc: CreditCard): Coffee = {
    cc.chargeCard
    println("Enjoy your coffee !"); Coffee("cuppacino")
  }

  def listIngredients(coffee: Coffee) = {
    println(s"This delightful ${coffee.name} coffee is made with ${coffee.shots}" +
      " shots of coffee, ${coffee.milk} units of milk")
  }
}

case class Coffee(name: String) {
  val milk: Int = 1
  val shots: Int = 1
  val cream: Int = 1
}
case class CreditCard() {
  def chargeCard = { println("Your credit card is charged") }
}

object LetsHaveCoffee extends App {
  val timsCafe = new Cafe
  timsCafe.buyCoffee("Cappacino", CreditCard())
  timsCafe.listIngredients(Coffee("expresso"))

  //Referential Transparency & Substitution model
  timsCafe.listIngredients(timsCafe.buyCoffee("Cappacino", CreditCard()))

}

