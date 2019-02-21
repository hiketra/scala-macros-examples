import ACTest.SecurityContext

import scala.util.Try

object Main extends App{
  Test.testMethod
  Test.methodWithArguments(1.0, 5.0)

  Dog("Bobby").sayHello

  try {
    println("Attempting to make a payment of 2000 as Admin")

  }

  println("Attempting to make 2000 payment with admin role" + s"New balance ${ACTest.makePayment(2000, 123)(SecurityContext("1001", "Admin"))}")
  try {
    println("Attempting to make 5000 payment with advisor role...")
    ACTest.makePayment(5000, 123)(SecurityContext("1001", "Advisor"))
  } catch {
    case e: Exception => println("Exception thrown!" + e.getMessage)
  }

  println("The final result")
}

