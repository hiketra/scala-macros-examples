object ACTest extends App {


  case class SecurityContext(customerId: String, role: String)

  var bank = 20000

  @AccessControl("Admin")
  //todo: figure out a way where we don't force the naming of parameters
  def makePayment(param1: Int, ignoredParameter: Int)(implicit context: SecurityContext): Int = {
    bank = bank - param1
    bank
  }

}
