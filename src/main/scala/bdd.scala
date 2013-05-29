/**
 * Created by : Rockie Yang(eyouyan@gmail.com, snowriver.org)
 * Created at : 5/28/13
 */
object bdd extends App {

  example ()

  def example() {
    TestCase ("normal user management flow",

      "create user"
      send "createUser(name='rockie', password='I hate always change password', phone='888888')"
      expect "successful",

      "check user exist"
      send "getUser(name='rockie')"
      expect "true"
      expect ".phone='88888'",

      "delete user"
      send "deleteUser(name='rockie')"
      expect "true"
    ) to "localhost" execute("now")

  }

  implicit def stringToTestStep(stepName: String): TestStep = new TestStep (stepName)


  class TestStep(stepName: String) {
    var request: Option[String] = None
    var expects: List[String] = Nil

    def send(request: String) = {
      this.request = Some (request)
      this
    }

    def expect(condition: String) = {
      expects = condition :: expects
      this
    }

    override def toString = {
      val expectsString = "expect: " + expects.mkString("\nexpect: ")
      val requestString = request.get
      val result = f"test step: $stepName%s\nrequest: $requestString%s\n$expectsString%s"
      result
    }
  }

  class TestCase(description: String, steps: List[TestStep]) {
    var tos: List[String] = Nil

    def to(url: String): TestCase = {
      tos = url :: tos
      this
    }

    def execute(time: String): TestCase = {
      println (toString)
      this
    }

    override def toString: String = {
      val result = description + "\n" + steps.mkString("\n\n")
      result
    }
  }

  object TestCase {

    def apply(description: String,
              steps: TestStep*) = {
      new TestCase (description, steps.toList)
    }
  }

}