package chapter3

import zio._
import zio.test._
import zio.test.Assertion._

object ExampleSpec extends ZIOSpecDefault {

  /*
    A collection of tests is represented by a `spec`, 
    which can be a `test` or a `suite` of tests
  */
  val testAdditionWorks = test ("Addition works") {
    assert (1 + 1) (equalTo(2))
  }

  val testZioSucceed = test ("ZIO.succeed succeeds with a value") { 
    assertZIO (ZIO.succeed(1 + 1)) (equalTo(2))
  }

  val testAssertionConjunction = test ("Assertion conjunction") {
    for
      x <- ZIO.succeed(1)
      y <- ZIO.succeed(2)
    yield assert (x) (equalTo(1)) && assert (y) (equalTo(2))
  }

  val testHasSameElements = test ("Has same elements") {
    assert (List(1, 1, 2, 3)) (hasSameElements(List(3, 1, 2, 1)))
  }

  val testFails = test ("Fails") { 
    val div = ZIO.attempt(1 / 0).catchAll(_ => ZIO.fail(())).exit
    assertZIO (div) (fails(isUnit))
  }

  /*
    Assertions can be combined with &&, || and not
  */
  val nonEmptyAndNonNegative: Assertion[Iterable[Int]] = 
    isNonEmpty && forall(nonNegative)

  val emptyOrHas3Elements = isEmpty || hasSize(equalTo(3))

  val notDistinct = not(isDistinct)

  def spec = suite ("ExampleSpec") (
    testAdditionWorks,
    testZioSucceed,
    testAssertionConjunction,
    testHasSameElements,
    testFails
  )
}

/*
  To test services we can provide alternate implementations that
  have the same interface. For example, to test an effect that uses Console, we can
  provide TestConsole
*/
object TestGreet extends ZIOSpecDefault {


  import zio.Console._
  import zio.test.TestEnvironment

  val greet: ZIO[Console, Nothing, Unit] = 
    for
      name  <- readLine.orDie
      _     <- printLine(s"Hello $name").orDie
    yield ()

  val nameToTest = "Sebastian"

  def testGreet = 
    for
      _     <- TestConsole.feedLines(nameToTest)
      _     <- greet
      value <- TestConsole.output
    yield assert (value) (equalTo(Vector(s"Hello, $nameToTest!\n")))

  def spec: Spec[TestEnvironment with Scope, Any] = suite ("Test greet") {
    test ("Hello") (assert (1 + 1) (equalTo(2)))
    // test ("Test greet") (testGreet) does not work. Fix later
  }

}