package chapter2

import zio._
import zio.ZIOAppDefault
import chapter2.{ ZIO => ZIOToy }
import scala.concurrent.{ ExecutionContext, Future }
import scala.io.Source
import java.io.IOException

object Exercises {

  /*
    1. Implement a ZIO version of the function readFile by using the ZIO.attempt constructor.
  */
  val readFile: String => String = { file => 

    val source = Source.fromFile(file)
    try source.getLines.mkString finally source.close()
  }

  val readFileZIO: String => Task[String] = file => 
    ZIO.attempt { readFile(file) }

  /*
    2. Implement a ZIO version of the function writeFile by using the ZIO.attempt constructor.
  */
  val writeFile: String => String => Unit = { file => text => 
    import java.io._

    val pw = new PrintWriter(new File(file))
    try pw.write(text) finally pw.close()
  }

  val writeFileZIO: String => String => Task[Unit] = file => text => 
    ZIO.attempt { writeFile (file) (text) }

  /* 
    3.  Using the flatMap method of ZIO effects, together with the readFileZio
        and writeFileZio functions that you wrote, implement a ZIO version of
        the function copyFile.
  */
  val copyFile: String => String => Unit = { source => dest => 
    
    val contents = readFile(source)
    writeFile (dest) (contents)
  }

  val copyFileZIO: String => String => Task[Unit] = source => dest => 
    readFileZIO(source).flatMap(writeFileZIO(dest))
  

  /*
    4. Rewrite the following ZIO code that uses flatMap into a for comprehension.

    def printLine(line: String) = ZIO.attempt(println(line))
    val readLine = ZIO.attempt(scala.io.StdIn.readLine())

    printLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
      printLine(s"Hello, ${name}!")))
  */
  def printLine (line: String) = ZIO.attempt(println(line))
  val readLine = ZIO.attempt(scala.io.StdIn.readLine())
  
  val whatIsYourName = for { 
    name  <- readLine
    _     <- printLine(s"Hello, ${name}!")
  } yield ()

  /*
    5. Rewrite the following ZIO code that uses flatMap into a for comprehension.

    val random = ZIO(scala.util.Random.nextInt(3) + 1)
    def printLine(line: String) = ZIO(println(line))
    val readLine = ZIO(scala.io.StdIn.readLine())

    random.flatMap { int =>
      printLine("Guess a number from 1 to 3:").flatMap { _ =>
        readLine.flatMap { num =>
          if (num == int.toString) printLine("You guessed right!")
          else printLine(s"You guessed wrong, the number was $int!")
        }
      }
    }

  */
  val random = ZIO.attempt(scala.util.Random.nextInt(3) + 1)

  val guessANumber = for {
    n     <- random
    _     <- printLine("Guess a number from 1 to 3:")
    guess <- readLine
    _     <- printLine(
      if (guess == n.toString) "You guessed right!"
      else s"You guessed wrong, the number was $n!"
    ) 
  } yield ()

  /*
    6.  Implement the zipWith function in terms of the toy model of a ZIO
        effect. The function should return an effect that sequentially composes
        the specified effects, merging their results with the specified user-defined
        function.
  */
  def zipWith [R, E, A, B, C] (
    self: ZIOToy[R, E, A],
    that: ZIOToy[R, E, B]
  ) (
    f: (A, B) => C
  ): ZIOToy[R, E, C] = for {
    a <- self
    b <- that
  } yield f(a, b)

  /*
    7.  Implement the collectAll function in terms of the toy model of a ZIO
        effect. The function should return an effect that sequentially collects the
        results of the specified collection of effects.
  */
  def traverse [R, E, A, B] (f: A => ZIOToy[R, E, B]) (
    in: Iterable[A]
  ): ZIOToy[R, E, List[B]] = {

    val empty: ZIOToy[R, E, List[B]] = ZIOToy.succeed(Nil)
    in.foldRight(empty) { (a, rest) => 
      zipWith (f(a), rest) { _ :: _ }
    }
  }

  def collectAll [R, E, A, B] = traverse (identity[ZIOToy[R, E, A]](_)) (_)

  /*
    8.  Implement the foreach function in terms of the toy model of a ZIO effect.
        The function should return an effect that sequentially runs the specified
        function on every element of the specified collection.
  */
  def foreach [R, E, A, B] (
    in: Iterable[A]
  ) (f: A => ZIOToy[R, E, B]): ZIOToy[R, E, List[B]] = traverse(f)(in)

  /*
    9.  Implement the orElse function in terms of the toy model of a ZIO effect.
        The function should return an effect that tries the left hand side, but if
        that effect fails, it will fallback to the effect on the right hand side.
  */
  def orElse[R, E1, E2, A](
    self: ZIOToy[R, E1, A],
    that: ZIOToy[R, E2, A]
  ): ZIOToy[R, E2, A] = self.foldZIO(
    _ => that,
    ZIOToy.succeed(_)
  )

  /*
    11. Using ZIO.fail and ZIO.succeed, implement the following function,
        which converts an Either into a ZIO effect:
  */
  def eitherToZIO [E, A] (either: Either[E, A]): ZIO[Any, E, A] =
    either.fold(ZIO.fail(_), ZIO.succeed(_))

  /*
    12. Using ZIO.fail and ZIO.succeed, implement the following function,
        which converts a List into a ZIO effect, by looking at the head element in
        the list and ignoring the rest of the elements.
  */
  def listToZIO [A] (list: List[A]): ZIO[Any, None.type, A] = 
    list match {
      case Nil => ZIO.fail(None)
      case x :: _ => ZIO.succeed(x)
    }

  /*
    13. Using ZIO.succeed, convert the following procedural function into a ZIO
        function:
  */
  def currentTime (): Long = java.lang.System.currentTimeMillis()

  lazy val currentTimeZIO: ZIO[Any, Nothing, Long] = 
    ZIO.succeed(currentTime())

  /*
    14. Using ZIO.async, convert the following asynchronous, callback-based 
        function into a ZIO function:
  */
  def getCacheValue(
    key: String,
    onSuccess: String => Unit,
    onFailure: Throwable => Unit
  ): Unit = ???

  def getCacheValueZIO (key: String): ZIO[Any, Throwable, String] =
    ZIO.async { callback => 
      getCacheValue(
        key,
        callback.compose(ZIO.succeed(_)),
        callback.compose(ZIO.fail(_))
      )
    }

  /*
    15. Using ZIO.async, convert the following asynchronous, callback-based 
        function into a ZIO function:
  */
  trait User

  def saveUserRecord(
    user: User,
    onSuccess: () => Unit,
    onFailure: Throwable => Unit
  ): Unit = ???

  def saveUserRecordZIO (user: User): ZIO[Any, Throwable, Unit] =
    ZIO.async { callback => 
      saveUserRecord(
        user,
        () => callback(ZIO.unit),
        callback.compose(ZIO.fail(_))
      )
    }

  /*
    16. Using ZIO.fromFuture, convert the following code to ZIO
  */
  trait Query
  trait Result

  def doQuery(query: Query) (
    implicit ec: ExecutionContext
  ): Future[Result] = ???


  def doQueryZIO (query: Query): ZIO[Any, Throwable, Result] =
    ZIO.fromFuture(implicit ec => doQuery(query))

  /*
    19. Using the Console service and recursion, write a function that will repeat-
        edly read input from the console until the specified user-defined function
        evaluates to true on the input.
  */
  def readUntil (
    acceptInput: String => Boolean
  ): ZIO[Console, IOException, String] =
    doWhile(Console.readLine)(acceptInput)

  /*
    20. Using recursion, write a function that will continue evaluating the specified
        effect, until the specified user-defined function evaluates to true on the
        output of the effect.
  */
  def doWhile [R, E, A] (
    body: ZIO[R, E, A]
  ) (condition: A => Boolean): ZIO[R, E, A] =
    body.flatMap { a => 
      if (condition(a)) ZIO.succeed(a)
      else doWhile(body)(condition)  
    }
}

/*
  10. Using the following code as a foundation, write a ZIO application that
      prints out the contents of whatever files are passed into the program as
      command-line arguments. You should use the function readFileZio that
      you developed in these exercises, as well as ZIO.foreach.
*/
object Cat extends ZIOAppDefault {
  import Exercises.readFileZIO

  def run = {

    val readAndPrint = { file: String => 
      readFileZIO(file).flatMap(Console.printLine(_))
    }

    getArgs.flatMap(
      ZIO.foreach(_)(readAndPrint)
    ).exitCode
  }
}

/*
  17. Using the Console, write a little program that asks the user what their
      name is, and then prints it out to them with a greeting.
*/
object HelloHuman extends ZIOAppDefault {

  def run = {
    
    val greet = for {
      _     <- Console.printLine("Input your name:")
      name  <- Console.readLine
      _     <- Console.printLine(s"Hello $name!")
    } yield ()

    greet.exitCode
  }

}

/*
  18. Using the Console and Random services in ZIO, write a little program that
      asks the user to guess a randomly chosen number between 1 and 3, and
      prints out if they were correct or not.
*/
object NumberGuessing extends ZIOAppDefault {
  import Exercises.doWhile

  def run = {

    val random1to3 = Random.nextIntBetween(1, 4)
    val askGuess = for {
      n     <- random1to3
      _     <- Console.printLine("Guess a number from 1 to 3:")
      guess <- Console.readLine
      isCorrect = n.toString == guess
      _     <- Console.printLine(
        if (isCorrect) "You guessed right!"
        else s"You guessed wrong, the number was $n!"
      ) 
    } yield isCorrect

    doWhile(askGuess)(identity(_)).exitCode
  }
}