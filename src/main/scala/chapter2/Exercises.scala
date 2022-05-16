package chapter2

import zio._
import zio.{ App => ZIOApp }
import scala.io.Source
import chapter2.{ZIO => ZIOToy }

object Exercises {

  /*
    1. Implement a ZIO version of the function readFile by using the ZIO.attempt constructor.
  */
  val readFile: String => String = { file => 

    val source = Source.fromFile(file)
    try source.getLines.mkString finally source.close()
  }

  val readFileZIO: String => Task[String] = file => 
    ZIO.effect { readFile(file) }

  /*
    2. Implement a ZIO version of the function writeFile by using the ZIO.attempt constructor.
  */
  val writeFile: String => String => Unit = { file => text => 
    import java.io._

    val pw = new PrintWriter(new File(file))
    try pw.write(text) finally pw.close()
  }

  val writeFileZIO: String => String => Task[Unit] = file => text => 
    ZIO.effect { writeFile (file) (text) }

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
    readFileZIO(source) >>= writeFileZIO(dest)
  

  /*
    4. Rewrite the following ZIO code that uses flatMap into a for comprehension.

    def printLine(line: String) = ZIO.attempt(println(line))
    val readLine = ZIO.attempt(scala.io.StdIn.readLine())

    printLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
      printLine(s"Hello, ${name}!")))
  */
  def printLine (line: String) = ZIO(println(line))
  val readLine = ZIO(scala.io.StdIn.readLine())
  
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
  val random = ZIO(scala.util.Random.nextInt(3) + 1)

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
}

/*
  10. Using the following code as a foundation, write a ZIO application that
      prints out the contents of whatever files are passed into the program as
      command-line arguments. You should use the function readFileZio that
      you developed in these exercises, as well as ZIO.foreach.
*/
object Cat extends ZIOApp {
  import Exercises._
  import zio.console.putStrLn

  def run (args: List[String]): URIO[ZEnv,ExitCode] = {

    val readAndPrint = { file: String => 
      readFileZIO(file).flatMap(putStrLn(_))
    }

    ZIO.foreach(args)(readAndPrint).exitCode
  }
}