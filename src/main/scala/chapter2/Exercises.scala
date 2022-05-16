package chapter2

import zio._
import scala.io.Source

object Exercises {

  /*
    1. Implement a ZIO version of the function readFile by using the ZIO.attempt constructor.
  */
  val readFile: String => String = { file => 

    val source = Source.fromFile(file)
    try source.getLines.mkString finally source.close()
  }

  val readFileZIO: String => Task[String] = file => 
    ZIO { readFile(file) }

  /*
    2. Implement a ZIO version of the function writeFile by using the ZIO.attempt constructor.
  */
  val writeFile: String => String => Unit = { file => text => 
    import java.io._

    val pw = new PrintWriter(new File(file))
    try pw.write(text) finally pw.close()
  }

  val writeFileZIO: String => String => Task[Unit] = file => text => 
    ZIO { writeFile (file) (text) }

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

}