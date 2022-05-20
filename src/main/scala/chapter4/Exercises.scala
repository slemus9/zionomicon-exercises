package chapter4

import zio._
import java.io.IOException

object Exercises {

  /*
    1.  Using the appropriate effect constructor, fix the following function so that
        it no longer fails with defects when executed. Make a note of how the
        inferred return type for the function changes.

    def failWithMessage (string: String) = ZIO.succeed(throw new Error(string))
  */

  // changes from ZIO[Any,Nothing,Nothing] to IO[Error,Nothing]
  def failWithMessage (string: String) = ZIO.fail(new Error(string))


  /*
    2.  Using the ZIO#foldCauseZIO operator and the Cause#defects method,
        implement the following function. This function should take the effect,
        inspect defects, and if a suitable defect is found, it should recover from
        the error with the help of the specified function, which generates a new
        success value for such a defect.
  */
  // Return type  ZIO[R, E, A] does not make sense for this function.
  // Changing it to ZIO[R, E, List[A]]
  def recoverFromSomeDefects [R, E, A] (zio: ZIO[R, E, A]) (
    f: Throwable => Option[A]
  ): ZIO[R, E, List[A]] = zio.foldCauseZIO(
    failure => {
      val successes = failure.defects.collect { t => f(t) match {
        case Some(a) => a
      } }
      if successes.size > 0 then ZIO.succeed(successes)
      else ZIO.failCause(failure)
    },
    success => ZIO.succeed(List(success))
  )

  /*
    3.  Using the ZIO#foldCauseZIO operator and the Cause#prettyPrint
        method, implement an operator that takes an effect, and returns a new
        effect that logs any failures of the original effect (including errors and
        defects), without changing its failure or success value.
  */
  // cannot avoid changing the error type, since Console.printLine fails with 
  // IOException
  def logFailures [R, E, A] (zio: ZIO[R, E, A]): ZIO[R, E | IOException, A] =
    zio.foldCauseZIO(
      cause => {
        val pretty = cause.prettyPrint
        Console.printLine(pretty) *> ZIO.failCause(cause)
      },
      ZIO.succeed(_)
    )

  /*
    4.  Using the ZIO#foldCauseZIO method, which “runs” an effect to an Exit
        value, implement the following function, which will execute the specified
        effect on any failure at all:
  */
  // could not implement it with handler: ZIO[R, E, Any].
  // Changed it to handler: ZIO[R, E, A]
  def onAnyFailure [R, E, A] (
    zio: ZIO[R, E, A],
    handler: ZIO[R, E, A]
  ): ZIO[R, E, A] =
    zio.orElse(handler)

  /*
    5.  Using the ZIO#refineOrDie method, implement the ioException func-
        tion, which refines the error channel to only include the IOException
        error.
  */
  def ioException[R, A](
    zio: ZIO[R, Throwable, A]
  ): ZIO[R, IOException, A] =
    zio.refineOrDie { 
      case e: IOException => e
    }

  /*
    6.  Using the ZIO#refineToOrDie method, narrow the error type of the fol-
        lowing effect to just NumberFormatException.

        val parseNumber: ZIO[Any, Throwable, Int] =
          ZIO.attempt("foo".toInt)
  */
  val parseNumber: ZIO[Any, NumberFormatException, Int] =
    ZIO.attempt("foo".toInt).refineToOrDie[NumberFormatException]

  /*
    7.  Using the ZIO#foldZIO method, implement the following two functions,
        which make working with Either values easier, by shifting the unexpected
        case into the error channel (and reversing this shifting).
  */
  // Changes ZIO[R, E, Either[A, B]] to ZIO[R, E, Either[B, A]]
  def left[R, E, A, B](
    zio: ZIO[R, E, Either[B, A]]
  ): ZIO[R, Either[E, B], A] =
    zio.foldZIO(
      error   => ZIO.fail(Left(error)),
      either  => either match {
        case Left(b) => ZIO.fail(Right(b))
        case Right(a) => ZIO.succeed(a)
      }
    )

  def unleft[R, E, A, B](
    zio: ZIO[R, Either[E, B], A]
  ): ZIO[R, E, Either[B, A]] =
    zio.foldZIO(
      errors => errors.fold(
        ZIO.fail(_),
        b => ZIO.succeed(Left(b))
      ),
      success => ZIO.succeed(Right(success))
    )

  
}