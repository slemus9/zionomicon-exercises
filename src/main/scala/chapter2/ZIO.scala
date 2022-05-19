package chapter2

import scala.util.Try
import scala.concurrent.ExecutionContext
import zio.Duration
import scala.concurrent.Future
import java.io.IOException

/**
  * Toy model of the ZIO data type
  * 
  * R: Type of the resource necessary to run the effect
  * 
  * E: Type of the error that may occur when running the effect
  * 
  * A: Type of the return value of the effect
  */
final case class ZIO [-R, +E, +A] (
  run: R => Either[E, A]
) { self => 

  def map [B] (f: A => B): ZIO[R, E, B] = ZIO(
    r => self.run(r).map(f)
  )

  /* 
    Sequencing effects.

    Run this effect, and the use its result to
    run the next effect. If the first effect fails
    with Left, the operation immediately fails
  */
  def flatMap [R1 <: R, E1 >: E, B] (
    f: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO(
    r => self.run(r).fold(ZIO.fail(_), f).run(r)
  )

  def >>= [R1 <: R, E1 >: E, B] = flatMap[R1, E1, B](_)

  def >> [R1 <: R, E1 >: E, B] (effect: ZIO[R1, E1, B]) = flatMap(_ => effect)

  /*
    Error Handling.

    Recover from error with another effect
  */
  def foldZIO [R1 <: R, E1, B] (
    onFailure: E => ZIO[R1, E1, B],
    onSuccess: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO(
    r => self.run(r).fold(onFailure, onSuccess).run(r)
  )

  /*
    We specify an effect that can never fail with E = Nothing 
    (Either[Nothing, A] can only be Right[A], since there is no
    value that can be of type Nothing)
  */
  def fold [B] (
    onFailure: E => B,
    onSuccess: A => B
  ): ZIO[R, Nothing, B] = ZIO(
    r => Right(self.run(r).fold(onFailure, onSuccess))
  )

  /*
    The Environment.

    An effect that does not need a particular environment to run is modeled with
    R = Any. We can access and environment or provide and environment
  */
  def provide (r: R): ZIO[Any, E, A] = ZIO(
    _ => self.run(r)
  )
}

object ZIO {

  // Type aliases
  // Does not require a particular resource
  type IO[+E, +A] = ZIO[Any, E, A] 
  // Does not require a particular environment and may fail with Throwable
  type Task[+A] = ZIO[Any, Throwable, A] 
  // Requires an Environment R and may fail with Throwable
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  // Does not require a particular environment and does not fail
  type UIO[+A] = ZIO[Any, Nothing, A]
  // Requires an Environment R and does not fail
  type URIO[-R, +A] = ZIO[R, Nothing, A]


  // Constructors
  /*
    Useful for transforming traditional synchronous side-effectful code that may throw 
    an exception
  */
  def attempt [A] (a: => A): Task[A] = ZIO(
    _ => Try(a).toEither
  )

  def fail [E] (e: => E): ZIO[Any, E, Nothing] = ZIO(
    _ => Left(e)
  )

  def succeed [A] (a: => A): UIO[A] = ZIO(
    _ => Right(a)
  )

  def fromEither [E, A] (either: Either[E, A]): IO[E, A] = ZIO(
    _ => either
  )

  def fromOption [A] (option: Option[A]): IO[None.type, A] = ZIO(
    _ => option.toRight(None)
  )

  def fromTry [A] (a: Try[A]): Task[A] = fromEither(
    a.toEither
  )

  /*
    Creates an Effect with an environment as a success values.
    The environment then can be accessed through combinators
    such as map or flatmap
  */
  def environment [R]: ZIO[R, Nothing, R] = ZIO(
    Right(_)
  )

  // Converting async callbacks
  def async [R, E, A] (
    callback: (ZIO[R, E, A] => Unit) => Any
  ): ZIO[R, E, A] = ???

  // Example
  private def getUserByIdAsync (id: Int) (callback: Option[String] => Unit): Unit = ???

  private def getUserByIdZIO (id: Int): IO[None.type, String] = async { callback => 
    getUserByIdAsync(id) { callback.compose(fromOption) }
  }

  def fromFuture [A] (make: ExecutionContext => Future[A]): Task[A] = ???

  /*
    Clock is a service that provides functionality related to time 
    and scheduling
  */
  trait Clock

  object Clock {

    def nanoTime: URIO[Clock, Long] = ???
    def sleep (duration: => Duration): URIO[Clock, Unit] = ???
  }

  def delay [R, E, A] (effect: ZIO[R, E, A]) (
    duration: Duration
  ): ZIO[R with Clock, E, A] =
    Clock.sleep(duration) >> effect

  /*
    Console is a service related to reading and writing to console
  */
  trait Console

  object Console { 

    val readLine: ZIO[Console, IOException, String] = ???
    def putStr (line: => String): URIO[Console, Unit] = ???
    def printLine (line: => String): URIO[Console, Unit] = ???
  }

  /*
    System is a service that is used for getting system and environment
    variables
  */
  trait System

  object System {

    // Access the specified environment variable
    def env (variable: String): ZIO[System, SecurityException, Option[String]] = ???
    // Access the specified system property
    def property (prop: String): ZIO[System, Throwable, Option[String]] = ???
  }

  // ZIO effects are stack safe
}