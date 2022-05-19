package other

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext
import scala.annotation.tailrec

/*
  Example on how to wrap effects.

  Personal basic implementation of an IO monad with stack safety on recursion, 
  based on the one found in cats-effect.
*/
sealed trait IO [+A] { self => 

  def map [B] (f: A => B): IO[B]

  def flatMap [B] (f: A => IO[B]): IO[B]

  def *> [B] (another: IO[B]): IO[B] = flatMap { _ => another }

  def as [B] (newValue: => B): IO[B] = map { _ => newValue } 

  def void: IO[Unit] = as { () } 

  def attempt: IO[Either[Throwable, A]]

  def option: IO[Option[A]] = attempt.map { _.toOption }

  def handleErrorWith [AA >: A] (f: Throwable => IO[AA]): IO[AA] = 
    attempt.flatMap { _.fold(f, IO.pure) }

  def redeem [B] (recover: Throwable => B, map: A => B): IO[B] = 
    attempt.map { _.fold(recover, map) }

  def redeemWith [B] (recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = 
    attempt.flatMap { _.fold(recover, bind) }

  def unsafeRunSync (): A = IO.runTrampolined(self)

  def unsafeToFuture () (implicit ec: ExecutionContext): Future[A] = 
    Future { unsafeRunSync }
}

private final case class IOResult [+A] (val value: A) extends IO[A] {

  override def map [B] (f: A => B): IO[B] = IO { f(value) }

  override def flatMap [B] (f: A => IO[B]): IO[B] = IO.suspend { f(value) } 

  override def attempt: IO[Either[Throwable,A]] = IO.pure(Right(value))
}

private final case class IOExec [+A] (val run: () => A) extends IO[A] {

  override def map [B] (f: A => B): IO[B] = IO { f(run()) }

  override def flatMap [B] (f: A => IO[B]): IO[B] = IO.suspend { f(run()) } 

  override def attempt: IO[Either[Throwable,A]] = IO { Try(run()).toEither }
}

private final case class IOSuspend [+A] (val run: () => IO[A]) extends IO[A] {

  override def map [B] (f: A => B): IO[B] = IO.suspend { run().map(f) }

  override def flatMap [B] (f: A => IO[B]): IO[B] = IO.suspend { run().flatMap(f) }

  override def attempt: IO[Either[Throwable,A]] = IO.suspend {
    Try(run()).toEither.fold(
      e   => IO(Left(e)),
      io  => io.attempt
    )
  }
}

object IO {

  def apply [A] (body: => A): IO[A] = IOExec(() => body)

  def suspend [A] (thunk: => IO[A]): IO[A] = IOSuspend(() => thunk)

  def delay [A] (body: => A): IO[A] = IO(body)

  def pure [A] (a: A): IO[A] = IOResult(a)

  def fromEither [A] (e: Either[Throwable, A]): IO[A] = 
    e.fold(IO.raiseError, IO.pure)

  def fromOption [A] (option: Option[A])(orElse: => Throwable): IO[A] = 
    option.fold (IO.raiseError[A](orElse)) (IO.pure)

  def fromTry [A] (t: Try[A]): IO[A] = 
    fromEither(t.toEither)

  def none [A]: IO[Option[A]] = IO.pure(None)

  def raiseError [A] (e: Throwable): IO[A] = IO(throw e)

  def raiseUnless (cond: Boolean) (e: => Throwable): IO[Unit] = 
    unlessA (cond) (raiseError(e))

  def raiseWhen (cond: Boolean) (e: => Throwable): IO[Unit] = 
    whenA (cond) (raiseError(e))

  def unlessA (cond: Boolean) (action: => IO[Unit]): IO[Unit] = 
    if (!cond) action else IO.unit

  def whenA (cond: Boolean) (action: => IO[Unit]): IO[Unit] = 
    if (cond) action else IO.unit

  val unit: IO[Unit] = IO.pure(())

  @tailrec
  private def runTrampolined [A] (io: IO[A]): A = io match {
    case IOResult(v)    => v
    case IOExec(run)    => run() 
    case IOSuspend(run) => runTrampolined(run())
  }
}


object IOExamples extends App {

  def fib (n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if (n > 0) 
        fib(n - 1, b, b2)
      else 
        IO.pure(a)
  }

  println(fib(100000).unsafeRunSync())
  println(
    (IO.pure(42) *> IO(throw new RuntimeException("error"))).attempt.unsafeRunSync()
  )
}