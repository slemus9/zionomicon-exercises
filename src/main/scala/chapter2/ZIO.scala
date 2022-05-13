package chapter2

import scala.util.Try

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

  def flatMap [R1 <: R, E1 >: E, B] (
    f: A => ZIO[R1, E1, B]
  ): ZIO[R1, E1, B] = ZIO(
    r => self.run(r).fold(ZIO.fail(_), f).run(r)
  )
}

object ZIO {

  def attempt [A] (a: => A): ZIO[Any, Throwable, A] = ZIO(
    _ => Try(a).toEither
  )

  def fail [E] (e: => E): ZIO[Any, E, Nothing] = ZIO(
    _ => Left(e)
  )
}