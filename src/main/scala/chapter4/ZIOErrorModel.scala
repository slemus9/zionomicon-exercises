package chapter4


/*
  Make the distinction between potentially recoverable `errors`
  and `defects`
*/
object ZIOErrorModel {

  // ZIO[R, E, A] can succeed with A or potentially fail with Cause[E]
  sealed trait Cause [+E]

  object Cause {

    // Defects (unpredictable and unrecoverable)
    final case class Die (t: Throwable) extends Cause[Nothing]
    // Errors
    final case class Fail [+E] (e: E) extends Cause[E]

    // Multiple errors
    
    // Two causes of failure that occur concurrently
    final case class Both [+E] (
      left: Cause[E], right: Cause[E]
    ) extends Cause[E]

    // Two causes of failure that occur sequentially
    final case class Then [+E] (
      left: Cause[E], right: Cause[E]
    ) extends Cause[E]

  }


  // Ways an effect can finish its execution
  sealed trait Exit [+E, +A]

  object Exit {

    final case class Success [+A] (value: A) extends Exit[Nothing, A]
    final case class Failure [+E] (cause: E) extends Exit[E, Nothing]
  }

  final case class ZIO [-R, +E, +A] (
    run: R => Either[Cause[E], A]
  ) { self => 

    /*
      Expose the full cause of an effect failure
    */
    def sandbox: ZIO[R, Cause[E], A] = ???
    
    /*
      Opposite of sandbox
    */
    def unsandbox [E1] (implicit ev: E <:< Cause[E1]): ZIO[R, E1, A] = ???

    def foldCauseZIO [R1 <: R, E1, B] (
      onFailure: Cause[E] => ZIO[R1, E1, B],
      onSuccess: A => ZIO[R1, E1, B]
    ): ZIO[R1, E1, B] = ZIO(
      r => self.run(r).fold(onFailure, onSuccess).run(r)
    )

    /*
      Transform errors to defects.
      From an effect that can fail with any subtype of Throwable, and returns
      an effect that fails with a defect (Die = Cause[Nothing]) if the original fails

      ZIO.refineWith can be used to transform only a subset of errors to defects, by
      supplying partial functions. Every error type that is not matched with the partial functions
      will be converted to a defect.
    */
    def orDie (implicit ev: E <:< Throwable): ZIO[R, Nothing, A] = ???

    /*
      Fallback to the given 
    */
    def orElse [R1 <: R, E2, A1 >: A] (
      that: ZIO[R1, E2, A1]
    ): ZIO[R1, E2, A1] = ???
  }
}

object StackedErrors {
  import zio._

  trait DatabaseError
  trait UserProfile

  /*
    In this example, the profile is optional within 
    the database, so it's reasonable to type this function
    with Option[UserProfile] on the success channel
  */
  def lookupProfile (
    userId: String
  ): ZIO[Any, DatabaseError, Option[UserProfile]] = ???

  /*
    An alternative to this is to shift all failures to
    the error channel
  */
  def lookupProfile2 (
    userId: String
  ): ZIO[Any, Option[DatabaseError], UserProfile] = 
    lookupProfile(userId).foldZIO(
      error => ZIO.fail(Some(error)),
      ZIO.fromOption(_)
    )

  def lookupProfile3 (
    userId: String
  ): ZIO[Any, Option[DatabaseError], UserProfile] =
    lookupProfile(userId).some
}